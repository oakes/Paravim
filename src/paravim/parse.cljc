(ns paravim.parse
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [find])
  #?(:clj (:import [java.util.regex Pattern Matcher])))

(def group-names [:whitespace
                  :special-char
                  :delimiter
                  :string
                  :comment
                  :number
                  :symbol])

(def non-code-groups #{:whitespace :comment})

(def groups ["([\\s,]+)"                   ;; whitespace
             "(~@|['`~^@])"                ;; special-char
             "([\\[\\]{}()]|#\\{)"         ;; delimiter
             "(\"(?:\\\\.|[^\\\\\"])*\"?)" ;; string
             "(;.*)"                       ;; comment
             "(\\d+\\.?[a-zA-Z\\d]*)"      ;; number
             "([^\\s\\[\\]{}('\"`,;)]+)"]) ;; symbol

(def group-range (range 0 (count group-names)))

(def regex-str (str/join "|" groups))
(def regex (re-pattern regex-str))

(def open-delims #{"#{" "(" "[" "{"})
(def close-delims #{"}" ")" "]"})
(def delims {"#{" "}"
             "(" ")"
             "[" "]"
             "{" "}"})

(defprotocol IRegex
  (find [this])
  (group [this index])
  (index [this]))

(defn ->regex [match-str]
  (let [regex #?(:clj  regex
                 :cljs (js/RegExp. regex-str "g"))
        *matcher #?(:clj (.matcher ^Pattern regex match-str)
                    :cljs (volatile! (make-array 0)))]
    (reify IRegex
      (find [this]
        #?(:clj  (re-find *matcher)
           :cljs (when @*matcher (vreset! *matcher (.exec regex match-str)))))
      (group [this index]
        #?(:clj  (.group ^Matcher *matcher ^int index)
           :cljs (aget @*matcher index)))
      (index [this]
        #?(:clj  (.start ^Matcher *matcher)
           :cljs (.-index @*matcher))))))

(declare read-token)

(defn wrap-coll [data]
  (into [:collection] data))

(defn read-coll [matcher [_ delim :as token-data]]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if (find matcher)
        (let [[_ token :as token-data] (read-token matcher)
              data (conj data token-data)]
          (cond
            (= token end-delim)
            (wrap-coll data)
            (close-delims token)
            (vary-meta (wrap-coll data)
              assoc :error-message "Unmatched delimiter")
            :else
            (recur data)))
        (vary-meta (wrap-coll data)
          assoc :error-message "EOF while reading")))))

(defn read-token [matcher]
  (let [token (group matcher 0)
        group (get group-names
                (some #(when (group matcher (inc %)) %) group-range)
                :whitespace)]
    (if (and (= group :delimiter)
             (open-delims token))
      (read-coll matcher [group token])
      [(if (and (= group :symbol)
                (str/starts-with? token ":"))
         :keyword
         group)
      token])))

(defn parse [s]
  (let [matcher (->regex s)]
    (loop [data []]
      (if (find matcher)
        (recur (conj data (read-token matcher)))
        data))))

