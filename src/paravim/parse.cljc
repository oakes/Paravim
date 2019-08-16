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
  (group [this index]))

(defn ->regex [s]
  (let [regex #?(:clj  regex
                 :cljs (js/RegExp. regex-str "g"))
        matcher #?(:clj (.matcher ^Pattern regex s)
                   :cljs (atom (make-array 0)))]
    (reify IRegex
      (find [this]
        #?(:clj  (re-find matcher)
           :cljs (when @matcher (reset! matcher (.exec regex s)))))
      (group [this index]
        #?(:clj  (.group ^Matcher matcher ^int index)
           :cljs (aget @matcher index))))))

(declare read-token)

(defn read-coll [matcher [_ delim :as token-data]]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if (find matcher)
        (let [[_ token :as token-data] (read-token matcher)
              data (conj data token-data)]
          (cond
            (= token end-delim)
            (into [:collection] data)
            (close-delims token)
            (with-meta (into [:collection] data)
              {:error-message "Unmatched delimiter"})
            :else
            (recur data)))
        (with-meta (into [:collection] data)
          {:error-message "EOF while reading"})))))

(defn read-token [matcher]
  (let [token (group matcher 0)
        group (get group-names
                (some #(when (group matcher (inc %)) %) group-range)
                :whitespace)]
    (cond
      (open-delims token)
      (read-coll matcher [group token])
      (and (= group :symbol)
           (str/starts-with? token ":"))
      [:keyword token]
      :else
      [group token])))

(defn parse [s]
  (let [matcher (->regex s)]
    (loop [data []]
      (if (find matcher)
        (recur (conj data (read-token matcher)))
        data))))

