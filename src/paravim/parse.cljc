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
  (group [this index])
  (index [this])
  (line [this])
  (character [this]))

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
           :cljs (.-index @*matcher)))
      (line [this]
        (->> (subs match-str 0 (inc (index this)))
             (re-seq #"\n")
             count))
      (character [this]
        (let [s (subs match-str 0 (inc (index this)))]
          (->> s
               (or (some->> (str/last-index-of s "\n")
                            inc
                            (subs s))
                   s)
               count
               dec))))))

(declare read-token)

(defn read-coll [matcher [_ delim :as token-data]]
  (let [end-delim (delims delim)
        line-num (line matcher)
        char-num (character matcher)]
    (vary-meta
      (loop [data [token-data]]
        (if (find matcher)
          (let [[_ token :as token-data] (read-token matcher)
                data (conj data token-data)]
            (cond
              (= token end-delim)
              (into [:collection] data)
              (close-delims token)
              (vary-meta (into [:collection] data)
                assoc :error-message "Unmatched delimiter")
              :else
              (recur data)))
          (vary-meta (into [:collection] data)
            assoc :error-message "EOF while reading")))
      assoc :line-num line-num :char-num char-num)))

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

