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

(defn wrap-coll [matcher data start-line-num start-char-num]
  (let [text (group matcher 0)
        line-num (line matcher)
        char-num (character matcher)]
    (vary-meta (into [:collection] data)
      assoc
      :start-line start-line-num
      :end-line line-num
      :start-char start-char-num
      :end-char (+ char-num (count text)))))

(defn read-coll [matcher [_ delim :as token-data]]
  (let [end-delim (delims delim)
        line-num (line matcher)
        char-num (character matcher)
        token-data (vary-meta token-data assoc
                     :open-delim? true
                     :start-line line-num
                     :end-line line-num
                     :start-char char-num
                     :end-char (+ char-num (count delim)))]
    (loop [data [token-data]]
      (if (find matcher)
        (let [[_ token :as token-data] (read-token matcher)
              data (conj data token-data)]
          (cond
            (= token end-delim)
            (wrap-coll matcher data line-num char-num)
            (close-delims token)
            (vary-meta (wrap-coll matcher data line-num char-num)
              assoc :error-message "Unmatched delimiter")
            :else
            (recur data)))
        (vary-meta (wrap-coll matcher data line-num char-num)
          assoc :error-message "EOF while reading")))))

(defn read-token [matcher]
  (let [token (group matcher 0)
        group (get group-names
                (some #(when (group matcher (inc %)) %) group-range)
                :whitespace)]
    (if (and (= group :delimiter)
             (open-delims token))
      (read-coll matcher [group token])
      (as-> group $
            (if (and (= $ :symbol)
                     (str/starts-with? token ":"))
              :keyword
              $)
            [$ token]
            (let [line-num (line matcher)
                  char-num (character matcher)]
              (vary-meta $ assoc
                :start-line line-num
                :start-char char-num))))))

(defn parse [s]
  (let [matcher (->regex s)]
    (loop [data []]
      (if (find matcher)
        (recur (conj data (read-token matcher)))
        data))))

