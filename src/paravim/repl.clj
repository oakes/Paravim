(ns paravim.repl
  (:require [clojure.string :as str]
            [clojure.main])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io PipedWriter PipedReader PrintWriter]))

(defn remove-returns [^String s]
  (str/escape s {\return ""}))

(def ^:const repl-buffer-size 32)
(def *current-ns (atom nil))

(defn pipe-into-console! [in-pipe callback]
  (let [ca (char-array repl-buffer-size)]
    (.start
      (Thread.
        (fn []
          (loop []
            (when-let [read (try (.read in-pipe ca)
                              (catch Exception _))]
              (when (pos? read)
                (let [s (remove-returns (String. ca 0 read))]
                  (callback s)
                  (Thread/sleep 100) ; prevent thread from being flooded
                  (recur))))))))))

(defn create-pipes []
  (let [out-pipe (PipedWriter.)
        in (LineNumberingPushbackReader. (PipedReader. out-pipe))
        pout (PipedWriter.)
        out (PrintWriter. pout)
        in-pipe (PipedReader. pout)]
    {:in in :out out :in-pipe in-pipe :out-pipe out-pipe}))

(defn start-repl-thread! [main-ns pipes callback]
  (let [{:keys [in-pipe in out]} pipes]
    (pipe-into-console! in-pipe callback)
    (.start
      (Thread.
        (fn []
          (binding [*out* out
                    *err* out
                    *in* in]
            (try
              (clojure.main/repl
                :init
                (fn []
                  (when main-ns
                    (doto main-ns require in-ns)))
                :read
                (fn [request-prompt request-exit]
                  (let [form (clojure.main/repl-read request-prompt request-exit)]
                    (if (= 'paravim.repl/exit form) request-exit form))))
              (catch Exception e (some-> (.getMessage e) println))
              (finally (println "=== Finished ==="))))))))
  pipes)

(defn reload-file! [{:keys [lines file-name clojure?] :as buffer} pipes current-tab]
  (when (and clojure? (= current-tab :paravim.session/files))
    (let [{:keys [out-pipe]} pipes
          content (str/join \newline lines)
          first-form (try (read-string {:read-cond :allow} content)
                       (catch Exception _))
          new-ns (when (and (list? first-form)
                            (= 'ns (first first-form))
                            (symbol? (second first-form))
                            (not= @*current-ns (second first-form)))
                   (second first-form))
          reload-content (str "(do "
                              (pr-str '(println))
                              (pr-str (list 'println "Reloading" file-name))
                              content
                              ;; newline in case the last line of `content` is a comment
                              \newline
                              (pr-str '(reset! paravim.repl/*current-ns (-> *ns* str symbol)))
                              '(quote ...done)
                              ")"
                              ;; newline to force repl to finish
                              \newline)]
      (when new-ns
        (doto out-pipe
          (.write (->
                    (list 'do
                      '(println)
                      (list 'println "Switching to" (list 'quote new-ns))
                      (list 'try
                        (list 'doto (list 'quote new-ns)
                          'require
                          'in-ns)
                        '(quote ...done)
                        '(catch Exception _ (quote ...failed))))
                    pr-str
                    (str \newline)))
          .flush))
      (doto out-pipe
        (.write reload-content)
        .flush)
      true)))

