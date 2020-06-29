(ns paravim.start-dev
  (:require [paravim.start :as start]
            [paravim.core :as c]
            [paravim.vim :as vim]
            [paravim.session :as session]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s])
  (:import [paravim.session Font]))

(session/merge-into-session {:font-changed
                             (let [font Font]
                               (println font))})

(defn intern-for-dev
  "Change a few functions to improve hot code reloading"
  []
  (let [init c/init
        tick c/tick]
    (intern 'paravim.core 'init (fn [game]
                                  (doto (init game)
                                    vim/init)))
    (intern 'paravim.core 'tick (fn [game]
                                  (try
                                    (tick game)
                                    (catch Exception e
                                      (.printStackTrace e)
                                      game))))
    ;; when paravim.core is reloaded, this function will re-run
    (intern 'paravim.core 'on-reload intern-for-dev)))

(defn start []
  (st/instrument)
  (set! s/*explain-out* expound/printer)
  (intern-for-dev)
  (start/-main))

