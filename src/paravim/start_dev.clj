(ns paravim.start-dev
  (:require [paravim.start :as start]
            [paravim.core :as c]
            [paravim.vim :as vim]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]))

;; change init and tick to work better with hot code reloading
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
                                    game)))))

(defn start []
  (st/instrument)
  (set! s/*explain-out* expound/printer)
  (start/-main))

