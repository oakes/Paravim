(ns paravim.start-dev
  (:require [paravim.start :as start]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]))

(defn start []
  (st/instrument)
  (set! s/*explain-out* expound/printer)
  (start/-main))

