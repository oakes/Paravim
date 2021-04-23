(ns paravim.start-dev
  (:require [paravim.start :as start]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]))

(defn start []
  (st/instrument)
  (st/unstrument 'odoyle.rules/insert) ;; don't require specs for attributes
  (set! s/*explain-out* expound/printer)
  (start/-main))

