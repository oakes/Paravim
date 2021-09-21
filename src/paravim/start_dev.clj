(ns paravim.start-dev
  (:require [paravim.start :as start]
            [clojure.spec.test.alpha :as st]))

(defn start []
  (st/instrument)
  (start/-main))

