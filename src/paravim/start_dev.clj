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

(defn start []
  (st/instrument)
  (set! s/*explain-out* expound/printer)
  (start/-main))

