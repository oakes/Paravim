(ns paravim.session
  (:require [clara.rules :as clara]
            [clarax.rules :as clarax]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [paravim.session :refer [->session-wrapper]])))

(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Mouse [x y])
(defrecord MouseHover [target cursor mouse])
(defrecord TextBox [id left right top bottom])
(defrecord BoundingBox [id x1 y1 x2 y2 align])
(defrecord Prefs [tab buffer font-size])

(def queries
  '{:get-game
    (fn []
      (let [game Game]
        game))
    :get-window
    (fn []
      (let [window Window]
        window))
    :get-mouse
    (fn []
      (let [mouse Mouse]
        mouse))
    :get-prefs
    (fn []
      (let [prefs Prefs]
        prefs))
    :get-mouse-hover
    (fn []
      (let [mouse-hover MouseHover]
        mouse-hover))})

(def rules
  '{:mouse-hovers-over-text
    (let [window Window
          mouse Mouse
          mouse-hover MouseHover
          :when (not= mouse (:mouse mouse-hover))
          prefs Prefs
          :when (not= nil (:tab prefs))
          {:keys [id left right top bottom] :as text-box} TextBox
          :when (and (= id (:tab prefs))
                     (<= left (:x mouse) (- (:width window) right))
                     (<= (top (:height window) (:font-size prefs))
                         (:y mouse)
                         (bottom (:height window) (:font-size prefs))))]
      (clarax/merge! mouse-hover {:target :text
                                  :cursor :ibeam
                                  :mouse mouse}))
    :mouse-hovers-over-bounding-box
    (let [window Window
          mouse Mouse
          mouse-hover MouseHover
          :when (not= mouse (:mouse mouse-hover))
          prefs Prefs
          {:keys [x1 y1 x2 y2 align] :as bounding-box} BoundingBox
          :when (let [font-size (:font-size prefs)
                      game-width (:width window)
                      x1 (cond->> (* x1 font-size)
                                  (= :right align)
                                  (- game-width))
                      y1 (* y1 font-size)
                      x2 (cond->> (* x2 font-size)
                                  (= :right align)
                                  (- game-width))
                      y2 (* y2 font-size)]
                  (and (<= x1 (:x mouse) x2)
                       (<= y1 (:y mouse) y2)))]
      (clarax/merge! mouse-hover {:target (:id bounding-box)
                                  :cursor :hand
                                  :mouse mouse}))})

#?(:clj (defmacro ->session-wrapper []
          (list '->session (merge queries rules))))

(def *session (atom nil))

(defn restart! []
  (reset! *session
    (-> (->session-wrapper)
        (clara/insert
          (->Mouse 0 0)
          (->MouseHover nil nil nil)
          (map->Prefs {:tab :files
                       :buffer nil
                       :font-size (/ 1 4)}))
        clara/fire-rules)))

(restart!)

