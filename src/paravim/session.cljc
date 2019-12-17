(ns paravim.session
  (:require [clara.rules :as clara]
            [clarax.rules :as clarax]
            [play-cljc.gl.entities-2d :as e]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [paravim.session :refer [->session-wrapper]])))

(def orig-camera (e/->camera true))
(def tabs [{:id :files
            :text "Files"}
           {:id :repl-in
            :text "REPL In"}
           {:id :repl-out
            :text "REPL Out"}])
(def tab-ids (mapv :id tabs))
(def tab? (set tab-ids))

(def font-size-step (/ 1 16))
(def min-font-size (/ 1 8))
(def max-font-size 1)

(defrecord Game [total-time delta-time context pipes send-input! vim])
(defrecord Window [width height])
(defrecord Mouse [x y])
(defrecord MouseHover [target cursor mouse])
(defrecord MouseClick [button reload-file!])
(defrecord TextBox [id left right top bottom])
(defrecord BoundingBox [id x1 y1 x2 y2 align])
(defrecord Prefs [font-size])
(defrecord CurrentTab [id])
(defrecord CurrentBuffer [id])

(defn change-font-size! [{:keys [font-size] :as prefs} diff]
  (let [new-val (+ font-size diff)]
    (when (<= min-font-size new-val max-font-size)
      (clarax/merge! prefs {:font-size new-val}))))

(defn font-dec! [prefs]
  (change-font-size! prefs (- font-size-step)))

(defn font-inc! [prefs]
  (change-font-size! prefs font-size-step))

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
    :get-current-tab
    (fn []
      (let [current-tab CurrentTab]
        current-tab))
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
          current-tab CurrentTab
          :when (not= nil (:id current-tab))
          prefs Prefs
          {:keys [id left right top bottom] :as text-box} TextBox
          :when (and (= id (:id current-tab))
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
                                  :mouse mouse}))
    :mouse-clicked
    (let [mouse-click MouseClick
          mouse-hover MouseHover
          current-tab CurrentTab
          prefs Prefs]
      (clara/retract! mouse-click)
      (when (= :left (:button mouse-click))
        (let [{:keys [target]} mouse-hover]
          (if (tab? target)
            (clarax/merge! current-tab {:id target})
            (case target
              :font-dec (font-dec! prefs)
              :font-inc (font-inc! prefs)
              :reload-file (when (:reload-file! mouse-click)
                             (clarax/merge! current-tab {:id :repl-in}))
              nil)))))
    :tab-changed
    (let [current-tab CurrentTab]
      (println current-tab))
    :prefs-changed
    (let [prefs Prefs]
      (println prefs))})

#?(:clj (defmacro ->session-wrapper []
          (list '->session (merge queries rules))))

(def *session (atom nil))

(defn restart! []
  (reset! *session
    (-> (->session-wrapper)
        (clara/insert
          (->Mouse 0 0)
          (->MouseHover nil nil nil)
          (->CurrentTab :files)
          (->CurrentBuffer nil)
          (->Prefs (/ 1 4)))
        clara/fire-rules)))

(restart!)

