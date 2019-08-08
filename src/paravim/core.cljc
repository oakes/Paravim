(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.text :as text]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.primitives-2d :as primitives]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-font-cljs]])))

(def orig-camera (e/->camera true))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :camera (t/translate orig-camera 0 0)
                       :camera-x 0
                       :camera-y 0
                       :lines []}))

(defn assoc-chars [text-entity font-entity lines]
  (reduce
    (partial apply chars/assoc-char)
    text-entity
    (for [line-num (range (count lines))
          char-num (range (count (nth lines line-num)))
          :let [ch (get-in lines [line-num char-num])]]
      [line-num char-num (chars/crop-char font-entity ch)])))

(defn update-cursor [{:keys [text-entity rect-entity baked-font] :as state} game line column]
  (if (and text-entity rect-entity baked-font)
    (let [font-width (-> baked-font :baked-chars (nth (- 115 (:first-char baked-font))) :w)
          font-height (:font-height baked-font)
          {:keys [left top width height]
           :or {left (* column font-width)
                top (* (dec line) font-height)
                width font-width
                height font-height}}
          (-> text-entity :characters (get-in [(dec line) column]))
          width (if (== 0 width) font-width width)]
      (-> state
          (assoc :cursor-entity (-> rect-entity
                                    (t/color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
                                    (t/translate left top)
                                    (t/scale width height)))
          (as-> state
                (let [{:keys [camera camera-x camera-y]} state
                      cursor-bottom (+ top font-height)
                      cursor-right (+ left width)
                      game-width (utils/get-width game)
                      game-height (utils/get-height game)
                      camera-bottom (+ camera-y game-height)
                      camera-right (+ camera-x game-width)
                      camera-x (cond
                                 (< left camera-x)
                                 left
                                 (> cursor-right camera-right)
                                 (- cursor-right game-width)
                                 :else
                                 camera-x)
                      camera-y (cond
                                 (< top camera-y)
                                 top
                                 (> cursor-bottom camera-bottom)
                                 (- cursor-bottom game-height)
                                 :else
                                 camera-y)]
                  (assoc state
                    :camera (t/translate orig-camera camera-x camera-y)
                    :camera-x camera-x
                    :camera-y camera-y)))))
    state))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs)
     (fn [{:keys [data]} baked-font]
       (let [font-entity (t/color (text/->font-entity game data baked-font) [1 1 1 1])
             text-entity (c/compile game (i/->instanced-entity font-entity))
             rect-entity (e/->entity game primitives/rect)
             rects-entity (c/compile game (i/->instanced-entity rect-entity))]
         (swap! *state
           (fn [state]
             (-> state
                 (assoc
                   :baked-font baked-font
                   :font-entity font-entity
                   :text-entity (assoc-chars text-entity font-entity (:lines @*state))
                   :rect-entity rect-entity
                   :rects-entity rects-entity)
                 (update-cursor game 1 0))))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 0 255) (/ 16 255) (/ 64 255) 0.9] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [text-entity rects-entity camera cursor-entity]} @*state]
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    (when cursor-entity
      (c/render game (-> (i/assoc rects-entity 0 cursor-entity)
                         (t/project game-width game-height)
                         (t/camera camera))))
    (when text-entity
      (c/render game (-> text-entity
                         (t/project game-width game-height)
                         (t/camera camera)))))
  ;; return the game map
  game)

