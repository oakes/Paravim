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
                       :current-buffer nil
                       :buffers {}}))

(defn assoc-lines [text-entity font-entity lines]
  (reduce
    (partial apply chars/assoc-char)
    text-entity
    (for [line-num (range (count lines))
          char-num (range (count (nth lines line-num)))
          :let [ch (get-in lines [line-num char-num])]]
      [line-num char-num (chars/crop-char font-entity ch)])))

(defn replace-lines [{:keys [lines] :as buffer} text-entity font-entity new-lines start-line line-count-change]
  (let [first-line (dec start-line)
        lines-before (subvec lines 0 first-line)
        lines-to-remove (if (neg? line-count-change) (* -1 line-count-change) 0)
        lines-to-add (if (neg? line-count-change) 0 line-count-change)
        lines-after (subvec lines (+ (count new-lines) first-line lines-to-remove))
        new-lines (into new-lines (vec (repeat lines-to-add [])))
        lines (->> lines-after
                   (into new-lines)
                   (into lines-before)
                   (into []))]
    (assoc buffer
      :text-entity (assoc-lines text-entity font-entity lines)
      :lines lines)))

(defn update-cursor [{:keys [font-width font-height base-rect-entity] :as state} game buffer-ptr line column]
  (update-in state [:buffers buffer-ptr]
    (fn [buffer]
      (let [left-char (get-in buffer [:text-entity :characters (dec line) (dec column)])
            curr-char (get-in buffer [:text-entity :characters (dec line) column])
            {:keys [left top width height]} curr-char
            width (if (or (nil? width) (== 0 width)) font-width width)
            left (or left
                     (some-> (:left left-char) (+ width))
                     0)
            top (or top (* (dec line) font-height))
            height (or height font-height)
            cursor-entity (-> base-rect-entity
                              (t/color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
                              (t/translate left top)
                              (t/scale width height))]
        (-> buffer
            (update :rects-entity #(i/assoc % 0 cursor-entity))
            (as-> buffer
                  (let [{:keys [camera camera-x camera-y]} buffer
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
                    (assoc buffer
                      :camera (t/translate orig-camera camera-x camera-y)
                      :camera-x camera-x
                      :camera-y camera-y))))))))

(defn assoc-buffer [{:keys [base-font-entity base-text-entity base-rects-entity] :as state} buffer-ptr lines]
  (assoc-in state [:buffers buffer-ptr]
    {:text-entity (assoc-lines base-text-entity base-font-entity lines)
     :rects-entity base-rects-entity
     :camera (t/translate orig-camera 0 0)
     :camera-x 0
     :camera-y 0
     :lines lines}))

(defn modify-buffer [{:keys [base-font-entity base-text-entity] :as state} game buffer-ptr lines start-line line-count-change]
  (update-in state [:buffers buffer-ptr]
    replace-lines base-text-entity base-font-entity lines start-line line-count-change))

(defn init [game callback]
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
         (swap! *state assoc
           :font-width (-> baked-font :baked-chars (nth (- 115 (:first-char baked-font))) :w)
           :font-height (:font-height baked-font)
           :base-font-entity font-entity
           :base-text-entity text-entity
           :base-rect-entity rect-entity
           :base-rects-entity rects-entity)
         (callback)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 0 255) (/ 16 255) (/ 64 255) 0.9] :depth 1}})

(defn tick [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [current-buffer buffers]} @*state]
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    (when-let [{:keys [rects-entity text-entity camera]} (get buffers current-buffer)]
      (c/render game (-> rects-entity
                         (t/project game-width game-height)
                         (t/camera camera)))
      (c/render game (-> text-entity
                         (t/project game-width game-height)
                         (t/camera camera)))))
  ;; return the game map
  game)

