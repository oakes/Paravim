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

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :line 1
                       :column 0
                       :lines []}))

(defn assoc-chars [text-entity font-entity lines]
  (reduce
    (partial apply chars/assoc-char)
    text-entity
    (for [line-num (range (count lines))
          char-num (range (count (nth lines line-num)))
          :let [ch (get-in lines [line-num char-num])]]
      [line-num char-num (chars/crop-char font-entity ch)])))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs)
     (fn [{:keys [data]} baked-font]
       (let [font-entity (text/->font-entity game data baked-font)
             text-entity (c/compile game (i/->instanced-entity font-entity))
             rect-entity (e/->entity game primitives/rect)
             rects-entity (c/compile game (i/->instanced-entity rect-entity))]
         (swap! *state assoc
                :font-entity font-entity
                :text-entity (assoc-chars text-entity font-entity (:lines @*state))
                :rect-entity rect-entity
                :rects-entity rects-entity)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [text-entity rect-entity rects-entity line column lines]} @*state]
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    (when text-entity
      (let [{:keys [left top width height]} (-> text-entity :characters (get-in [(dec line) column]))
            rects-entity (i/assoc rects-entity 0 (-> rect-entity
                                                     (t/color [0 0 0 0.5])
                                                     (t/translate left top)
                                                     (t/scale width height)))]
        (c/render game (-> rects-entity
                           (t/project game-width game-height)
                           (t/translate 0 0)))
        (c/render game (-> text-entity
                           (t/project game-width game-height)
                           (t/translate 0 0))))))
  ;; return the game map
  game)

