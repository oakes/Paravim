(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.text :as text]
            [play-cljc.gl.text :as gl.text]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def bitmap-size 512)
(def font-height 64)
(def text "Hello, world!")

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (let [baked-font (text/->baked-font "ttf/FiraCode-Regular.ttf" font-height bitmap-size bitmap-size)
        font (c/compile game (gl.text/->font-entity game baked-font))
        entity (c/compile game (gl.text/->text-entity game font text))]
    (swap! *state assoc :entity entity)))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the font
    (let [{:keys [entity]} @*state]
      (c/render game (-> entity
                         (t/project game-width game-height)
                         (t/translate 0 0)
                         (t/scale (:width entity) (:height entity))))))
  ;; return the game map
  game)

