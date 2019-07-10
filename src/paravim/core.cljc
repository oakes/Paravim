(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.gl.text :as text]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-bitmap-clj ->text-entity-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-bitmap-cljs ->text-entity-cljs]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def text "Hello, world!")

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-bitmap-clj :cljs load-bitmap-cljs)
     (fn [{:keys [data width height]}]
       (let [font-entity (c/compile game (text/->font-entity game data width height))
             text-entity (c/compile game (#?(:clj ->text-entity-clj :cljs ->text-entity-cljs) game font-entity text))]
         (swap! *state assoc :entity text-entity)))))

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
    (when-let [entity (:entity @*state)]
      (c/render game (-> entity
                         (t/project game-width game-height)
                         (t/translate 0 0)
                         (t/scale (:width entity) (:height entity))))))
  ;; return the game map
  game)

