(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.text :as text]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-font-cljs]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :lines []}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs)
     (fn [{:keys [data]} baked-font]
       (let [font-entity (text/->font-entity game data baked-font)
             text-entity (c/compile game (i/->instanced-entity font-entity))]
         (swap! *state assoc :font-entity font-entity :text-entity text-entity)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [font-entity text-entity lines]} @*state]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the font
    (when text-entity
      (c/render game (-> (reduce
                           (partial apply chars/assoc-char)
                           text-entity
                           (for [line-num (range (count lines))
                                 char-num (range (count (nth lines line-num)))
                                 :let [ch (get-in lines [line-num char-num])]]
                             [line-num char-num (chars/crop-char font-entity ch)]))
                         (t/project game-width game-height)
                         (t/translate 0 0)))))
  ;; return the game map
  game)

