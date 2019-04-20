(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [clojure.java.io :as io]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]]))
  (:import [org.lwjgl.stb STBTruetype STBTTFontinfo]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (let [arr (with-open [out (java.io.ByteArrayOutputStream.)]
              (io/copy (-> "ttf/FiraCode-Regular.ttf"
                           io/resource
                           io/input-stream)
                out)
              (.toByteArray out))
        buf (doto (java.nio.ByteBuffer/allocateDirect (alength arr))
              (.put arr))
        info (STBTTFontinfo/create)]
    (println (.capacity buf))
    (STBTruetype/stbtt_InitFont info buf)))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height)))
  ;; return the game map
  game)

