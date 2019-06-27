(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [clojure.java.io :as io]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]]))
  (:import [org.lwjgl.stb STBTruetype STBTTFontinfo STBTTBakedChar]
           [org.lwjgl BufferUtils]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def bitmap-size 512)
(def font-height 128)

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
        ttf (doto (java.nio.ByteBuffer/allocateDirect (alength arr))
              (.order (java.nio.ByteOrder/nativeOrder))
              (.put arr)
              .flip)
        info (STBTTFontinfo/create)
        _ (or (STBTruetype/stbtt_InitFont info ttf)
              (throw (IllegalStateException. "Failed to initialize font information.")))
        #_#_
        bitmap (doto (java.nio.ByteBuffer/allocateDirect (* bitmap-size bitmap-size))
                 (.order (java.nio.ByteOrder/nativeOrder)))
        cdata (STBTTBakedChar/malloc 2048)
        bitmap (BufferUtils/createByteBuffer (* bitmap-size bitmap-size))
        _ (STBTruetype/stbtt_BakeFontBitmap ttf font-height bitmap bitmap-size bitmap-size 33 cdata)
        cseq (->> cdata .iterator iterator-seq (take 30) vec)
        i (-> cseq count rand-int)
        q (nth cseq i)
        x (.x0 q)
        y (.y0 q)
        w (- (.x1 q) (.x0 q))
        h (- (.y1 q) (.y0 q))]
    (println i)
    (swap! *state assoc
      :x x :y y :w w :h h
      :font
      (c/compile game (-> (e/->image-entity game bitmap bitmap-size bitmap-size)
                          (assoc-in
                            [:fragment :functions 'main]
                            '([]
                              (= outColor (texture u_image v_texCoord))
                              ("if" (== (.rgb outColor) (vec3 "0.0" "0.0" "0.0"))
                                "discard")
                              ("else"
                                (= outColor (vec4 "0.0" "0.0" "0.0" "1.0")))))
                          (update-in
                            [:uniforms 'u_image]
                            assoc
                            :opts {:mip-level 0
                                   :internal-fmt (gl game RED)
                                   :width bitmap-size
                                   :height bitmap-size
                                   :border 0
                                   :src-fmt (gl game RED)
                                   :src-type (gl game UNSIGNED_BYTE)}
                            :params {(gl game TEXTURE_MAG_FILTER)
                                     (gl game LINEAR)
                                     (gl game TEXTURE_MIN_FILTER)
                                     (gl game LINEAR)})
                          (t/crop x y w h))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [font x y w h]} @*state]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the font
    (c/render game (-> font
                       (t/project game-width game-height)
                       (t/translate x y)
                       (t/scale w h))))
  ;; return the game map
  game)

