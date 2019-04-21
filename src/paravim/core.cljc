(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [clojure.java.io :as io]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]]))
  (:import [org.lwjgl.stb STBTruetype STBTTFontinfo STBTTBakedChar]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def bitmap-size 512)
(def font-height 32)

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
        info (doto (STBTTFontinfo/create)
               (STBTruetype/stbtt_InitFont ttf))
        codepoint->glyph (reduce
                           (fn [m codepoint]
                             (let [glyph-index (STBTruetype/stbtt_FindGlyphIndex info (int codepoint))]
                               (if (pos? glyph-index)
                                 (assoc m codepoint glyph-index)
                                 m)))
                           {}
                           (range 0x0000 0xFFFF))
        bitmap (doto (java.nio.ByteBuffer/allocateDirect (* bitmap-size bitmap-size))
                 (.order (java.nio.ByteOrder/nativeOrder)))
        cdata (STBTTBakedChar/malloc 96)]
    (STBTruetype/stbtt_BakeFontBitmap ttf font-height bitmap bitmap-size bitmap-size 32 cdata)
    (swap! *state assoc :font
      (c/compile game (-> (e/->image-entity game bitmap bitmap-size bitmap-size)
                          (update-in
                            [:uniforms 'u_image]
                            assoc
                            :opts {:mip-level 0
                                   :internal-fmt (gl game ALPHA)
                                   :width bitmap-size
                                   :height bitmap-size
                                   :border 0
                                   :src-fmt (gl game ALPHA)
                                   :src-type (gl game UNSIGNED_BYTE)}
                            :params {(gl game TEXTURE_MAG_FILTER)
                                     (gl game LINEAR)
                                     (gl game TEXTURE_MIN_FILTER)
                                     (gl game LINEAR)}))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [font]} @*state]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the font
    (c/render game (-> font
                       (t/project game-width game-height)
                       (t/translate 0 0)
                       (t/scale bitmap-size bitmap-size))))
  ;; return the game map
  game)

