(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [clojure.java.io :as io]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]]))
  (:import [org.lwjgl.stb STBTruetype STBTTFontinfo STBTTBakedChar]
           [java.nio.file Files Paths]
           [java.nio.channels Channels]
           [org.lwjgl BufferUtils]
           [org.lwjgl.system MemoryStack]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def bitmap-size 512)
(def font-height 32)

(defn resize-buffer [buffer new-capacity]
  (let [new-buffer (BufferUtils/createByteBuffer new-capacity)]
    (.flip buffer)
    (.put buffer new-buffer)
    new-buffer))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (let [#_#_
        arr (with-open [out (java.io.ByteArrayOutputStream.)]
              (io/copy (-> "ttf/FiraCode-Regular.ttf"
                           io/resource
                           io/input-stream)
                out)
              (.toByteArray out))
        #_#_
        ttf (doto (java.nio.ByteBuffer/allocateDirect (alength arr))
              (.order (java.nio.ByteOrder/nativeOrder))
              (.put arr)
              .flip)
        source (-> "ttf/FiraCode-Regular.ttf"
                   io/resource
                   io/input-stream)
        rbc (Channels/newChannel source)
        ttf (loop [buffer (BufferUtils/createByteBuffer (* 512 1024))]
              (let [bs (.read rbc buffer)]
                (cond
                  (= bs -1)
                  buffer
                  (= 0 (.remaining buffer))
                  (recur (resize-buffer buffer (-> buffer .capacity (* 3) (/ 2))))
                  :else
                  (recur buffer))))
        _ (.flip ttf)
        info (STBTTFontinfo/create)
        _ (or (STBTruetype/stbtt_InitFont info ttf)
              (throw (IllegalStateException. "Failed to initialize font information.")))
        stack (MemoryStack/stackPush)
        p-ascent (.mallocInt stack 1)
        p-descent (.mallocInt stack 1)
        p-line-gap (.mallocInt stack 1)
        _ (STBTruetype/stbtt_GetFontVMetrics info p-ascent p-descent p-line-gap)
        ascent (.get p-ascent 0)
        descent (.get p-descent 0)
        line-gap (.get p-line-gap 0)
        #_#_
        codepoint->glyph (reduce
                           (fn [m codepoint]
                             (let [glyph-index (STBTruetype/stbtt_FindGlyphIndex info (int codepoint))]
                               (if (pos? glyph-index)
                                 (assoc m codepoint glyph-index)
                                 m)))
                           {}
                           (range 0x0000 0xFFFF))
        #_#_
        bitmap (doto (java.nio.ByteBuffer/allocateDirect (* bitmap-size bitmap-size))
                 (.order (java.nio.ByteOrder/nativeOrder)))
        cdata (STBTTBakedChar/malloc 96)
        bitmap (BufferUtils/createByteBuffer (* bitmap-size bitmap-size))]
    (STBTruetype/stbtt_BakeFontBitmap ttf font-height bitmap bitmap-size bitmap-size 32 cdata)
    (swap! *state assoc :font
      (c/compile game (-> (e/->image-entity game bitmap bitmap-size bitmap-size)
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

