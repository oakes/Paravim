(ns paravim.core
  (:require [paravim.utils :as utils]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [clojure.java.io :as io]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]]))
  (:import [org.lwjgl.stb STBTruetype STBTTFontinfo STBTTBakedChar]
           [org.lwjgl BufferUtils]
           [org.lwjgl.system MemoryStack]))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}}))

(def bitmap-size 512)
(def font-height 64)

(def text (vec (seq "Hello, world!")))

(def flip-y-matrix
  [1  0  0
   0 -1  0
   0  0  1])

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
        _ (STBTruetype/stbtt_BakeFontBitmap ttf font-height bitmap bitmap-size bitmap-size 32 cdata)
        stack (MemoryStack/stackPush)
        *ascent (.callocInt stack 1)
        *descent (.callocInt stack 1)
        *line-gap (.callocInt stack 1)
        _ (STBTruetype/stbtt_GetFontVMetrics info *ascent *descent *line-gap)
        ascent (.get *ascent 0)
        descent (.get *descent 0)
        line-gap (.get *line-gap 0)
        scale (STBTruetype/stbtt_ScaleForPixelHeight info font-height)
        baseline (* ascent scale)
        chars (->> cdata .iterator iterator-seq
                   (mapv (fn [q]
                           {:x (.x0 q)
                            :y (.y0 q)
                            :w (- (.x1 q) (.x0 q))
                            :h (- (.y1 q) (.y0 q))
                            :x-off (.xoff q)
                            :y-off (.yoff q)
                            :x-adv (.xadvance q)})))
        font (c/compile game (-> (e/->image-entity game bitmap bitmap-size bitmap-size)
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
                                            (gl game LINEAR)})))
        entity (loop [i 0
                      total 0
                      inner-entities []]
                 (if-let [ch (get text i)]
                   (let [{:keys [x y w h x-off y-off x-adv]} (nth chars (- (int ch) 32))]
                     (recur (inc i)
                            (+ total x-adv)
                            (conj inner-entities
                                  (-> font
                                      (t/project bitmap-size bitmap-size)
                                      (t/crop x y w h)
                                      (t/translate (+ total x-off) (- font-height baseline y-off))
                                      (t/scale w h)
                                      (update-in [:uniforms 'u_matrix]
                                                 #(m/multiply-matrices 3 flip-y-matrix %))))))
                   (-> (e/->image-entity game nil total font-height)
                       (assoc
                         :width total
                         :height font-height
                         :render-to-texture {'u_image (mapv #(assoc % :viewport {:x 0 :y (- font-height bitmap-size) :width bitmap-size :height bitmap-size})
                                                            inner-entities)})
                       (#(c/compile game %)))))]
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

