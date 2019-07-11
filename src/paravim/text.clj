(ns paravim.text
  (:require [play-cljc.text :as text]
            [play-cljc.gl.text :as gl.text]))

(def bitmap-size 512)
(def bitmap (text/->bitmap bitmap-size bitmap-size))
(def font-height 64)
(def baked-font (text/->baked-font "ttf/FiraCode-Regular.ttf" font-height bitmap))

(defn load-font-clj [callback]
  (callback bitmap baked-font))

(defmacro load-font-cljs [callback]
  (let [{:keys [width height]} bitmap]
    `(let [image# (js/Image. ~width ~height)]
       (doto image#
         (-> .-src (set! ~(text/bitmap->data-uri bitmap)))
         (-> .-onload (set! #(~callback {:data image# :width ~width :height ~height} ~baked-font)))))))

