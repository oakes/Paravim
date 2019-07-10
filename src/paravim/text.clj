(ns paravim.text
  (:require [play-cljc.text :as text]
            [play-cljc.gl.text :as gl.text]))

(def bitmap-size 512)
(def bitmap (text/->bitmap bitmap-size bitmap-size))
(def font-path "ttf/FiraCode-Regular.ttf")
(def font-height 64)
(def baked-font (text/->baked-font font-path font-height bitmap bitmap-size bitmap-size))

(defn load-bitmap-clj [callback]
  (callback bitmap bitmap-size bitmap-size))

(defmacro load-bitmap-cljs [callback]
  `(let [image# (js/Image. ~bitmap-size ~bitmap-size)]
     (doto image#
       (-> .-src (set! ~(text/bitmap->data-uri bitmap bitmap-size bitmap-size)))
       (-> .-onload (set! #(~callback image# ~bitmap-size ~bitmap-size))))))

(defn ->text-entity-clj [game font-entity text]
  (gl.text/->text-entity game baked-font font-entity text))

(defmacro ->text-entity-cljs [game font-entity text]
  `(gl.text/->text-entity ~game ~baked-font ~font-entity ~text))

