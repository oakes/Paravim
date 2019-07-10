(ns paravim.text
  (:require [play-cljc.text :as text]
            [play-cljc.gl.text :as gl.text]))

(def bitmap-size 512)
(def bitmap (text/->bitmap bitmap-size bitmap-size))
(def font-height 64)
(def baked-font (text/->baked-font "ttf/FiraCode-Regular.ttf" font-height bitmap))

(defn load-bitmap-clj [callback]
  (callback bitmap))

(defmacro load-bitmap-cljs [callback]
  `(let [image# (js/Image. ~(:width bitmap) ~(:height bitmap))]
     (doto image#
       (-> .-src (set! ~(text/bitmap->data-uri bitmap)))
       (-> .-onload (set! #(~callback (assoc ~(dissoc bitmap :data) :data image#)))))))

(defn ->text-entity-clj [game font-entity text]
  (gl.text/->text-entity game baked-font font-entity text))

(defmacro ->text-entity-cljs [game font-entity text]
  `(gl.text/->text-entity ~game ~baked-font ~font-entity ~text))
