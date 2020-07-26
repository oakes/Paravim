(ns paravim.constants
  (:require [play-cljc.gl.entities-2d :as e]))

(def orig-camera (e/->camera))

(def buttons [{:id :paravim.session/font-inc
               :text "Font +"
               :shortcut-char 5}
              {:id :paravim.session/font-dec
               :text "Font -"
               :shortcut-char 5}
              {:id :paravim.session/reload-file
               :text "Reload File"
               :shortcut-char 7}])

(def tab->path {:paravim.session/files "scratch.clj"
                :paravim.session/repl-in "repl.in"
                :paravim.session/repl-out "repl.out"})

(def tabs [{:id :paravim.session/files
            :text "Files"}
           {:id :paravim.session/repl-in
            :text "REPL In"}
           {:id :paravim.session/repl-out
            :text "REPL Out"}])

(def tab-ids (mapv :id tabs))
(def tab? (set tab-ids))

(def font-size-step (/ 1 16))
(def min-font-size (/ 1 8))
(def max-font-size 1)
(def default-font-multiplier (/ 1 4))

(def repl-in-lines 5)

(def minimap-scale 6)
(def minimap-min-chars 30)
(def minimap-min-size-to-show-chars (/ (* default-font-multiplier 2) minimap-scale))

(def max-visible-lines 1000)
(def max-clojure-lines 1000)

