(ns paravim.constants
  (:require [play-cljc.gl.entities-2d :as e]))

(def orig-camera (e/->camera))

(def buttons [{:id :font-inc
               :text "Font +"
               :shortcut-char 5}
              {:id :font-dec
               :text "Font -"
               :shortcut-char 5}
              {:id :reload-file
               :text "Reload File"
               :shortcut-char 7}])

(def tab->path {:files "scratch.clj"
                :repl-in "repl.in"
                :repl-out "repl.out"})

(def tabs [{:id :files
            :text "Files"}
           {:id :repl-in
            :text "REPL In"}
           {:id :repl-out
            :text "REPL Out"}])

(def tab-ids (mapv :id tabs))
(def tab? (set tab-ids))

(def font-size-step (/ 1 16))
(def min-font-size (/ 1 8))
(def max-font-size 1)
(def default-font-size (/ 1 4))

(def repl-in-lines 5)

(def minimap-scale 6)
(def minimap-min-chars 30)
(def minimap-min-size-to-show-chars (/ (* default-font-size 2) minimap-scale))
(def max-lines 1000)

