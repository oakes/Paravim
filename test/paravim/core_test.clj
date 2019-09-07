(ns paravim.core-test
  (:require [clojure.test :refer :all]
            [paravim.start :as start]
            [paravim.vim :as vim]
            [libvim-clj.core :as v]
            [paravim.core :as c]))

(def window (start/->window))
(def vim (vim/->vim))
(def game (start/->game window vim))

(def buffer-ptr (v/open-buffer vim "test/resources/core.clj"))
(v/set-current-buffer vim buffer-ptr)

(defn count-lines []
  (count (get-in @c/*state [:buffers buffer-ptr :text-entity :characters])))

(deftest delete-all-lines
  (is (= 5 (count-lines)))
  (run! (partial vim/on-input game vim) ["g" "g" "d" "G"])
  (is (= 0 (count-lines)))
  (vim/on-input game vim "u")
  (is (= 5 (count-lines))))

