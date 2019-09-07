(ns paravim.core-test
  (:require [clojure.test :refer :all]
            [paravim.start :as start]
            [paravim.vim :as vim]
            [libvim-clj.core :as v]
            [paravim.core :as c]
            [play-cljc.gl.core :as pc]))

(def window (start/->window))
(def vim (vim/->vim))
(def game (start/->game window vim))

(deftest delete-all-lines-and-undo
  (let [buffer-ptr (v/open-buffer vim "test/test_file.clj")
        char-count #(count (get-in @c/*state [:buffers buffer-ptr :text-entity :characters]))]
    (v/set-current-buffer vim buffer-ptr)
    (is (= 5 (char-count)))
    (run! (partial vim/on-input game vim) ["g" "g" "d" "G"])
    (is (= 0 (char-count)))
    (vim/on-input game vim "u")
    (is (= 5 (char-count)))))

