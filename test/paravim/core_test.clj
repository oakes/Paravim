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

(defn get-characters [buffer-ptr entity-key]
  (get-in @c/*state [:buffers buffer-ptr entity-key :characters]))

(defn count-lines [buffer-ptr]
  (count (get-characters buffer-ptr :text-entity)))

(defn get-line [buffer-ptr line-num]
  (get-in @c/*state [:buffers buffer-ptr :lines line-num]))

(deftest delete-all-lines
  (is (= 5 (count-lines buffer-ptr)))
  (run! (partial vim/on-input game vim) ["g" "g" "d" "G"])
  (is (= 0 (count-lines buffer-ptr)))
  (vim/on-input game vim "u")
  (is (= 5 (count-lines buffer-ptr))))

(deftest dedent-function-body
  (is (= "(defn -main []" (get-line buffer-ptr 3)))
  (is (= "  (println \"Hello, World!\"))" (get-line buffer-ptr 4)))
  ;; dedent
  (v/set-cursor-position vim 5 0)
  (run! (partial vim/on-input game vim) ["i" "<Del>" "<Del>"])
  ;; make sure only the characters on those two lines changed
  (let [chars-before-parinfer (get-characters buffer-ptr :text-entity)
        chars-after-parinfer (get-characters buffer-ptr :parinfer-text-entity)]
    (dotimes [line-num (count-lines buffer-ptr)]
      (let [line-before-parinfer (nth chars-before-parinfer line-num)
            line-after-parinfer (nth chars-after-parinfer line-num)
            should-be-equal? (not (#{3 4} line-num))
            comparison ((if should-be-equal? = not=) line-before-parinfer line-after-parinfer)]
        (is comparison (str "Line " line-num " should be " (if should-be-equal? "equal" "different"))))))
  ;; execute parinfer
  (vim/on-input game vim "<Esc>")
  (is (= "(defn -main [])" (get-line buffer-ptr 3)))
  (is (= "(println \"Hello, World!\")" (get-line buffer-ptr 4))))

