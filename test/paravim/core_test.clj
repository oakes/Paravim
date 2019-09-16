(ns paravim.core-test
  (:require [clojure.test :refer :all]
            [paravim.start :as start]
            [paravim.vim :as vim]
            [libvim-clj.core :as v]
            [paravim.core :as c]
            [play-cljc.gl.core :as pc])
  (:import [org.lwjgl.glfw GLFW]))

(def window (start/->window))
(def vim (vim/->vim))
(def game (pc/->game window))
(def paravim-utils (start/init game vim nil))

(defn get-characters [buffer-ptr entity-key]
  (get-in @c/*state [:buffers buffer-ptr entity-key :characters]))

(defn count-lines [buffer-ptr]
  (count (get-characters buffer-ptr :text-entity)))

(defn get-line [buffer-ptr line-num]
  (get-in @c/*state [:buffers buffer-ptr :lines line-num]))

(def core-buffer (v/open-buffer vim "test/resources/core.clj"))
(def bad-indent-buffer (v/open-buffer vim "test/resources/bad_indent.clj"))

(deftest delete-all-lines
  (v/set-current-buffer vim core-buffer)
  (is (= 5 (count-lines core-buffer)))
  (run! (partial vim/on-input game vim) ["g" "g" "d" "G"])
  (is (= 0 (count-lines core-buffer)))
  (vim/on-input game vim "u")
  (is (= 5 (count-lines core-buffer))))

(deftest dedent-function-body
  (v/set-current-buffer vim core-buffer)
  (is (= "(defn -main []" (get-line core-buffer 3)))
  (is (= "  (println \"Hello, World!\"))" (get-line core-buffer 4)))
  ;; dedent
  (v/set-cursor-position vim 5 0)
  (run! (partial vim/on-input game vim) ["i" "<Del>" "<Del>"])
  ;; make sure only the characters on those two lines changed
  (let [chars-before-parinfer (get-characters core-buffer :text-entity)
        chars-after-parinfer (get-characters core-buffer :parinfer-text-entity)]
    (dotimes [line-num (count-lines core-buffer)]
      (let [line-before-parinfer (nth chars-before-parinfer line-num)
            line-after-parinfer (nth chars-after-parinfer line-num)
            should-be-equal? (not (#{3 4} line-num))
            comparison ((if should-be-equal? = not=) line-before-parinfer line-after-parinfer)]
        (is comparison (str "Line " line-num " should be " (if should-be-equal? "equal" "different"))))))
  ;; execute parinfer
  (vim/on-input game vim "<Esc>")
  (is (= "(defn -main [])" (get-line core-buffer 3)))
  (is (= "(println \"Hello, World!\")" (get-line core-buffer 4)))
  ;; undo
  (run! (partial vim/on-input game vim) ["u" "u" "u"])
  (is (= "(defn -main []" (get-line core-buffer 3)))
  (is (= "  (println \"Hello, World!\"))" (get-line core-buffer 4))))

(deftest fix-bad-indentation
  (v/set-current-buffer vim bad-indent-buffer)
  (is (= "2 3)" (get-line bad-indent-buffer 3)))
  (is (= "    (println a b))" (get-line bad-indent-buffer 7)))
  (vim/on-input game vim "<Down>") ;; any keystroke will trigger parinfer
  (is (= " 2 3)" (get-line bad-indent-buffer 3)))
  (is (= "   (println a b))" (get-line bad-indent-buffer 7))))

(deftest dont-break-play-cljc-template
  (v/set-current-buffer vim core-buffer)
  (is (= (c/get-mode) 'NORMAL))
  (is (map? paravim-utils))
  (let [handle (:context game)]
    (start/on-mouse-move! paravim-utils handle 0 0)
    (start/on-mouse-click! paravim-utils handle GLFW/GLFW_MOUSE_BUTTON_LEFT GLFW/GLFW_PRESS 0)
    (start/on-key! paravim-utils handle GLFW/GLFW_KEY_DOWN 0 GLFW/GLFW_PRESS 0)
    (start/on-char! paravim-utils handle (int \j))
    (start/on-resize! paravim-utils handle 800 600)))

;; this test reproduces a crash in libvim
;; when search highlighting is enabled
;; without hlsearch set
(deftest search-highlights-crash
  (v/set-current-buffer vim core-buffer)
  (dotimes [_ 50]
    (run! (partial vim/on-input game vim)
      ["/" "h" "e" "l" "l" "o"
       "<Esc>"
       ":" "%" "s" "/" "h" "e" "l" "l" "o" "/" "w" "o" "r" "l" "d"
       "<Esc>"])))

