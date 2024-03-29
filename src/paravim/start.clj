(ns paravim.start
  (:require [paravim.core :as c]
            [paravim.repl :as repl]
            [paravim.vim :as vim]
            [paravim.session :as session]
            [paravim.constants :as constants]
            [libvim-clj.core :as v]
            [play-cljc.gl.core :as pc]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [odoyle.rules :as o])
  (:import  [org.lwjgl.glfw GLFW Callbacks
             GLFWCursorPosCallbackI GLFWKeyCallbackI GLFWMouseButtonCallbackI
             GLFWCharCallbackI GLFWFramebufferSizeCallbackI GLFWWindowCloseCallbackI
             GLFWScrollCallbackI]
            [org.lwjgl.opengl GL GL33]
            [org.lwjgl.system MemoryUtil])
  (:gen-class))

(defn- get-density-ratio [window]
  (let [*fb-width (MemoryUtil/memAllocInt 1)
        *window-width (MemoryUtil/memAllocInt 1)
        _ (GLFW/glfwGetFramebufferSize window *fb-width nil)
        _ (GLFW/glfwGetWindowSize window *window-width nil)
        fb-width (.get *fb-width)
        window-width (.get *window-width)]
    (MemoryUtil/memFree *fb-width)
    (MemoryUtil/memFree *window-width)
    (/ fb-width window-width)))

(def ^:private keycode->keyword
  {GLFW/GLFW_KEY_BACKSPACE :backspace
   GLFW/GLFW_KEY_DELETE :delete
   GLFW/GLFW_KEY_TAB :tab
   GLFW/GLFW_KEY_ENTER :enter
   GLFW/GLFW_KEY_ESCAPE :escape
   GLFW/GLFW_KEY_UP :up
   GLFW/GLFW_KEY_DOWN :down
   GLFW/GLFW_KEY_LEFT :left
   GLFW/GLFW_KEY_RIGHT :right
   GLFW/GLFW_KEY_HOME :home
   GLFW/GLFW_KEY_END :end
   GLFW/GLFW_KEY_PAGE_UP :page-up
   GLFW/GLFW_KEY_PAGE_DOWN :page-down
   GLFW/GLFW_KEY_GRAVE_ACCENT :backtick
   GLFW/GLFW_KEY_F :f
   GLFW/GLFW_KEY_C :c
   GLFW/GLFW_KEY_V :v
   GLFW/GLFW_KEY_MINUS :-
   GLFW/GLFW_KEY_EQUAL :=})

(def ^:private keycode->char
  {GLFW/GLFW_KEY_D \D
   GLFW/GLFW_KEY_H \H
   GLFW/GLFW_KEY_J \J
   GLFW/GLFW_KEY_M \M
   GLFW/GLFW_KEY_P \P
   GLFW/GLFW_KEY_R \R
   GLFW/GLFW_KEY_U \U})

(def ^:private control-keycode?
  #{GLFW/GLFW_KEY_LEFT_CONTROL GLFW/GLFW_KEY_RIGHT_CONTROL})

;; https://vim.fandom.com/wiki/Mapping_keys_in_Vim_-_Tutorial_%28Part_2%29

(def ^:private keyword->name
  {:backspace "BS"
   :delete "Del"
   :tab "Tab"
   :enter "Enter"
   :escape "Esc"
   :up "Up"
   :down "Down"
   :page-up "PageUp"
   :page-down "PageDown"
   :left "Left"
   :right "Right"
   :home "Home"
   :end "End"})

(defn on-mouse-move! [{:keys [::density-ratio] :as game} window xpos ypos]
  (let [x (* xpos density-ratio)
        y (* ypos density-ratio)
        session (swap! session/*session
                       (fn [session]
                         (-> session
                             (c/update-mouse x y)
                             ;; fire rules will make the mouse hover rules run,
                             ;; so the `get-mouse` call will have the correct data
                             o/fire-rules)))
        mouse (session/get-mouse session)]
    (GLFW/glfwSetCursor window
                        (GLFW/glfwCreateStandardCursor
                          (case (:cursor mouse)
                            :ibeam GLFW/GLFW_IBEAM_CURSOR
                            :hand GLFW/GLFW_HAND_CURSOR
                            GLFW/GLFW_ARROW_CURSOR)))))

(defn on-mouse-click! [game window button action mods]
  (when (and (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (= action GLFW/GLFW_PRESS))
    (c/click-mouse! game @session/*session :left)))

(defn on-key! [{:keys [::vim/vim ::pipes] :as game} window keycode scancode action mods]
  (let [control? (not= 0 (bit-and mods GLFW/GLFW_MOD_CONTROL))
        ;alt? (not= 0 (bit-and mods GLFW/GLFW_MOD_ALT))
        shift? (not= 0 (bit-and mods GLFW/GLFW_MOD_SHIFT))
        press? (= action GLFW/GLFW_PRESS)
        repeat? (= action GLFW/GLFW_REPEAT)
        release? (= action GLFW/GLFW_RELEASE)
        control-key? (control-keycode? keycode)]
    (when (or press? release?)
      (swap! session/*session o/insert
             ::session/vim
             ::session/control? (boolean
                                  (or (and control? (not control-key?))
                                      (and press? control-key?)))))
    (when (or press? repeat?)
      (when-let [session @session/*session]
        (let [{:keys [mode]} (session/get-vim session)
              k (keycode->keyword keycode)
              current-tab (:id (session/get-current-tab session))
              current-buffer (:buffer-id (session/get-current-buffer session))]
          (cond
            press?
            (cond
              ;; pressing enter in the repl
              (and (= current-tab ::constants/repl-in)
                   (= k :enter)
                   (= mode 'NORMAL))
              (vim/repl-enter! vim session pipes)
              ;; all ctrl shortcuts
              control?
              (case k
                (:tab :backtick)
                (c/shift-current-tab! game session (if shift? -1 1))
                :f (repl/reload-file! (session/get-buffer session current-buffer) pipes current-tab)
                :- (swap! session/*session c/font-dec)
                := (swap! session/*session c/font-inc)
                :v (if (= mode 'INSERT)
                     (let [text (GLFW/glfwGetClipboardString window)]
                       (vim/on-bulk-input vim text false))
                     (vim/on-input vim session "<C-V>"))
                ; else
                (when-let [key-name (if k
                                      (keyword->name k)
                                      (keycode->char keycode))]
                  (vim/on-input vim session (str "<C-" (when shift? "S-") key-name ">"))))
              ;; all other input
              :else
              (when-let [key-name (keyword->name k)]
                (vim/on-input vim session (str "<" key-name ">"))))
            repeat?
            (when-let [key-name (keyword->name k)]
              (vim/on-input vim session (str "<" key-name ">")))))))))

(defn on-char! [{:keys [::vim/vim] :as game} window codepoint]
  (vim/on-input vim @session/*session (str (char codepoint))))

(defn on-resize! [game window width height]
  (swap! session/*session c/update-window-size width height))

(defn on-scroll! [{:keys [::density-ratio] :as game} window xoffset yoffset]
  (c/scroll! @session/*session (/ xoffset density-ratio) (/ yoffset density-ratio)))

(defn- listen-for-events [game window]
  (doto window
    (GLFW/glfwSetCursorPosCallback
      (reify GLFWCursorPosCallbackI
        (invoke [this window xpos ypos]
          (on-mouse-move! game window xpos ypos))))
    (GLFW/glfwSetMouseButtonCallback
      (reify GLFWMouseButtonCallbackI
        (invoke [this window button action mods]
          (on-mouse-click! game window button action mods))))
    (GLFW/glfwSetKeyCallback
      (reify GLFWKeyCallbackI
        (invoke [this window keycode scancode action mods]
          (on-key! game window keycode scancode action mods))))
    (GLFW/glfwSetCharCallback
      (reify GLFWCharCallbackI
        (invoke [this window codepoint]
          (on-char! game window codepoint))))
    (GLFW/glfwSetFramebufferSizeCallback
      (reify GLFWFramebufferSizeCallbackI
        (invoke [this window width height]
          (on-resize! game window width height))))
    (GLFW/glfwSetScrollCallback
      (reify GLFWScrollCallbackI
        (invoke [this window xoffset yoffset]
          (on-scroll! game window xoffset yoffset))))
    (GLFW/glfwSetWindowCloseCallback
      (reify GLFWWindowCloseCallbackI
        (invoke [this window]
          (System/exit 0))))))

(defn- poll-input! [{:keys [context ::vim/vim ::command-chan ::single-command-chan ::repl-output] :as game}]
  (or
    (if-let [input (or (async/poll! command-chan)
                       (async/poll! single-command-chan))]
      (case (first input)
        :append (assoc game ::repl-output (conj repl-output (second input)))
        :new-buf (let [id-or-path (second input)]
                   (cond
                     (integer? id-or-path)
                     (v/set-current-buffer vim id-or-path)
                     (string? id-or-path)
                     (v/open-buffer vim id-or-path))
                   nil)
        :set-window-title (GLFW/glfwSetWindowTitle context (second input))
        :set-clipboard-string (GLFW/glfwSetClipboardString context (second input))
        :resize-window (vim/update-window-size! game)
        :move-cursor (apply vim/update-cursor-position! vim (second input))
        :update-vim (do
                      (swap! session/*session
                             (fn [session]
                               (-> session
                                   (o/insert ::session/vim (second input))
                                   (c/insert-buffer-refresh (:buffer-id (session/get-current-buffer session))))))
                      nil))
      (when-let [session @session/*session] ;; this could be momentarily nil while hot code reloading
        (when (vim/ready-to-append? session vim repl-output)
          (binding [vim/*update-ui?* false]
            (vim/append-to-buffer! game session (first repl-output)))
          (assoc game ::repl-output (vec (rest repl-output))))))
    game))

(defn ->window []
  (when-not (GLFW/glfwInit)
    (throw (Exception. "Unable to initialize GLFW")))
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL33/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_TRANSPARENT_FRAMEBUFFER GLFW/GLFW_TRUE)
  (if-let [window (GLFW/glfwCreateWindow 1024 768 "Paravim" 0 0)]
    (do
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GL/createCapabilities)
      window)
    (throw (Exception. "Failed to create window"))))

(defn init
  ([game]
   (init game (vim/->vim) (async/chan)))
  ([game vim command-chan]
   (let [pipes (repl/create-pipes)
         density-ratio (get-density-ratio (:context game))
         game (assoc game
                ::density-ratio density-ratio
                ::pipes pipes
                ::poll-input! poll-input!
                ::command-chan command-chan
                ;; single-command-chan is like command-chan but can only contain one command
                ::single-command-chan (async/chan (async/sliding-buffer 1))
                ::repl-output []
                ::vim/vim vim
                ::vim/init vim/init)]
     (c/init game)
     (swap! session/*session c/font-multiply density-ratio)
     (when-not (::disable-repl? game)
       (repl/start-repl-thread! nil pipes #(async/put! command-chan [:append %])))
     ;; load init file
     (when-let [path (not-empty (System/getProperty "paravim.init"))]
       (println "Loading Paravim init file:" path)
       (let [path (if (.startsWith path "~")
                    (.getCanonicalPath (io/file (System/getProperty "user.home") (subs path 1)))
                    path)]
         (load-file path)))
     game)))

(defn- start [game window]
  (let [game (assoc game
               :delta-time 0
               :total-time (GLFW/glfwGetTime)
               ::c/clear? true)
        game (init game)]
    (GLFW/glfwShowWindow window)
    (listen-for-events game window)
    (loop [game game]
      (when-not (GLFW/glfwWindowShouldClose window)
        (let [ts (GLFW/glfwGetTime)
              game (assoc game
                          :delta-time (- ts (:total-time game))
                          :total-time ts)
              game (c/tick game)]
          (GLFW/glfwSwapBuffers window)
          (GLFW/glfwPollEvents)
          (recur game))))
    (Callbacks/glfwFreeCallbacks window)
    (GLFW/glfwDestroyWindow window)
    (GLFW/glfwTerminate)))

(defn -main [& args]
  (let [window (->window)]
    (start (pc/->game window) window)))
