(ns paravim.start
  (:require [paravim.core :as c]
            [libvim-clj.core :as v]
            [clojure.string :as str]
            [play-cljc.gl.core :as pc]
            [parinferish.core :as par])
  (:import  [org.lwjgl.glfw GLFW Callbacks GLFWCursorPosCallbackI GLFWKeyCallbackI GLFWCharCallbackI]
            [org.lwjgl.opengl GL GL41]
            [org.lwjgl.system MemoryUtil]
            [javax.sound.sampled AudioSystem Clip])
  (:gen-class))

(defn listen-for-mouse [window]
  (GLFW/glfwSetCursorPosCallback window
    (reify GLFWCursorPosCallbackI
      (invoke [this window xpos ypos]
        (swap! c/*state
          (fn [state]
            (let [*fb-width (MemoryUtil/memAllocInt 1)
                  *fb-height (MemoryUtil/memAllocInt 1)
                  *window-width (MemoryUtil/memAllocInt 1)
                  *window-height (MemoryUtil/memAllocInt 1)
                  _ (GLFW/glfwGetFramebufferSize window *fb-width *fb-height)
                  _ (GLFW/glfwGetWindowSize window *window-width *window-height)
                  fb-width (.get *fb-width)
                  fb-height (.get *fb-height)
                  window-width (.get *window-width)
                  window-height (.get *window-height)
                  width-ratio (/ fb-width window-width)
                  height-ratio (/ fb-height window-height)
                  x (* xpos width-ratio)
                  y (* ypos height-ratio)]
              (MemoryUtil/memFree *fb-width)
              (MemoryUtil/memFree *fb-height)
              (MemoryUtil/memFree *window-width)
              (MemoryUtil/memFree *window-height)
              (assoc state :mouse-x x :mouse-y y))))))))

(defn keycode->keyword [keycode]
  (condp = keycode
    GLFW/GLFW_KEY_BACKSPACE :backspace
    GLFW/GLFW_KEY_TAB :tab
    GLFW/GLFW_KEY_ENTER :enter
    GLFW/GLFW_KEY_ESCAPE :escape
    GLFW/GLFW_KEY_UP :up
    GLFW/GLFW_KEY_DOWN :down
    GLFW/GLFW_KEY_LEFT :left
    GLFW/GLFW_KEY_RIGHT :right
    nil))

(defn listen-for-keys [window callback]
  (GLFW/glfwSetKeyCallback window
    (reify GLFWKeyCallbackI
      (invoke [this window keycode scancode action mods]
        (when-let [k (keycode->keyword keycode)]
          (when (= action GLFW/GLFW_PRESS)
            (when-let [key-name (v/keyword->name k)]
              (callback key-name))))))))

(defn listen-for-chars [window callback]
  (GLFW/glfwSetCharCallback window
    (reify GLFWCharCallbackI
      (invoke [this window codepoint]
        (callback (str (char codepoint)))))))

(defn update-buffers [state game]
  (if-let [updates (not-empty (:buffer-updates state))]
    (-> (reduce
          (fn [state {:keys [buffer-ptr lines first-line line-count-change]}]
            (c/modify-buffer state game buffer-ptr lines first-line line-count-change))
          state
          updates)
        (assoc :buffer-updates []))
    state))

(defn apply-parinfer! [{:keys [mode] :as state} vim game buffer-ptr]
  (let [{:keys [parsed-code needs-parinfer?]} (get-in state [:buffers buffer-ptr])]
    (when needs-parinfer?
      (let [cursor-line (v/get-cursor-line vim)
            cursor-column (v/get-cursor-column vim)
            diffs (par/diff parsed-code)]
        (when (seq diffs)
          (when (not= 'INSERT mode)
            (v/input vim "i"))
          (doseq [{:keys [line column content action]} diffs]
            (v/set-cursor-position vim (inc line) column)
            (doseq [ch (seq content)]
              (case action
                :remove
                (v/input vim "<Del>")
                :insert
                (v/input vim (str ch)))))
          (v/set-cursor-position vim cursor-line cursor-column)
          (when (not= 'INSERT mode)
            (v/input vim "<Esc>"))))
      (swap! c/*state
        (fn [state]
          (-> state
              (assoc-in [:buffers buffer-ptr :needs-parinfer?] false)
              (update-buffers game)))))))

(defn -main [& args]
  (when-not (GLFW/glfwInit)
    (throw (Exception. "Unable to initialize GLFW")))
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 4)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 1)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL41/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_TRANSPARENT_FRAMEBUFFER GLFW/GLFW_TRUE)
  (if-let [window (GLFW/glfwCreateWindow 1024 768 "Paravim" 0 0)]
    (do
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)
      (GL/createCapabilities)
      (listen-for-mouse window)
      (let [initial-game (assoc (pc/->game window)
                                :delta-time 0
                                :total-time 0)
            vim (doto (v/->vim)
                  v/init
                  (v/execute "set hidden")
                  (v/execute "set noswapfile")
                  (v/execute "set nobackup")
                  (v/execute "set nowritebackup")
                  (v/execute "set tabstop=2")
                  (v/execute "set softtabstop=2")
                  (v/execute "set shiftwidth=2")
                  (v/execute "set expandtab")
                  (v/execute "filetype plugin indent on"))
            on-input (fn [s]
                       (let [{:keys [mode command-text command-text-entity current-buffer]} @c/*state]
                         (when (and (= 'INSERT mode) (= s "<Esc>"))
                           (-> (swap! c/*state update-buffers initial-game)
                               (apply-parinfer! vim initial-game current-buffer)))
                         (if (and (= mode 'COMMAND_LINE) command-text)
                           (let [pos (v/get-command-position vim)]
                             (case s
                               "<Tab>"
                               (when (= (count command-text) pos)
                                 (when-let [completion-text (v/get-command-completion vim)]
                                   (when-let [first-part (str/last-index-of command-text " ")]
                                     (dotimes [_ (- (count command-text) (inc first-part))]
                                       (v/input vim "<BS>"))
                                     (doseq [ch completion-text]
                                       (v/input vim (str ch))))))
                               ("<Right>" "<Left>" "<Up>" "<Down>")
                               nil
                               (v/input vim s)))
                           (v/input vim s))
                         (let [current-buffer (v/get-current-buffer vim)
                               mode (v/get-mode vim)
                               cursor-line (dec (v/get-cursor-line vim))
                               cursor-column (v/get-cursor-column vim)]
                           (cond-> (swap! c/*state
                                     (fn [state]
                                       (-> state
                                           (assoc :mode mode)
                                           (c/update-command (v/get-command-text vim) (v/get-command-position vim))
                                           (update-in [:buffers current-buffer] assoc :cursor-line cursor-line :cursor-column cursor-column)
                                           (update-buffers initial-game)
                                           (c/update-cursor initial-game current-buffer)
                                           (c/update-highlight current-buffer)
                                           (cond-> (v/visual-active? vim)
                                                   (c/update-selection current-buffer (-> (v/get-visual-range vim)
                                                                                          (update :start-line dec)
                                                                                          (update :end-line dec)))))))
                                   (and (not= 'INSERT mode)
                                        (not= s "u"))
                                   (apply-parinfer! vim initial-game current-buffer)))))]
        (listen-for-keys window on-input)
        (listen-for-chars window on-input)
        (v/set-on-quit vim (fn [buffer-ptr force?]
                             (System/exit 0)))
        (v/set-on-auto-command vim (fn [buffer-ptr event]
                                     (case event
                                       EVENT_BUFENTER
                                       (let [cursor-line (dec (v/get-cursor-line vim))
                                             cursor-column (v/get-cursor-column vim)]
                                         (swap! c/*state
                                           (fn [state]
                                             (-> state
                                                 (assoc :current-buffer buffer-ptr)
                                                 (cond-> (nil? (get-in state [:buffers buffer-ptr]))
                                                         (-> (c/assoc-buffer buffer-ptr
                                                               (v/get-file-name vim buffer-ptr)
                                                               (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                                                                      (v/get-line vim buffer-ptr (inc i)))))
                                                             (update-in [:buffers buffer-ptr] assoc :cursor-line cursor-line :cursor-column cursor-column)
                                                             (c/update-cursor initial-game buffer-ptr)))))))
                                       nil)))
        (v/set-on-buffer-update vim (fn [buffer-ptr start-line end-line line-count-change]
                                      (let [first-line (dec start-line)
                                            last-line (+ (dec end-line) line-count-change)
                                            lines (vec (for [i (range first-line last-line)]
                                                         (v/get-line vim buffer-ptr (inc i))))]
                                        (swap! c/*state update :buffer-updates conj {:buffer-ptr buffer-ptr
                                                                                     :lines lines
                                                                                     :first-line first-line
                                                                                     :line-count-change line-count-change}))))
        (c/init initial-game (fn []
                               (v/open-buffer vim "deps.edn")))
        (loop [game initial-game]
          (when-not (GLFW/glfwWindowShouldClose window)
            (let [ts (GLFW/glfwGetTime)
                  game (assoc game
                              :delta-time (- ts (:total-time game))
                              :total-time ts)
                  game (c/tick game)]
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur game)))))
      (Callbacks/glfwFreeCallbacks window)
      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwTerminate))
    (throw (Exception. "Failed to create window"))))

