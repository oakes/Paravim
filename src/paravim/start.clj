(ns paravim.start
  (:require [paravim.core :as c]
            [paravim.vim :as v]
            [clojure.string :as str]
            [play-cljc.gl.core :as pc])
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
                  v/init)
            on-input (fn [s]
                       (let [{:keys [command-text command-text-entity]} @c/*state]
                         (if (and command-text
                                  command-text-entity
                                  (= s "<Tab>"))
                           (when (= (count command-text) (v/get-command-position vim))
                             (when-let [completion-text (v/get-command-completion vim)]
                               (when-let [last-part-count (some->> (str/last-index-of command-text " ")
                                                                   inc
                                                                   (subs command-text)
                                                                   count)]
                                 (when (> (count completion-text) last-part-count)
                                   (run! #(v/input vim (str %))
                                     (subs completion-text last-part-count))))))
                           (v/input vim s)))
                       (swap! c/*state
                         (fn [state]
                           (-> state
                               (c/update-command (v/get-command-text vim) (v/get-command-position vim))
                               (c/update-cursor
                                 initial-game
                                 (v/get-current-buffer vim)
                                 (dec (v/get-cursor-line vim))
                                 (v/get-cursor-column vim))))))]
        (listen-for-keys window on-input)
        (listen-for-chars window on-input)
        (v/set-on-auto-command vim (fn [buffer-ptr event]
                                     (case event
                                       EVENT_BUFENTER
                                       (swap! c/*state
                                         (fn [state]
                                           (-> state
                                               (assoc :current-buffer buffer-ptr)
                                               (cond-> (nil? (get-in state [:buffers buffer-ptr]))
                                                       (-> (c/assoc-buffer buffer-ptr
                                                             (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                                                                    (v/get-line vim buffer-ptr (inc i)))))
                                                           (c/update-cursor initial-game buffer-ptr
                                                             (dec (v/get-cursor-line vim))
                                                             (v/get-cursor-column vim)))))))
                                       nil)))
        (v/set-on-buffer-update vim (fn [buffer-ptr start-line end-line line-count-change]
                                      (swap! c/*state
                                        (fn [state]
                                          (let [first-line (dec start-line)
                                                last-line (+ (dec end-line) line-count-change)
                                                lines (vec (for [i (range first-line last-line)]
                                                             (v/get-line vim buffer-ptr (inc i))))]
                                            (c/modify-buffer state initial-game buffer-ptr lines first-line line-count-change))))))
        (c/init initial-game (fn []
                               (v/open-buffer vim "resources/public/index.html")))
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

