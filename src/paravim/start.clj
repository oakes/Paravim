(ns paravim.start
  (:require [paravim.core :as c]
            [paravim.repl :as repl]
            [paravim.vim :as vim]
            [clojure.string :as str]
            [play-cljc.gl.core :as pc]
            [clojure.core.async :as async])
  (:import  [org.lwjgl.glfw GLFW Callbacks
             GLFWCursorPosCallbackI GLFWKeyCallbackI GLFWCharCallbackI
             GLFWMouseButtonCallbackI GLFWWindowSizeCallbackI]
            [org.lwjgl.opengl GL GL41]
            [org.lwjgl.system MemoryUtil]
            [javax.sound.sampled AudioSystem Clip])
  (:gen-class))

(defn listen-for-resize [window game]
  (GLFW/glfwSetWindowSizeCallback window
    (reify GLFWWindowSizeCallbackI
      (invoke [this window width height]
        (swap! c/*state
               (fn [{:keys [current-buffer current-tab tab->buffer] :as state}]
                 (as-> state state
                       (if current-buffer
                         (c/update-cursor state game current-buffer)
                         state)
                       ;; if we're in the repl, make sure both the input and output are refreshed
                       (if-let [other-tab (case current-tab
                                            :repl-in :repl-out
                                            :repl-out :repl-in
                                            nil)]
                         (c/update-cursor state game (tab->buffer other-tab))
                         state))))))))

(defn get-density-ratio [window]
  (let [*fb-width (MemoryUtil/memAllocInt 1)
        *window-width (MemoryUtil/memAllocInt 1)
        _ (GLFW/glfwGetFramebufferSize window *fb-width nil)
        _ (GLFW/glfwGetWindowSize window *window-width nil)
        fb-width (.get *fb-width)
        window-width (.get *window-width)]
    (MemoryUtil/memFree *fb-width)
    (MemoryUtil/memFree *window-width)
    (float (/ fb-width window-width))))

(defn listen-for-mouse [window game vim]
  (GLFW/glfwSetCursorPosCallback window
    (reify GLFWCursorPosCallbackI
      (invoke [this window xpos ypos]
        (as-> (swap! c/*state
                (fn [state]
                  (let [density-ratio (get-density-ratio window)
                        x (* xpos density-ratio)
                        y (* ypos density-ratio)]
                    (c/update-mouse state game x y))))
              state
              (GLFW/glfwSetCursor window
                                  (GLFW/glfwCreateStandardCursor
                                    (case (:mouse-type state)
                                      ;:ibeam GLFW/GLFW_IBEAM_CURSOR
                                      :hand GLFW/GLFW_HAND_CURSOR
                                      GLFW/GLFW_ARROW_CURSOR)))))))
  (GLFW/glfwSetMouseButtonCallback window
    (reify GLFWMouseButtonCallbackI
      (invoke [this window button action mods]
        (when (and (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
                   (= action GLFW/GLFW_PRESS))
          (vim/open-buffer-for-tab! vim (swap! c/*state c/click-mouse game)))))))

(def keycode->keyword
  {GLFW/GLFW_KEY_BACKSPACE :backspace
   GLFW/GLFW_KEY_DELETE :delete
   GLFW/GLFW_KEY_TAB :tab
   GLFW/GLFW_KEY_ENTER :enter
   GLFW/GLFW_KEY_ESCAPE :escape
   GLFW/GLFW_KEY_UP :up
   GLFW/GLFW_KEY_DOWN :down
   GLFW/GLFW_KEY_LEFT :left
   GLFW/GLFW_KEY_RIGHT :right})

(def keycode->char
  {GLFW/GLFW_KEY_D \D
   GLFW/GLFW_KEY_R \R
   GLFW/GLFW_KEY_U \U})

(defn listen-for-keys [window callback vim pipes]
  (GLFW/glfwSetKeyCallback window
    (reify GLFWKeyCallbackI
      (invoke [this window keycode scancode action mods]
        (when (= action GLFW/GLFW_PRESS)
          (let [control? (not= 0 (bit-and mods GLFW/GLFW_MOD_CONTROL))
                alt? (not= 0 (bit-and mods GLFW/GLFW_MOD_ALT))
                shift? (not= 0 (bit-and mods GLFW/GLFW_MOD_SHIFT))]
            (if-let [k (keycode->keyword keycode)]
              (cond
                (and (or control? alt?) (= k :tab))
                (vim/open-buffer-for-tab! vim (swap! c/*state c/change-tab (if shift? -1 1)))
                (and (= k :enter)
                     (vim/normal-mode? vim)
                     (= :repl-in (:current-tab @c/*state)))
                (vim/repl-enter! vim callback pipes)
                :else
                (when-let [key-name (vim/keyword->name k)]
                  (callback key-name)))
              (when control?
                (when-let [ch (keycode->char keycode)]
                  (callback (str "<C-" (when shift? "S-") ch ">")))))))))))

(defn listen-for-chars [window callback]
  (GLFW/glfwSetCharCallback window
    (reify GLFWCharCallbackI
      (invoke [this window codepoint]
        (callback (str (char codepoint)))))))

(defn poll-input [game vim c]
  (async/go-loop [delayed-inputs []]
    (if-let [[inputs-to-run inputs-to-delay] (vim/split-inputs vim delayed-inputs)]
      (do
        (binding [vim/*update-ui?* false]
          (doseq [input inputs-to-run]
            (vim/append-to-buffer! game vim input)))
        (recur inputs-to-delay))
      (let [input (async/<! c)]
        (if (string? input)
          (do
            (vim/on-input game vim input)
            (recur delayed-inputs))
          (recur (conj delayed-inputs input)))))))

(defn ->window []
  (when-not (GLFW/glfwInit)
    (throw (Exception. "Unable to initialize GLFW")))
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 4)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 1)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL41/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_TRANSPARENT_FRAMEBUFFER GLFW/GLFW_TRUE)
  (when-let [window (GLFW/glfwCreateWindow 1024 768 "Paravim" 0 0)]
    (GLFW/glfwMakeContextCurrent window)
    (GLFW/glfwSwapInterval 1)
    (GL/createCapabilities)
    window))

(defn ->game [window vim vim-chan]
  (let [initial-game (assoc (pc/->game window)
                            :delta-time 0
                            :total-time 0)
        send-input! (if vim-chan
                      (partial async/put! vim-chan)
                      #(vim/on-input initial-game vim %))
        pipes (repl/create-pipes)]
    (listen-for-resize window initial-game)
    (listen-for-mouse window initial-game vim)
    (listen-for-keys window send-input! vim pipes)
    (listen-for-chars window send-input!)
    (c/init initial-game)
    (when vim-chan
      (poll-input initial-game vim vim-chan))
    (vim/init vim (fn [buffer-ptr event]
                    (case event
                      EVENT_BUFENTER
                      (when vim/*update-ui?*
                        (vim/on-buf-enter initial-game vim buffer-ptr))
                      nil)))
    (when vim-chan
      (if-let [buffer (-> @c/*state :tab->buffer :repl-out)]
        (repl/start-repl-thread! nil pipes #(async/put! vim-chan {:buffer buffer :string %}))
        (throw (ex-info "REPL output buffer not found" {}))))
    initial-game))

(defn -main [& args]
  (if-let [window (->window)]
    (do
      (GLFW/glfwShowWindow window)
      (loop [game (->game window (vim/->vim) (async/chan))]
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
      (GLFW/glfwTerminate))
    (throw (Exception. "Failed to create window"))))

