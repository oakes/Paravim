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

(defn- get-density-ratio [window]
  (let [*fb-width (MemoryUtil/memAllocInt 1)
        *window-width (MemoryUtil/memAllocInt 1)
        _ (GLFW/glfwGetFramebufferSize window *fb-width nil)
        _ (GLFW/glfwGetWindowSize window *window-width nil)
        fb-width (.get *fb-width)
        window-width (.get *window-width)]
    (MemoryUtil/memFree *fb-width)
    (MemoryUtil/memFree *window-width)
    (float (/ fb-width window-width))))

(def ^:private keycode->keyword
  {GLFW/GLFW_KEY_BACKSPACE :backspace
   GLFW/GLFW_KEY_DELETE :delete
   GLFW/GLFW_KEY_TAB :tab
   GLFW/GLFW_KEY_ENTER :enter
   GLFW/GLFW_KEY_ESCAPE :escape
   GLFW/GLFW_KEY_UP :up
   GLFW/GLFW_KEY_DOWN :down
   GLFW/GLFW_KEY_LEFT :left
   GLFW/GLFW_KEY_RIGHT :right})

(def ^:private keycode->char
  {GLFW/GLFW_KEY_D \D
   GLFW/GLFW_KEY_R \R
   GLFW/GLFW_KEY_U \U})

(defn on-mouse-move! [{:keys [game]} window xpos ypos]
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
                                GLFW/GLFW_ARROW_CURSOR)))))

(defn on-mouse-click! [{:keys [vim pipes game]} window button action mods]
  (when (and (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (= action GLFW/GLFW_PRESS))
    (vim/open-buffer-for-tab! vim (swap! c/*state c/click-mouse game
                                         (fn [state]
                                           (let [{:keys [current-tab current-buffer buffers tab->buffer]} state
                                                 {:keys [out-pipe]} pipes
                                                 {:keys [lines file-name clojure?] :as buffer} (get buffers current-buffer)]
                                             (if (and clojure? (= current-tab :files))
                                               (do
                                                 (doto out-pipe
                                                   (.write (str "(do "
                                                                (pr-str '(println))
                                                                (pr-str (list 'println "Reloading" file-name))
                                                                (str/join \newline lines)
                                                                ")\n"))
                                                   .flush)
                                                 (assoc state :current-tab :repl-in))
                                               state)))))))

(defn on-key! [{:keys [vim pipes send-input!]} window keycode scancode action mods]
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
          (vim/repl-enter! vim send-input! pipes)
          :else
          (when-let [key-name (vim/keyword->name k)]
            (send-input! key-name)))
        (when control?
          (when-let [ch (keycode->char keycode)]
            (send-input! (str "<C-" (when shift? "S-") ch ">"))))))))

(defn on-resize! [{:keys [game]} window width height]
  (swap! c/*state
         (fn [{:keys [current-buffer current-tab tab->buffer] :as state}]
           (as-> state state
                 (if current-buffer
                   (update-in state [:buffers current-buffer] c/update-cursor state game)
                   state)
                 ;; if we're in the repl, make sure both the input and output are refreshed
                 (if-let [other-tab (case current-tab
                                      :repl-in :repl-out
                                      :repl-out :repl-in
                                      nil)]
                   (update-in state [:buffers (tab->buffer other-tab)] c/update-cursor state game)
                   state)))))

(defn on-char! [{:keys [send-input!]} window codepoint]
  (send-input! (str (char codepoint))))

(defn- listen-for-events [utils window]
  (doto window
    (GLFW/glfwSetWindowSizeCallback
      (reify GLFWWindowSizeCallbackI
        (invoke [this window width height]
          (on-resize! utils window width height))))
    (GLFW/glfwSetCursorPosCallback
      (reify GLFWCursorPosCallbackI
        (invoke [this window xpos ypos]
          (on-mouse-move! utils window xpos ypos))))
    (GLFW/glfwSetMouseButtonCallback
      (reify GLFWMouseButtonCallbackI
        (invoke [this window button action mods]
          (on-mouse-click! utils window button action mods))))
    (GLFW/glfwSetKeyCallback
      (reify GLFWKeyCallbackI
        (invoke [this window keycode scancode action mods]
          (on-key! utils window keycode scancode action mods))))
    (GLFW/glfwSetCharCallback
      (reify GLFWCharCallbackI
        (invoke [this window codepoint]
          (on-char! utils window codepoint))))))

(defn- poll-input [game vim c]
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
  ([game vim vim-chan]
   (let [send-input! (if vim-chan
                       (partial async/put! vim-chan)
                       #(vim/on-input game vim %))
         pipes (repl/create-pipes)]
     (c/init game)
     (when vim-chan
       (poll-input game vim vim-chan))
     (vim/init vim (fn [buffer-ptr event]
                     (case event
                       EVENT_BUFENTER
                       (when vim/*update-ui?*
                         (vim/on-buf-enter game vim buffer-ptr))
                       nil)))
     (when vim-chan
       (if-let [buffer (-> @c/*state :tab->buffer :repl-out)]
         (repl/start-repl-thread! nil pipes #(async/put! vim-chan {:buffer buffer :string %}))
         (throw (ex-info "REPL output buffer not found" {}))))
     {:pipes pipes
      :send-input! send-input!
      :vim vim
      :game game})))

(defn- start [game window]
  (let [game (assoc game :delta-time 0 :total-time 0 :clear? true)
        utils (init game)]
    (GLFW/glfwShowWindow window)
    (listen-for-events utils window)
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

