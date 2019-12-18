(ns paravim.start
  (:require [paravim.core :as c]
            [paravim.repl :as repl]
            [paravim.vim :as vim]
            [paravim.utils :as utils]
            [paravim.session :as session]
            [play-cljc.gl.core :as pc]
            [clojure.core.async :as async])
  (:import  [org.lwjgl.glfw GLFW Callbacks
             GLFWCursorPosCallbackI GLFWKeyCallbackI GLFWMouseButtonCallbackI
             GLFWCharCallbackI GLFWWindowSizeCallbackI]
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

(defn on-mouse-move! [game window xpos ypos]
  (let [density-ratio (float (get-density-ratio window))
        x (* xpos density-ratio)
        y (* ypos density-ratio)
        session (swap! session/*session c/update-mouse x y)
        mouse-hover (c/get-mouse-hover session)]
    (swap! session/*session c/update-state assoc
           :mouse-hover (:target mouse-hover)
           :mouse-type (:cursor mouse-hover))
    (GLFW/glfwSetCursor window
                        (GLFW/glfwCreateStandardCursor
                          (case (:cursor mouse-hover)
                            :ibeam GLFW/GLFW_IBEAM_CURSOR
                            :hand GLFW/GLFW_HAND_CURSOR
                            GLFW/GLFW_ARROW_CURSOR)))))

(defn on-mouse-click! [{:keys [::c/vim ::c/pipes ::c/send-input!] :as game} window button action mods]
  (when (and (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (= action GLFW/GLFW_PRESS))
    (swap! session/*session
           (fn [session]
             (-> session
                 (c/click-mouse :left)
                 (c/update-state c/update-cursor-if-necessary (c/get-constants session) game))))))

(defn on-key! [{:keys [::c/vim ::c/pipes ::c/send-input!] :as game} window keycode scancode action mods]
  (let [control? (not= 0 (bit-and mods GLFW/GLFW_MOD_CONTROL))
        ;alt? (not= 0 (bit-and mods GLFW/GLFW_MOD_ALT))
        shift? (not= 0 (bit-and mods GLFW/GLFW_MOD_SHIFT))
        press? (= action GLFW/GLFW_PRESS)
        release? (= action GLFW/GLFW_RELEASE)
        control-key? (control-keycode? keycode)]
    (when (or press? release?)
      (swap! session/*session c/update-state assoc :control?
                       (or (and control? (not control-key?))
                           (and press? control-key?))))
    (when press?
      (let [session @session/*session
            {:keys [mode] :as state} (c/get-state session)
            k (keycode->keyword keycode)
            current-tab (:id (c/get-current-tab session))
            current-buffer (:id (c/get-current-buffer session))]
        (cond
          ;; pressing enter in the repl
          (and (= current-tab :repl-in)
               (= k :enter)
               (= mode 'NORMAL))
          (vim/repl-enter! vim send-input! pipes)
          ;; all ctrl shortcuts
          control?
          (case k
            (:tab :backtick)
            (swap! session/*session c/shift-current-tab (if shift? -1 1))
            :f (session/reload-file! state pipes current-tab current-buffer)
            :- (swap! session/*session
                      (fn [session]
                        (-> session
                            c/font-dec
                            (c/update-state c/update-cursor-if-necessary (c/get-constants session) game))))
            := (swap! session/*session
                      (fn [session]
                        (-> session
                            c/font-inc
                            (c/update-state c/update-cursor-if-necessary (c/get-constants session) game))))
            ; else
            (when-let [key-name (if k
                                  (keyword->name k)
                                  (keycode->char keycode))]
              (send-input! (str "<C-" (when shift? "S-") key-name ">"))))
          ;; all other input
          :else
          (when-let [key-name (keyword->name k)]
            (send-input! (str "<" key-name ">"))))))))

(defn on-char! [{:keys [::c/send-input!] :as game} window codepoint]
  (send-input! (str (char codepoint))))

(defn on-resize! [{:keys [::c/send-input!] :as game} window width height]
  (swap! session/*session
         (fn [session]
           (-> session
               (c/update-window-size width height)
               (c/update-state
                 (fn [state]
                   (let [session @session/*session
                         current-tab (:id (c/get-current-tab session))
                         current-buffer (:id (c/get-current-buffer session))]
                     (as-> state state
                           (if current-buffer
                             (update-in state [:buffers current-buffer] c/update-cursor state (c/get-constants session) game)
                             state)
                           ;; if we're in the repl, make sure both the input and output are refreshed
                           (if-let [other-tab (case current-tab
                                                :repl-in :repl-out
                                                :repl-out :repl-in
                                                nil)]
                             (let [other-buffer (:buffer-id (c/get-tab session {:?id other-tab}))]
                               (update-in state [:buffers other-buffer] c/update-cursor state (c/get-constants session) game))
                             state))))))))
  (send-input! [:resize]))

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
    (GLFW/glfwSetWindowSizeCallback
      (reify GLFWWindowSizeCallbackI
        (invoke [this window width height]
          (on-resize! game window width height))))))

(defn- poll-input! [{:keys [::c/vim ::c/vim-chan ::c/append-repl-chan ::c/repl-output] :as game}]
  (loop [vim-input nil
         repl-output repl-output]
    (cond
      ;; process input from either chan
      vim-input
      (if (string? vim-input)
        (do
          (vim/on-input game vim vim-input)
          (recur nil repl-output))
        (case (first vim-input)
          :append (recur nil (conj repl-output (second vim-input)))
          :new-tab (do
                     (vim/open-buffer-for-tab! vim @session/*session)
                     (async/put! vim-chan [:resize])
                     (recur nil repl-output))
          :resize (let [width (utils/get-width game)
                        height (utils/get-height game)]
                    (vim/set-window-size! vim @session/*session width height)
                    (recur nil repl-output))))
      ;; append to the repl
      (vim/ready-to-append? vim repl-output)
      (do
        (binding [vim/*update-ui?* false]
          (vim/append-to-buffer! game vim (first repl-output)))
        (recur nil []))
      ;; poll chans
      :else
      (if-let [input (or (async/poll! vim-chan)
                         (async/poll! append-repl-chan))]
        (recur input repl-output)
        (assoc game ::c/repl-output repl-output)))))

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
                       ;; only used in tests
                       (fn [x]
                         (when (string? x)
                           (vim/on-input game vim x))))
         poll-input! (if vim-chan
                       poll-input!
                       ;; only used in tests
                       identity)
         append-repl-chan (when vim-chan (async/chan))
         pipes (repl/create-pipes)
         density-ratio (get-density-ratio (:context game))
         game (assoc game
                ::c/pipes pipes
                ::c/send-input! send-input!
                ::c/poll-input! poll-input!
                ::c/vim vim
                ::c/vim-chan vim-chan
                ::c/append-repl-chan append-repl-chan
                ::c/repl-output [])]
     (when (pos-int? density-ratio)
       (swap! session/*session c/font-multiply density-ratio))
     (c/init game)
     (vim/init vim game)
     (when vim-chan
       (async/put! vim-chan [:resize])
       (repl/start-repl-thread! nil pipes #(async/put! append-repl-chan [:append %])))
     game)))

(defn- start [game window]
  (let [game (assoc game :delta-time 0 :total-time 0 ::c/clear? true)
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

