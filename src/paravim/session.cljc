(ns paravim.session
  (:require [paravim.buffers :as buffers]
            [paravim.scroll :as scroll]
            [paravim.constants :as constants]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            [clojure.string :as str]
            [clojure.core.async :as async]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [paravim.session :refer [->session-wrapper]])))

(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Mouse [x y])
(defrecord MouseHover [target cursor mouse])
(defrecord TextBox [id left right top bottom])
(defrecord BoundingBox [id x1 y1 x2 y2 align])
(defrecord Font [size])
(defrecord Vim [mode ascii control? show-search? command
                visual-range highlights message])
(defrecord Command [command-start command-text command-completion
                    command-text-entity command-cursor-entity])
(defrecord CurrentTab [id])
(defrecord Tab [id buffer-id])
(defrecord Buffer [id tab-id
                   text-entity parinfer-text-entity
                   parsed-code needs-parinfer? needs-parinfer-init? needs-clojure-refresh?
                   camera camera-x camera-y camera-target-x camera-target-y camera-animation-time
                   scroll-speed-x scroll-speed-y
                   path file-name
                   lines clojure?
                   cursor-line cursor-column
                   font window])
(defrecord Constants [base-rect-entity
                      base-rects-entity
                      font-width
                      font-height
                      base-font-entity
                      base-text-entity
                      roboto-font-entity
                      roboto-text-entity
                      toolbar-text-entities
                      highlight-text-entities])

(def queries
  '{:get-game
    (fn []
      (let [game Game]
        game))
    :get-window
    (fn []
      (let [window Window]
        window))
    :get-mouse
    (fn []
      (let [mouse Mouse]
        mouse))
    :get-current-tab
    (fn []
      (let [current-tab CurrentTab]
        current-tab))
    :get-current-buffer
    (fn []
      (let [current-tab CurrentTab
            tab Tab
            :when (= (:id tab) (:id current-tab))]
        (:buffer-id tab)))
    :get-tab
    (fn [?id]
      (let [tab Tab
            :when (= (:id tab) ?id)]
        tab))
    :get-mouse-hover
    (fn []
      (let [mouse-hover MouseHover]
        mouse-hover))
    :get-font
    (fn []
      (let [font Font]
        font))
    :get-vim
    (fn []
      (let [vim Vim]
        vim))
    :get-bounding-box
    (fn [?id]
      (let [bounding-box BoundingBox
            :when (= (:id bounding-box) ?id)]
        bounding-box))
    :get-text-box
    (fn [?id]
      (let [text-box TextBox
            :when (= (:id text-box) ?id)]
        text-box))
    :get-buffer
    (fn [?id]
      (let [buffer Buffer
            :when (= (:id buffer) ?id)]
        buffer))
    :get-constants
    (fn []
      (let [constants Constants]
        constants))
    :get-command
    (fn []
      (let [command Command]
        command))})

(def rules
  '{:mouse-hovers-over-text
    (let [window Window
          mouse Mouse
          mouse-hover MouseHover
          :when (not= mouse (:mouse mouse-hover))
          current-tab CurrentTab
          :when (not= nil (:id current-tab))
          font Font
          {:keys [id left right top bottom] :as text-box} TextBox
          :when (and (= id (:id current-tab))
                     (<= left (:x mouse) (- (:width window) right))
                     (<= (top (:height window) (:size font))
                         (:y mouse)
                         (bottom (:height window) (:size font))))]
      (clarax/merge! mouse-hover {:target :text
                                  :cursor :ibeam
                                  :mouse mouse}))
    :mouse-hovers-over-bounding-box
    (let [window Window
          mouse Mouse
          mouse-hover MouseHover
          :when (not= mouse (:mouse mouse-hover))
          font Font
          {:keys [x1 y1 x2 y2 align] :as bounding-box} BoundingBox
          :when (let [font-size (:size font)
                      game-width (:width window)
                      x1 (cond->> (* x1 font-size)
                                  (= :right align)
                                  (- game-width))
                      y1 (* y1 font-size)
                      x2 (cond->> (* x2 font-size)
                                  (= :right align)
                                  (- game-width))
                      y2 (* y2 font-size)]
                  (and (<= x1 (:x mouse) x2)
                       (<= y1 (:y mouse) y2)))]
      (clarax/merge! mouse-hover {:target (:id bounding-box)
                                  :cursor :hand
                                  :mouse mouse}))
    :update-cursor-when-font-changes
    (let [game Game
          window Window
          font Font
          buffer Buffer
          :when (and (not= font (:font buffer))
                     ;; ignore ascii buffers
                     (number? (:id buffer)))
          vim Vim
          text-box TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants Constants]
      (clarax/merge! buffer
        (-> buffer
            (buffers/update-cursor (:mode vim) (:size font) text-box constants window)
            (assoc :font font)))
      (async/put! (:paravim.core/single-command-chan game) [:resize-window]))
    :update-cursor-when-window-resizes
    (let [game Game
          window Window
          font Font
          current-tab CurrentTab
          tab Tab
          :when (= (:id tab) (:id current-tab))
          buffer Buffer
          :when (and (not= window (:window buffer))
                     (or (= (:id buffer) (:buffer-id tab))
                         ;; if we're in the repl, make sure both the input and output are refreshed
                         (= (:tab-id buffer) (case (:id current-tab)
                                               :repl-in :repl-out
                                               :repl-out :repl-in
                                               nil))))
          vim Vim
          text-box TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants Constants]
      (clarax/merge! buffer
        (-> buffer
            (buffers/update-cursor (:mode vim) (:size font) text-box constants window)
            (assoc :window window)))
      (async/put! (:paravim.core/single-command-chan game) [:resize-window]))
    :rubber-band-effect
    (let [window Window
          font Font
          {:keys [camera-target-x camera-target-y scroll-speed-x scroll-speed-y] :as buffer} Buffer
          text-box TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants Constants]
      (some->> (scroll/rubber-band-camera buffer (:size font) text-box constants window)
               (clarax/merge! buffer)))
    :move-camera-to-target
    (let [{:keys [delta-time total-time] :as game} Game
          window Window
          font Font
          {:keys [camera-x camera-y camera-target-x camera-target-y camera-animation-time] :as buffer} Buffer
          :when (and (not= total-time camera-animation-time)
                     (or (not (== camera-x camera-target-x))
                         (not (== camera-y camera-target-y))))
          text-box TextBox
          :when (= (:id text-box) (:tab-id buffer))]
      (clarax/merge! buffer
        (assoc (scroll/animate-camera buffer (:size font) text-box window delta-time)
          :camera-animation-time total-time)))
    :show-search-when-command-starts
    (let [command Command
          vim Vim
          :when (and (not= command (:command vim))
                     (#{"/" "?"} (:command-start command)))]
      (clarax/merge! vim {:show-search? true
                          :command command}))})

#?(:clj (defmacro ->session-wrapper []
          (list '->session (merge queries rules))))

(def *session
  (-> (->session-wrapper)
      (clara/insert
        (->Mouse 0 0)
        (->MouseHover nil nil nil)
        (->CurrentTab :files)
        (->Tab :files nil)
        (->Tab :repl-in nil)
        (->Tab :repl-out nil)
        (->Font (/ 1 4))
        (map->Vim {:mode 'NORMAL
                   :show-search? false})
        (map->Command {}))
      clara/fire-rules
      atom))

(let [query-fns (clarax/query-fns @*session)]
  (def get-game (:get-game query-fns))
  (def get-window (:get-window query-fns))
  (def get-mouse (:get-mouse query-fns))
  (def get-mouse-hover (:get-mouse-hover query-fns))
  (def get-font (:get-font query-fns))
  (def get-vim (:get-vim query-fns))
  (def get-current-tab (:get-current-tab query-fns))
  (def get-current-buffer (:get-current-buffer query-fns))
  (def get-tab (:get-tab query-fns))
  (def get-bounding-box (:get-bounding-box query-fns))
  (def get-text-box (:get-text-box query-fns))
  (def get-buffer (:get-buffer query-fns))
  (def get-constants (:get-constants query-fns))
  (def get-command (:get-command query-fns)))

