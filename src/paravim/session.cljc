(ns paravim.session
  (:require [paravim.buffers :as buffers]
            [paravim.scroll :as scroll]
            [paravim.constants :as constants]
            [paravim.minimap :as minimap]
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
(defrecord MouseHover [target cursor mouse-anchor])
(defrecord TextBox [id left right top bottom])
(defrecord BoundingBox [id x1 y1 x2 y2 align])
(defrecord Font [size])
(defrecord Vim [mode ascii control? show-search?
                visual-range highlights message
                command-start command-text command-completion
                command-text-entity command-cursor-entity])
(defrecord CurrentTab [id])
(defrecord Tab [id buffer-id])
(defrecord Buffer [id tab-id
                   text-entity parinfer-text-entity rects-entity
                   parsed-code needs-parinfer? needs-parinfer-init? needs-clojure-refresh?
                   camera camera-x camera-y camera-target-x camera-target-y total-time-anchor
                   scroll-speed-x scroll-speed-y
                   path file-name
                   lines clojure?
                   cursor-line cursor-column
                   font-anchor window-anchor show-minimap?])
(defrecord Minimap [buffer-id show? text-entity rects-entity anchor])
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
  '{:paravim.session/get-game
    (fn []
      (let [game paravim.session.Game]
        game))
    :paravim.session/get-window
    (fn []
      (let [window paravim.session.Window]
        window))
    :paravim.session/get-mouse
    (fn []
      (let [mouse paravim.session.Mouse]
        mouse))
    :paravim.session/get-current-tab
    (fn []
      (let [current-tab paravim.session.CurrentTab]
        current-tab))
    :paravim.session/get-current-buffer
    (fn []
      (let [current-tab paravim.session.CurrentTab
            tab paravim.session.Tab
            :when (= (:id tab) (:id current-tab))]
        (:buffer-id tab)))
    :paravim.session/get-tab
    (fn [?id]
      (let [tab paravim.session.Tab
            :when (= (:id tab) ?id)]
        tab))
    :paravim.session/get-mouse-hover
    (fn []
      (let [mouse-hover paravim.session.MouseHover]
        mouse-hover))
    :paravim.session/get-font
    (fn []
      (let [font paravim.session.Font]
        font))
    :paravim.session/get-vim
    (fn []
      (let [vim paravim.session.Vim]
        vim))
    :paravim.session/get-bounding-box
    (fn [?id]
      (let [bounding-box paravim.session.BoundingBox
            :when (= (:id bounding-box) ?id)]
        bounding-box))
    :paravim.session/get-text-box
    (fn [?id]
      (let [text-box paravim.session.TextBox
            :when (= (:id text-box) ?id)]
        text-box))
    :paravim.session/get-buffer
    (fn [?id]
      (let [buffer paravim.session.Buffer
            :when (= (:id buffer) ?id)]
        buffer))
    :paravim.session/get-minimap
    (fn [?id]
      (let [minimap paravim.session.Minimap
            :when (= (:buffer-id minimap) ?id)]
        minimap))
    :paravim.session/get-constants
    (fn []
      (let [constants paravim.session.Constants]
        constants))})

(def rules
  '{:paravim.session/mouse-hovers-over-text
    (let [window paravim.session.Window
          mouse paravim.session.Mouse
          mouse-hover paravim.session.MouseHover
          :when (not= mouse (:mouse-anchor mouse-hover))
          current-tab paravim.session.CurrentTab
          :when (not= nil (:id current-tab))
          font paravim.session.Font
          {:keys [id left right top bottom] :as text-box} paravim.session.TextBox
          :when (and (= id (:id current-tab))
                     (<= left (:x mouse) (- (:width window) right))
                     (<= (top (:height window) (:size font))
                         (:y mouse)
                         (bottom (:height window) (:size font))))]
      (clarax.rules/merge! mouse-hover {:target :text
                                        :cursor :ibeam
                                        :mouse-anchor mouse}))
    :paravim.session/mouse-hovers-over-bounding-box
    (let [window paravim.session.Window
          mouse paravim.session.Mouse
          mouse-hover paravim.session.MouseHover
          :when (not= mouse (:mouse-anchor mouse-hover))
          font paravim.session.Font
          {:keys [x1 y1 x2 y2 align] :as bounding-box} paravim.session.BoundingBox
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
      (clarax.rules/merge! mouse-hover {:target (:id bounding-box)
                                        :cursor :hand
                                        :mouse-anchor mouse}))
    :paravim.session/update-buffer-when-font-changes
    (let [game paravim.session.Game
          window paravim.session.Window
          font paravim.session.Font
          buffer paravim.session.Buffer
          :when (and (not= font (:font-anchor buffer))
                     ;; ignore ascii buffers
                     (number? (:id buffer)))
          vim paravim.session.Vim
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants paravim.session.Constants]
      (clarax.rules/merge! buffer
        (-> buffer
            (paravim.buffers/update-cursor (:mode vim) (:size font) text-box constants window)
            (paravim.buffers/update-highlight constants)
            (paravim.buffers/update-selection constants (:visual-range vim))
            (paravim.buffers/update-search-highlights constants vim)
            (assoc :font-anchor font)))
      (clojure.core.async/put! (:paravim.core/single-command-chan game) [:resize-window]))
    :paravim.session/update-buffer-when-window-resizes
    (let [game paravim.session.Game
          window paravim.session.Window
          font paravim.session.Font
          current-tab paravim.session.CurrentTab
          tab paravim.session.Tab
          :when (= (:id tab) (:id current-tab))
          buffer paravim.session.Buffer
          :when (and (not= window (:window-anchor buffer))
                     (or (= (:id buffer) (:buffer-id tab))
                         ;; if we're in the repl, make sure both the input and output are refreshed
                         (= (:tab-id buffer) (case (:id current-tab)
                                               :repl-in :repl-out
                                               :repl-out :repl-in
                                               nil))))
          vim paravim.session.Vim
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants paravim.session.Constants]
      (clarax.rules/merge! buffer
        (-> buffer
            (paravim.buffers/update-cursor (:mode vim) (:size font) text-box constants window)
            (paravim.buffers/update-highlight constants)
            (paravim.buffers/update-selection constants (:visual-range vim))
            (paravim.buffers/update-search-highlights constants vim)
            (assoc :window-anchor window)))
      (clojure.core.async/put! (:paravim.core/single-command-chan game) [:resize-window]))
    :paravim.session/update-minimap
    (let [window paravim.session.Window
          font paravim.session.Font
          buffer paravim.session.Buffer
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants paravim.session.Constants
          minimap paravim.session.Minimap
          :when (and (= (:buffer-id minimap) (:id buffer))
                     (not= [window font buffer] (:anchor minimap)))]
      (let [new-minimap (paravim.minimap/->minimap buffer constants (:size font) (:width window) (:height window) text-box)
            new-buffer (assoc buffer :show-minimap? (:show? new-minimap))]
        (when (not= buffer new-buffer)
          (clarax.rules/merge! buffer new-buffer))
        (clarax.rules/merge! minimap
          (assoc new-minimap
                 ;; this prevents the rule from firing if none of these three things have changed
                 :anchor [window font new-buffer]))))
    :paravim.session/rubber-band-effect
    (let [window paravim.session.Window
          font paravim.session.Font
          buffer paravim.session.Buffer
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants paravim.session.Constants]
      (some->> (paravim.scroll/rubber-band-camera buffer (:size font) text-box constants window)
               (clarax.rules/merge! buffer)))
    :paravim.session/move-camera-to-target
    (let [{:keys [delta-time total-time] :as game} paravim.session.Game
          window paravim.session.Window
          font paravim.session.Font
          {:keys [camera-x camera-y camera-target-x camera-target-y total-time-anchor] :as buffer} paravim.session.Buffer
          :when (and (not= total-time total-time-anchor)
                     (or (not (== camera-x camera-target-x))
                         (not (== camera-y camera-target-y))))
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))]
      (clarax.rules/merge! buffer
        (assoc (paravim.scroll/animate-camera buffer (:size font) text-box window delta-time)
          :total-time-anchor total-time)))})

#?(:clj (defmacro ->session-wrapper []
          (list '->session (merge queries rules))))

(def *initial-session (atom (->session-wrapper)))
(defonce *session (atom nil))
(defonce *reload? (atom false))

;; when this ns is reloaded, reload the session
(when @*session
  (reset! *reload? true))

#?(:clj (defmacro merge-into-session [rules-and-queries]
          `(do
             (reset! *initial-session (->session ~(merge queries rules rules-and-queries)))
             (reset! *reload? true)
             nil)))

(defn def-queries [session]
  (let [query-fns (clarax/query-fns session)]
    (def get-game (:paravim.session/get-game query-fns))
    (def get-window (:paravim.session/get-window query-fns))
    (def get-mouse (:paravim.session/get-mouse query-fns))
    (def get-mouse-hover (:paravim.session/get-mouse-hover query-fns))
    (def get-font (:paravim.session/get-font query-fns))
    (def get-vim (:paravim.session/get-vim query-fns))
    (def get-current-tab (:paravim.session/get-current-tab query-fns))
    (def get-current-buffer (:paravim.session/get-current-buffer query-fns))
    (def get-tab (:paravim.session/get-tab query-fns))
    (def get-bounding-box (:paravim.session/get-bounding-box query-fns))
    (def get-text-box (:paravim.session/get-text-box query-fns))
    (def get-buffer (:paravim.session/get-buffer query-fns))
    (def get-minimap (:paravim.session/get-minimap query-fns))
    (def get-constants (:paravim.session/get-constants query-fns))))

