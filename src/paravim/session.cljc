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
  #?(:cljs (:require-macros [paravim.session :refer [merge-into-session]])))

(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Mouse [x y])
(defrecord MouseHover [target cursor mouse-anchor])
(defrecord TextBox [id left right top bottom])
(defrecord BoundingBox [id x1 y1 x2 y2 align])
(defrecord Font [size])
(defrecord FontMultiplier [size])
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
                   font-anchor window-anchor show-minimap?
                   last-update])
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
  '{::get-game
    (fn []
      (let [game paravim.session.Game]
        game))
    ::get-window
    (fn []
      (let [window paravim.session.Window]
        window))
    ::get-mouse
    (fn []
      (let [mouse paravim.session.Mouse]
        mouse))
    ::get-current-tab
    (fn []
      (let [current-tab paravim.session.CurrentTab]
        current-tab))
    ::get-current-buffer
    (fn []
      (let [current-tab paravim.session.CurrentTab
            tab paravim.session.Tab
            :when (= (:id tab) (:id current-tab))]
        (:buffer-id tab)))
    ::get-tab
    (fn [?id]
      (let [tab paravim.session.Tab
            :when (= (:id tab) ?id)]
        tab))
    ::get-mouse-hover
    (fn []
      (let [mouse-hover paravim.session.MouseHover]
        mouse-hover))
    ::get-font
    (fn []
      (let [font paravim.session.Font]
        font))
    ::get-font-multiplier
    (fn []
      (let [font paravim.session.FontMultiplier]
        font))
    ::get-vim
    (fn []
      (let [vim paravim.session.Vim]
        vim))
    ::get-bounding-box
    (fn [?id]
      (let [bounding-box paravim.session.BoundingBox
            :when (= (:id bounding-box) ?id)]
        bounding-box))
    ::get-text-box
    (fn [?id]
      (let [text-box paravim.session.TextBox
            :when (= (:id text-box) ?id)]
        text-box))
    ::get-buffer
    (fn [?id]
      (let [buffer paravim.session.Buffer
            :when (= (:id buffer) ?id)]
        buffer))
    ::get-minimap
    (fn [?id]
      (let [minimap paravim.session.Minimap
            :when (= (:buffer-id minimap) ?id)]
        minimap))
    ::get-constants
    (fn []
      (let [constants paravim.session.Constants]
        constants))})

(def rules
  '{::mouse-hovers-over-text
    (let [window paravim.session.Window
          mouse paravim.session.Mouse
          mouse-hover paravim.session.MouseHover
          :when (not= mouse (:mouse-anchor mouse-hover))
          current-tab paravim.session.CurrentTab
          :when (not= nil (:id current-tab))
          font paravim.session.FontMultiplier
          {:keys [id left right top bottom] :as text-box} paravim.session.TextBox
          :when (and (= id (:id current-tab))
                     (<= left (:x mouse) (- (:width window) right))
                     (<= (top (:height window) (:size font))
                         (:y mouse)
                         (bottom (:height window) (:size font))))]
      (clarax.rules/merge! mouse-hover {:target :text
                                        :cursor :ibeam
                                        :mouse-anchor mouse}))
    ::mouse-hovers-over-bounding-box
    (let [window paravim.session.Window
          mouse paravim.session.Mouse
          mouse-hover paravim.session.MouseHover
          :when (not= mouse (:mouse-anchor mouse-hover))
          font paravim.session.FontMultiplier
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
    ::update-buffer-when-font-changes
    (let [game paravim.session.Game
          window paravim.session.Window
          font paravim.session.FontMultiplier
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
    ::update-buffer-when-window-resizes
    (let [game paravim.session.Game
          window paravim.session.Window
          font paravim.session.FontMultiplier
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
    ::minimap
    (let [game paravim.session.Game
          :when (:total-time game)
          window paravim.session.Window
          font paravim.session.FontMultiplier
          {:keys [last-update] :as buffer} paravim.session.Buffer
          ;; wait until the next tick after the buffer was updated
          ;; this makes it a lot faster, i think the buffer record
          ;; is updated multiple times per tick, so this condition
          ;; prevents this from running extra times.
          ;; the better long-term solution is to break up the
          ;; buffer record so we don't trigger rules more than necessary
          :when (and (some? last-update)
                     (> (:total-time game) last-update))
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
    ::rubber-band-effect
    (let [window paravim.session.Window
          font paravim.session.FontMultiplier
          buffer paravim.session.Buffer
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))
          constants paravim.session.Constants]
      (some->> (paravim.scroll/rubber-band-camera buffer (:size font) text-box constants window)
               (clarax.rules/merge! buffer)))
    ::move-camera-to-target
    (let [{:keys [delta-time total-time] :as game} paravim.session.Game
          window paravim.session.Window
          font paravim.session.FontMultiplier
          {:keys [camera-x camera-y camera-target-x camera-target-y total-time-anchor] :as buffer} paravim.session.Buffer
          :when (and (not= total-time total-time-anchor)
                     (or (not (== camera-x camera-target-x))
                         (not (== camera-y camera-target-y))))
          text-box paravim.session.TextBox
          :when (= (:id text-box) (:tab-id buffer))]
      (clarax.rules/merge! buffer
        (assoc (paravim.scroll/animate-camera buffer (:size font) text-box window delta-time)
          :total-time-anchor total-time)))
    ::font
    (let [font paravim.session.Font
          font-multiplier paravim.session.FontMultiplier
          constants paravim.session.Constants]
      (cond
        ;; font needs to be initialized
        (== 0 (:size font))
        (clarax.rules/merge! font {:size (* (:size font-multiplier) (:font-height constants))})
        ;; change the multiplier to match the font size
        (pos? (:size font))
        (clarax.rules/merge! font-multiplier {:size (/ (:size font) (:font-height constants))})))})

(defonce *initial-session (atom nil))
(defonce *session (atom nil))
(defonce *reload? (atom false))

#?(:clj (defmacro merge-into-session [& args]
          `(do
             (reset! *initial-session (->session ~(->> (apply merge queries rules args)
                                                       ;; remove nil rules (this allows people to disable rules)
                                                       (filter second)
                                                       (into {}))))
             ;; reload the session if it's been created already
             (when @*session
               (reset! *reload? true))
             nil)))

;; create initial session
(merge-into-session)

(defn def-queries [session]
  (let [query-fns (clarax/query-fns session)]
    (def get-game (::get-game query-fns))
    (def get-window (::get-window query-fns))
    (def get-mouse (::get-mouse query-fns))
    (def get-mouse-hover (::get-mouse-hover query-fns))
    (def get-font (::get-font query-fns))
    (def get-font-multiplier (::get-font-multiplier query-fns))
    (def get-vim (::get-vim query-fns))
    (def get-current-tab (::get-current-tab query-fns))
    (def get-current-buffer (::get-current-buffer query-fns))
    (def get-tab (::get-tab query-fns))
    (def get-bounding-box (::get-bounding-box query-fns))
    (def get-text-box (::get-text-box query-fns))
    (def get-buffer (::get-buffer query-fns))
    (def get-minimap (::get-minimap query-fns))
    (def get-constants (::get-constants query-fns))))

