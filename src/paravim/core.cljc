(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [paravim.session :as session]
            [paravim.colors :as colors]
            [paravim.buffers :as buffers]
            [paravim.constants :as constants]
            [paravim.scroll :as scroll]
            #?(:clj [paravim.repl :refer [reload-file!]])
            [odoyle.rules :as o]
            [clojure.string :as str]
            [clojure.set :as set]
            [play-cljc.gl.utils :as u]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.text :as text]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.primitives-2d :as primitives]
            [clojure.core.rrb-vector :as rrb]
            [clojure.core.async :as async]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-font-cljs]])))

(defn update-mouse [session x y]
  (-> session
      (o/insert ::session/mouse {::session/x x
                                 ::session/y y
                                 ::session/target nil
                                 ::session/cursor nil})))

(defn update-window-size [session width height]
  (o/insert session ::session/window {::session/width width ::session/height height}))

(defn update-current-tab [session id]
  (o/insert session ::session/tab ::session/current id))

(defn new-tab! [command-chan session tab-id]
  (let [buffer-id-or-path (or (:buffer-id (session/get-tab session tab-id))
                              ;; if no buffer exists, open a new one
                              (get constants/tab->path tab-id))]
    (async/put! command-chan [:new-buf buffer-id-or-path])))

(defn shift-current-tab! [game session direction]
  (let [current-tab (session/get-current-tab session)
        id (:id current-tab)
        index (+ (.indexOf constants/tab-ids id)
                 direction)
        index (cond
                (neg? index) (dec (count constants/tab-ids))
                (= index (count constants/tab-ids)) 0
                :else index)]
    (new-tab! (:paravim.start/command-chan game) session (nth constants/tab-ids index))))

(defn update-tab [session id buffer-id]
  (o/insert session id ::session/buffer-id buffer-id))

(defn insert-buffer-update [session bu]
  (if-let [buffer (session/get-buffer session (:buffer-id bu))]
    (let [constants (session/get-constants session)
          new-buffer (-> buffer
                         (buffers/update-text-buffer constants (:lines bu) (:first-line bu) (:line-count-change bu))
                         (assoc :needs-clojure-refresh? (:clojure? buffer)))]
      (session/insert session (:buffer-id bu) new-buffer))
    session))

(defn insert-buffer-refresh [session buffer-id]
  (if-let [buffer (session/get-buffer session buffer-id)]
    (let [constants (session/get-constants session)
          vim (session/get-vim session)
          font-multiplier (:multiplier (session/get-font session))
          text-box (session/get-text-box session (:tab-id buffer))
          window (session/get-window session)
          new-buffer (as-> buffer $
                           (if (:needs-clojure-refresh? buffer)
                             (-> $
                                 (buffers/parse-clojure-buffer (:mode vim))
                                 (buffers/update-clojure-buffer constants)
                                 (assoc :needs-clojure-refresh? false))
                             $)
                           (buffers/update-cursor $ (:mode vim) font-multiplier text-box constants window)
                           (buffers/update-highlight $ constants)
                           (buffers/update-selection $ constants (:visual-range vim))
                           (buffers/update-search-highlights $ constants (:show-search? vim) (:highlights vim)))]
      (session/insert session buffer-id new-buffer))
    session))

(defn change-font-size [session diff-multiplier]
  (let [font (session/get-font session)
        font-height (:font-height (session/get-constants session))
        diff (* diff-multiplier font-height)
        min-font-size (* constants/min-font-size font-height)
        max-font-size (* constants/max-font-size font-height)
        curr-val (:size font)
        new-val (+ curr-val diff)]
    (if (or (<= min-font-size new-val max-font-size)
            ;; the user went outside of the normal font size range
            ;; via their init file, so let them change it
            (< curr-val min-font-size)
            (> curr-val max-font-size))
      (o/insert session ::session/font ::session/size new-val)
      session)))

(defn font-dec [session]
  (change-font-size session (- constants/font-size-step)))

(defn font-inc [session]
  (change-font-size session constants/font-size-step))

(defn font-multiply [session n]
  (let [font-size (:multiplier (session/get-font session))]
    (change-font-size session (- (* font-size n) font-size))))

(defn remove-buffer [session buffer-id]
  (cond->> []
           (session/get-buffer session buffer-id)
           (into [::session/tab-id
                  ::session/text-entity
                  ::session/parinfer-text-entity
                  ::session/rects-entity
                  ::session/parsed-code
                  ::session/needs-parinfer?
                  ::session/needs-parinfer-init?
                  ::session/needs-clojure-refresh?
                  ::session/camera
                  ::session/camera-x
                  ::session/camera-y
                  ::session/camera-target-x
                  ::session/camera-target-y
                  ::session/scroll-speed-x
                  ::session/scroll-speed-y
                  ::session/path
                  ::session/file-name
                  ::session/lines
                  ::session/clojure?
                  ::session/cursor-line
                  ::session/cursor-column
                  ::session/show-minimap?])
           (session/get-minimap session buffer-id)
           (into [:paravim.minimap/show?
                  :paravim.minimap/rects-entity
                  :paravim.minimap/text-entity])
           true
           (reduce
             (fn [session attr]
               (o/retract session buffer-id attr))
             session)))

(defn- mouse->cursor-position [buffer mouse font-size-multiplier text-box constants window]
  (let [text-top ((:top text-box) (:height window) font-size-multiplier)
        {:keys [x y]} mouse
        {:keys [camera-x camera-y]} buffer
        column (-> (+ x camera-x)
                   (/ (* font-size-multiplier (:font-width constants)))
                   long)
        line (-> (+ y camera-y)
                 (- text-top)
                 (/ (* font-size-multiplier (:font-height constants)))
                 long)]
    [line column]))

(defn click-mouse! [game session button]
  (let [mouse (session/get-mouse session)
        current-tab (session/get-current-tab session)
        buffer-id (:buffer-id (session/get-current-buffer session))
        buffer (session/get-buffer session buffer-id)]
    (when (= :left button)
      (let [{:keys [target]} mouse]
        (if (constants/tab? target)
          (new-tab! (:paravim.start/command-chan game) session target)
          (case target
            ::constants/font-dec (swap! session/*session font-dec)
            ::constants/font-inc (swap! session/*session font-inc)
            ::constants/reload-file (when (and buffer (reload-file! buffer (:paravim.start/pipes game) (:id current-tab)))
                                      (new-tab! (:paravim.start/command-chan game) session ::constants/repl-in))
            :text (when buffer
                    (let [text-box (session/get-text-box session (:id current-tab))
                          window (session/get-window session)
                          constants (session/get-constants session)
                          font-multiplier (:multiplier (session/get-font session))]
                      (async/put! (:paravim.start/command-chan game)
                                  [:move-cursor (mouse->cursor-position buffer mouse font-multiplier text-box constants window)])))
            nil))))))

(defn scroll! [session xoffset yoffset]
  (let [current-tab (session/get-current-tab session)
        tab (session/get-tab session (:id current-tab))
        buffer-id (:buffer-id (session/get-current-buffer session))
        buffer (session/get-buffer session buffer-id)]
    (when buffer
      (swap! session/*session session/insert buffer-id (scroll/start-scrolling-camera buffer xoffset yoffset)))))

(defn get-mode []
  (:mode (session/get-vim @session/*session)))

(defn command-text [text completion]
  {::session/command-text text
   ::session/command-completion (when (some-> text str/trim seq)
                                  completion)})

(defn assoc-command-entity [command text-entity cursor-entity]
  (assoc command ::session/command-text-entity text-entity ::session/command-cursor-entity cursor-entity))

(defn assoc-command [{:keys [::session/command-start ::session/command-text ::session/command-completion] :as command}
                     {:keys [base-text-entity base-font-entity base-rects-entity font-height] :as constants}
                     vim-mode font-size-multiplier position]
  (if command-text
    (let [char-entities (mapv #(-> base-font-entity
                                   (chars/crop-char %)
                                   (t/color colors/bg-color))
                          (str command-start command-text))
          completion-entities (when command-completion
                                (mapv #(-> base-font-entity
                                           (chars/crop-char %)
                                           (t/color (colors/set-alpha colors/bg-color colors/completion-alpha)))
                                    (str command-start
                                         (some->> (str/index-of command-text " ") inc (subs command-text 0))
                                         command-completion)))
          char-count (count char-entities)
          comp-count (count completion-entities)
          char-entities (if (> comp-count char-count)
                          (into char-entities (subvec completion-entities char-count))
                          char-entities)
          command-text-entity (-> (chars/assoc-line base-text-entity 0 char-entities)
                                  (chars/update-uniforms font-height colors/text-alpha))
          line-chars (get-in command-text-entity [:characters 0])
          command-cursor-entity (i/assoc base-rects-entity 0 (buffers/->cursor-entity vim-mode constants line-chars 0 (inc position) font-size-multiplier))]
      (assoc-command-entity command command-text-entity command-cursor-entity))
    (assoc-command-entity command nil nil)))

(defn assoc-attr-lengths [text-entity]
  (reduce
    (fn [text-entity attr-name]
      (let [type-name (u/get-attribute-type text-entity attr-name)
            {:keys [size iter]} (merge u/default-opts (u/type->attribute-opts type-name))]
        (assoc-in text-entity [:attribute-lengths attr-name]
                  (* size iter))))
    text-entity
    (keys chars/instanced-font-attrs->unis)))

(defn init-entities [game callback]
  (let [base-rect-entity (e/->entity game primitives/rect)
        base-rects-entity (c/compile game (i/->instanced-entity base-rect-entity))]
    ;; load fonts
    (#?(:clj load-font-clj :cljs load-font-cljs) :firacode
       (fn [{:keys [data]} baked-font]
         (let [base-font-entity (-> (text/->font-entity game data baked-font)
                                    (t/color colors/text-color))
               base-text-entity (-> (i/->instanced-entity base-font-entity)
                                    (assoc :vertex chars/instanced-font-vertex-shader
                                           :fragment chars/instanced-font-fragment-shader
                                           :characters [])
                                    assoc-attr-lengths)
               base-text-entity (c/compile game base-text-entity)
               font-width (-> baked-font :baked-chars (nth (- 115 (:first-char baked-font))) :xadv)
               font-height (:font-height baked-font)
               snap-to-top (fn [game-height multiplier] (* font-height multiplier))
               snap-to-bottom (fn [game-height multiplier] (- game-height (* font-height multiplier)))
               repl-in-top (fn [game-height multiplier] (- game-height (* constants/repl-in-lines font-height multiplier)))
               repl-out-bottom (fn [game-height multiplier] (- game-height (* (inc constants/repl-in-lines) font-height multiplier)))
               text-boxes {::constants/files {:left 0 :right 0 :top snap-to-top :bottom snap-to-bottom}
                           ::constants/repl-in {:left 0 :right 0 :top repl-in-top :bottom snap-to-bottom}
                           ::constants/repl-out {:left 0 :right 0 :top snap-to-top :bottom repl-out-bottom}}]
           (#?(:clj load-font-clj :cljs load-font-cljs) :roboto
            (fn [{:keys [data]} baked-font]
              (let [roboto-font-entity (-> (text/->font-entity game data baked-font)
                                           (t/color colors/text-color))
                    roboto-text-entity (-> (i/->instanced-entity roboto-font-entity)
                                           (assoc :vertex chars/instanced-font-vertex-shader
                                                  :fragment chars/instanced-font-fragment-shader
                                                  :characters [])
                                           assoc-attr-lengths)
                    roboto-text-entity (c/compile game roboto-text-entity)
                    tab-spacing (* font-width 2)
                    tab-entities (reduce
                                   (fn [m {:keys [id text]}]
                                     (assoc m id (buffers/assoc-lines roboto-text-entity roboto-font-entity font-height [text])))
                                   {}
                                   constants/tabs)
                    bounding-boxes (reduce-kv
                                     (fn [m i {:keys [id]}]
                                       (let [last-tab (some->> (get constants/tabs (dec i)) :id (get m))
                                             left (if last-tab (+ (:x2 last-tab) tab-spacing) 0)
                                             right (-> tab-entities (get id) :characters first last :x-total (+ left))]
                                         (assoc m id {:x1 left :y1 0 :x2 right :y2 font-height :align :left})))
                                     {}
                                     constants/tabs)
                    button-entities (reduce
                                      (fn [m {:keys [id text]}]
                                        (assoc m id (buffers/assoc-lines roboto-text-entity roboto-font-entity font-height [text])))
                                      {}
                                      constants/buttons)
                    bounding-boxes (reduce-kv
                                     (fn [m i {:keys [id]}]
                                       (let [last-button (some->> (get constants/buttons (dec i)) :id (get m))
                                             right (if last-button (+ (:x1 last-button) tab-spacing) 0)
                                             left (-> button-entities (get id) :characters first last :x-total (+ right))]
                                        (assoc m id {:x1 left :y1 0 :x2 right :y2 font-height :align :right})))
                                     bounding-boxes
                                     constants/buttons)
                    ;; when the user holds down control, we want to show the shortcut character
                    ;; for each button in a different color so they can learn the shortcut
                    highlight-button-entities (reduce
                                                (fn [m {:keys [id text shortcut-char]}]
                                                  (let [character (get-in m [id :characters 0 shortcut-char])]
                                                    (update m id i/assoc shortcut-char
                                                            (-> character
                                                                (chars/update-translation-matrix (:left character) 0)
                                                                (t/color colors/yellow-color)))))
                                                button-entities
                                                constants/buttons)]
                (callback
                  {:constants {::session/base-rect-entity base-rect-entity
                               ::session/base-rects-entity base-rects-entity
                               ::session/font-width font-width
                               ::session/font-height font-height
                               ::session/base-font-entity base-font-entity
                               ::session/base-text-entity base-text-entity
                               ::session/roboto-font-entity roboto-font-entity
                               ::session/roboto-text-entity roboto-text-entity
                               ::session/toolbar-text-entities (merge tab-entities button-entities)
                               ::session/highlight-text-entities highlight-button-entities}
                   :text-boxes text-boxes
                   :bounding-boxes bounding-boxes})))))))))

;; cache the entities so they aren't recaculated after reloading the session
(defonce ^:private *entity-cache (atom nil))

;; store the game map (this is a fix for a problem in old versions of the play-cljc template)
(defonce ^:private *game (atom nil))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; save game map
  (when-not @*game
    (reset! *game game))
  ;; initialize session
  (reset! session/*session
    (-> @session/*initial-session
        (o/insert ::session/global (select-keys game [:paravim.start/command-chan
                                                      :paravim.start/single-command-chan]))
        (o/insert ::session/window {::session/width (utils/get-width game)
                                    ::session/height (utils/get-height game)})))
  ;; initialize entities
  (let [callback (fn [{:keys [constants text-boxes bounding-boxes] :as entities}]
                   (reset! *entity-cache entities)
                   (swap! session/*session
                          (fn [session]
                            (as-> session $
                                  (o/insert $ ::session/constant constants)
                                  (reduce-kv session/insert $ text-boxes)
                                  (reduce-kv session/insert $ bounding-boxes)))))]
    (if-let [entities @*entity-cache]
      (callback entities)
      (init-entities game callback)))
  ;; fire rules
  (swap! session/*session o/fire-rules)
  ;; init vim
  ;; this should never be nil, but it could be after
  ;; hot code reloading if this function isn't given the
  ;; game map created in paravim.start
  (when-let [init-vim (:paravim.vim/init game)]
    (init-vim game))
  ;; return game map
  game)

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color colors/bg-color :depth 1}})

(defn render-buffer [game session constants font-size-multiplier game-width game-height current-tab buffer-ptr show-cursor? show-minimap?]
  (when-let [{:keys [rects-entity text-entity parinfer-text-entity camera]
              :as buffer} (session/get-buffer session buffer-ptr)]
    (when-let [text-box (session/get-text-box session current-tab)]
      (let [text-top ((:top text-box) game-height font-size-multiplier)]
        (when (and rects-entity show-cursor?)
          (c/render game (-> rects-entity
                             (t/project game-width game-height)
                             (t/invert camera)
                             (t/translate 0 text-top)
                             (t/scale font-size-multiplier font-size-multiplier))))
        (let [[lines-to-skip-count lines-to-crop-count] (buffers/get-visible-lines buffer constants text-box game-height font-size-multiplier)]
          (when parinfer-text-entity
            (c/render game (-> parinfer-text-entity
                               (buffers/crop-text-entity lines-to-skip-count lines-to-crop-count)
                               (t/project game-width game-height)
                               (t/invert camera)
                               (t/translate 0 text-top)
                               (t/scale font-size-multiplier font-size-multiplier))))
          (c/render game (-> text-entity
                             (buffers/crop-text-entity lines-to-skip-count lines-to-crop-count)
                             (cond-> (not show-cursor?)
                                     (assoc-in [:uniforms 'u_alpha] colors/unfocused-alpha))
                             (t/project game-width game-height)
                             (t/invert camera)
                             (t/translate 0 text-top)
                             (t/scale font-size-multiplier font-size-multiplier)))
          (when (and show-minimap? (:show-minimap? buffer))
            (when-let [minimap (session/get-minimap session buffer-ptr)]
              (c/render game (:rects-entity minimap))
              (c/render game (-> (:text-entity minimap)
                                 (cond-> (not show-cursor?)
                                         (assoc-in [:uniforms 'u_alpha] colors/unfocused-alpha)))))))))))

(defn tick [game]
  (when @session/*reload?
    (reset! session/*reload? false)
    (init game))
  (let [session (if-let [delta (:delta-time game)]
                  (swap! session/*session
                    (fn [session]
                      (-> session
                          (o/insert ::session/time ::session/delta delta)
                          o/fire-rules)))
                  @session/*session)
        current-tab (:id (session/get-current-tab session))
        current-buffer (:buffer-id (session/get-current-buffer session))
        buffer (session/get-buffer session current-buffer)
        font-size-multiplier (:multiplier (session/get-font session))
        {game-width :width game-height :height :as window} (session/get-window session)
        {:keys [base-rect-entity base-rects-entity
                base-text-entity base-font-entity
                font-height
                toolbar-text-entities highlight-text-entities]
         :as constants} (session/get-constants session)
        {:keys [mode ascii control?
                command-text-entity command-cursor-entity]
         :as vim} (session/get-vim session)]
    (when (and window (pos? game-width) (pos? game-height))
      (if (::clear? game)
        (c/render game (update screen-entity :viewport assoc :width game-width :height game-height))
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color colors/bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width game-height))))))
      (if (and ascii (= current-tab ::constants/files))
        (render-buffer game session constants font-size-multiplier game-width game-height current-tab ascii false false)
        (render-buffer game session constants font-size-multiplier game-width game-height current-tab current-buffer (not= mode 'COMMAND_LINE) (#{::constants/files ::constants/repl-out} current-tab)))
      (case current-tab
        ::constants/repl-in (when-let [buffer-ptr (:buffer-id (session/get-tab session ::constants/repl-out))]
                              (render-buffer game session constants font-size-multiplier game-width game-height ::constants/repl-out buffer-ptr false true))
        ::constants/repl-out (when-let [buffer-ptr (:buffer-id (session/get-tab session ::constants/repl-in))]
                               (render-buffer game session constants font-size-multiplier game-width game-height ::constants/repl-in buffer-ptr false false))
        nil)
      (when (and base-rects-entity base-rect-entity)
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color colors/bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width (* font-size-multiplier font-height))))
                           (i/assoc 1 (-> base-rect-entity
                                          (t/color (cond
                                                     (= 'COMMAND_LINE mode) colors/tan-color
                                                     (:message vim) colors/red-color
                                                     :else colors/bg-color))
                                          (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                                          (t/scale game-width (* font-size-multiplier font-height)))))))
      (doseq [[k entity] toolbar-text-entities
              :let [bounding-box (session/get-bounding-box session k)
                    highlight-entity (when control?
                                       (get highlight-text-entities k))]
              ;; hide the reload file button when necessary
              :when (or (not= k ::constants/reload-file)
                        (and (= current-tab ::constants/files)
                             (:clojure? buffer)))]
        (c/render game (-> (or highlight-entity entity)
                           (assoc-in [:uniforms 'u_alpha] (if (or (= k current-tab)
                                                                  highlight-entity)
                                                            colors/text-alpha
                                                            colors/unfocused-alpha))
                           (t/project game-width game-height)
                           (t/translate (-> bounding-box :x1 (* font-size-multiplier)
                                            (cond->> (= :right (:align bounding-box))
                                                     (- game-width)))
                                        (:y1 bounding-box))
                           (t/scale font-size-multiplier font-size-multiplier))))
      (cond
        (and (= mode 'COMMAND_LINE)
             command-text-entity
             command-cursor-entity)
        (do
          (c/render game (-> command-cursor-entity
                             (t/project game-width game-height)
                             (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                             (t/scale font-size-multiplier font-size-multiplier)))
          (c/render game (-> command-text-entity
                             (t/project game-width game-height)
                             (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                             (t/scale font-size-multiplier font-size-multiplier))))
        (:message vim)
        (c/render game (-> (chars/assoc-line base-text-entity 0
                             (mapv #(-> base-font-entity
                                        (chars/crop-char %)
                                        (t/color colors/text-color))
                               (:message vim)))
                           (chars/update-uniforms font-height colors/text-alpha)
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                           (t/scale font-size-multiplier font-size-multiplier)))))
    ;; update the game record
    (let [poll-input! (:paravim.start/poll-input! game)
          game (if poll-input!
                 (poll-input! game)
                 ;; if `game` has no poll input fn, use the one from the atom.
                 ;; this would only happen in old versions of the play-cljc template
                 ;; because it wasn't passing the latest game map to this function.
                 (let [poll-input!' (:paravim.start/poll-input! @*game)
                       game' (merge @*game game)]
                   (reset! *game (poll-input!' game'))))]
      ;; return new game record
      game)))

