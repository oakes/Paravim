(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [paravim.session :as session]
            [paravim.colors :as colors]
            [paravim.buffers :as buffers]
            [paravim.constants :as constants]
            [paravim.scroll :as scroll]
            #?(:clj [paravim.repl :refer [reload-file!]])
            [clara.rules :as clara]
            [clarax.rules :as clarax]
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
      (clarax/merge (session/get-mouse-hover session) {:target nil :cursor nil})
      (clarax/merge (session/get-mouse session) {:x x :y y})
      clara/fire-rules))

(defn update-window-size [session width height]
  (-> session
      (clarax/merge (session/get-window session) {:width width :height height})
      clara/fire-rules))

(defn update-current-tab [session id]
  (-> session
      (clarax/merge (session/get-current-tab session) {:id id})
      clara/fire-rules))

(defn new-tab! [game session tab-id]
  (let [buffer-id-or-path (or (:buffer-id (session/get-tab session {:?id tab-id}))
                              ;; if no buffer exists, open a new one
                              (get constants/tab->path tab-id))]
    (async/put! (:paravim.core/command-chan game) [:new-buf buffer-id-or-path])))

(defn shift-current-tab! [game session direction]
  (let [current-tab (session/get-current-tab session)
        id (:id current-tab)
        index (+ (.indexOf constants/tab-ids id)
                 direction)
        index (cond
                (neg? index) (dec (count constants/tab-ids))
                (= index (count constants/tab-ids)) 0
                :else index)]
    (new-tab! game session (nth constants/tab-ids index))))

(defn update-tab [session id buffer-id]
  (-> session
      (clarax/merge (session/get-tab session {:?id id}) {:buffer-id buffer-id})
      clara/fire-rules))

(defn insert-buffer-update [session bu]
  (if-let [buffer (session/get-buffer session {:?id (:buffer-id bu)})]
    (let [constants (session/get-constants session)]
      (as-> buffer $
            (buffers/update-text-buffer $ constants (:lines bu) (:first-line bu) (:line-count-change bu))
            (assoc $ :needs-clojure-refresh? (:clojure? buffer))
            (clarax/merge session buffer $)
            (clara/fire-rules $)))
    session))

(defn insert-buffer-refresh [session buffer-id]
  (if-let [buffer (session/get-buffer session {:?id buffer-id})]
    (let [constants (session/get-constants session)
          vim (session/get-vim session)
          font (session/get-font session)
          text-box (session/get-text-box session {:?id (:tab-id buffer)})
          window (session/get-window session)]
      (as-> buffer $
            (if (:needs-clojure-refresh? buffer)
              (-> $
                  (buffers/parse-clojure-buffer (:mode vim))
                  (buffers/update-clojure-buffer constants)
                  (assoc :needs-clojure-refresh? false))
              $)
            (buffers/update-cursor $ (:mode vim) (:size font) text-box constants window)
            (buffers/update-highlight $ constants)
            (buffers/update-selection $ constants (:visual-range vim))
            (buffers/update-search-highlights $ constants vim)
            (clarax/merge session buffer $)
            (clara/fire-rules $)))
    session))

(defn change-font-size [session diff]
  (let [font (session/get-font session)
        curr-val (:size font)
        new-val (+ curr-val diff)]
    (if (<= constants/min-font-size new-val constants/max-font-size)
      (-> session
          (clarax/merge font {:size new-val})
          clara/fire-rules)
      session)))

(defn font-dec [session]
  (change-font-size session (- constants/font-size-step)))

(defn font-inc [session]
  (change-font-size session constants/font-size-step))

(defn font-multiply [session n]
  (let [font-size (:size (session/get-font session))]
    (change-font-size session (- (* font-size n) font-size))))

(defn upsert-buffer [session buffer]
  (if-let [existing-buffer (session/get-buffer session {:?id (:id buffer)})]
    (-> session
        (clarax/merge existing-buffer buffer)
        clara/fire-rules)
    (-> session
        (clara/insert (session/map->Buffer buffer))
        (clara/insert (session/map->Minimap {:buffer-id (:id buffer)}))
        clara/fire-rules)))

(defn remove-buffer [session buffer-id]
  (let [buffer (session/get-buffer session {:?id buffer-id})
        minimap (session/get-minimap session {:?id buffer-id})]
    (-> session
        (cond-> buffer (clara/retract buffer))
        (cond-> minimap (clara/retract minimap))
        clara/fire-rules)))

(defn update-vim [session m]
  (-> session
      (clarax/merge (session/get-vim session) m)
      clara/fire-rules))

(defn- mouse->cursor-position [buffer mouse font-size text-box constants window]
  (let [text-top ((:top text-box) (:height window) font-size)
        {:keys [x y]} mouse
        {:keys [camera-x camera-y]} buffer
        column (-> (+ x camera-x)
                   (/ (* font-size (:font-width constants)))
                   long)
        line (-> (+ y camera-y)
                 (- text-top)
                 (/ (* font-size (:font-height constants)))
                 long)]
    [line column]))

(defn click-mouse! [game session button]
  (let [mouse-hover (session/get-mouse-hover session)
        current-tab (session/get-current-tab session)
        buffer-id (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id buffer-id})]
    (when (= :left button)
      (let [{:keys [target]} mouse-hover]
        (if (constants/tab? target)
          (new-tab! game session target)
          (case target
            :font-dec (swap! session/*session font-dec)
            :font-inc (swap! session/*session font-inc)
            :reload-file (when (and buffer (reload-file! buffer (::pipes game) (:id current-tab)))
                           (new-tab! game session :repl-in))
            :text (when buffer
                    (let [text-box (session/get-text-box session {:?id (:id current-tab)})
                          window (session/get-window session)
                          constants (session/get-constants session)
                          font (session/get-font session)]
                      (async/put! (:paravim.core/command-chan game)
                                  [:move-cursor (mouse->cursor-position buffer (:mouse-anchor mouse-hover) (:size font) text-box constants window)])))
            nil))))))

(defn scroll! [session xoffset yoffset]
  (let [current-tab (session/get-current-tab session)
        tab (session/get-tab session {:?id (:id current-tab)})
        buffer-id (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id buffer-id})]
    (when buffer
      (swap! session/*session upsert-buffer (merge buffer (scroll/start-scrolling-camera buffer xoffset yoffset))))))

(defn get-mode []
  (:mode (session/get-vim @session/*session)))

(defn command-text [text completion]
  {:command-text text :command-completion (when (some-> text str/trim seq) completion)})

(defn assoc-command-entity [command text-entity cursor-entity]
  (assoc command :command-text-entity text-entity :command-cursor-entity cursor-entity))

(defn assoc-command [{:keys [command-start command-text command-completion] :as command}
                     {:keys [base-text-entity base-font-entity base-rects-entity font-height] :as constants}
                     vim-mode font-size position]
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
          command-cursor-entity (i/assoc base-rects-entity 0 (buffers/->cursor-entity vim-mode constants line-chars 0 (inc position) font-size))]
      (assoc-command-entity command command-text-entity command-cursor-entity))
    (assoc-command-entity command nil nil)))

(defn get-extension
  [path]
  (some->> (str/last-index-of path ".")
           (+ 1)
           (subs path)
           str/lower-case))

(def clojure-exts #{"clj" "cljs" "cljc" "edn"})

(defn clojure-path? [path]
  (-> path get-extension clojure-exts))

(defn ->buffer [id {:keys [base-font-entity base-text-entity font-height] :as constants} path file-name lines current-tab]
  (let [clojure? (or (= current-tab :repl-in)
                     (clojure-path? path))]
    {:id id
     :text-entity (buffers/assoc-lines base-text-entity base-font-entity font-height lines)
     :camera (t/translate constants/orig-camera 0 0)
     :camera-x 0
     :camera-y 0
     :camera-target-x 0
     :camera-target-y 0
     :camera-animation-time 0
     :scroll-speed-x 0
     :scroll-speed-y 0
     :path path
     :file-name file-name
     :lines lines
     :tab-id current-tab
     :clojure? clojure?
     :needs-clojure-refresh? clojure?
     :needs-parinfer-init? clojure?}))

(defn ->ascii [id {:keys [base-font-entity base-text-entity font-height] :as constants} lines]
  {:id id
   :text-entity (buffers/assoc-lines base-text-entity base-font-entity font-height lines)
   :camera (t/translate constants/orig-camera 0 0)
   :camera-x 0
   :camera-y 0
   :camera-target-x 0
   :camera-target-y 0
   :camera-animation-time 0
   :scroll-speed-x 0
   :scroll-speed-y 0
   :lines lines
   :tab-id :files})

(defn assoc-attr-lengths [text-entity]
  (reduce
    (fn [text-entity attr-name]
      (let [type-name (u/get-attribute-type text-entity attr-name)
            {:keys [size iter]} (merge u/default-opts (u/type->attribute-opts type-name))]
        (assoc-in text-entity [:attribute-lengths attr-name]
                  (* size iter))))
    text-entity
    (keys chars/instanced-font-attrs->unis)))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; create rect entities
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
               text-boxes {:files {:left 0 :right 0 :top snap-to-top :bottom snap-to-bottom}
                           :repl-in {:left 0 :right 0 :top repl-in-top :bottom snap-to-bottom}
                           :repl-out {:left 0 :right 0 :top snap-to-top :bottom repl-out-bottom}}]
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
                                         (assoc m id {:x1 left :y1 0 :x2 right :y2 font-height})))
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
                (swap! session/*session
                  (fn [session]
                    (as-> session $
                          (clara/insert $
                            (session/map->Game game)
                            (session/->Window (utils/get-width game) (utils/get-height game))
                            (session/map->Constants
                              {:base-rect-entity base-rect-entity
                               :base-rects-entity base-rects-entity
                               :font-width font-width
                               :font-height font-height
                               :base-font-entity base-font-entity
                               :base-text-entity base-text-entity
                               :roboto-font-entity roboto-font-entity
                               :roboto-text-entity roboto-text-entity
                               :toolbar-text-entities (merge tab-entities button-entities)
                               :highlight-text-entities highlight-button-entities}))
                          (reduce-kv
                            (fn [session id text-box]
                              (clara/insert session (session/map->TextBox (assoc text-box :id id))))
                            $
                            text-boxes)
                          (reduce-kv
                            (fn [session id bounding-box]
                              (clara/insert session (session/map->BoundingBox (assoc bounding-box :id id))))
                            $
                            bounding-boxes)
                          (clara/fire-rules $)))))))))))
  game)

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color colors/bg-color :depth 1}})

(defn render-buffer [game session constants font-size-multiplier game-width game-height current-tab buffer-ptr show-cursor? show-minimap?]
  (when-let [{:keys [rects-entity text-entity parinfer-text-entity camera]
              :as buffer} (session/get-buffer session {:?id buffer-ptr})]
    (when-let [text-box (session/get-text-box session {:?id current-tab})]
      (let [text-top ((:top text-box) game-height font-size-multiplier)]
        (when (and rects-entity show-cursor?)
          (c/render game (-> rects-entity
                             (t/project game-width game-height)
                             (t/camera camera)
                             (t/translate 0 text-top)
                             (t/scale font-size-multiplier font-size-multiplier))))
        (let [[lines-to-skip-count lines-to-crop-count] (buffers/get-visible-lines buffer constants text-box game-height font-size-multiplier)]
          (when parinfer-text-entity
            (c/render game (-> parinfer-text-entity
                               (buffers/crop-text-entity lines-to-skip-count lines-to-crop-count)
                               (t/project game-width game-height)
                               (t/camera camera)
                               (t/translate 0 text-top)
                               (t/scale font-size-multiplier font-size-multiplier))))
          (c/render game (-> text-entity
                             (buffers/crop-text-entity lines-to-skip-count lines-to-crop-count)
                             (cond-> (not show-cursor?)
                                     (assoc-in [:uniforms 'u_alpha] colors/unfocused-alpha))
                             (t/project game-width game-height)
                             (t/camera camera)
                             (t/translate 0 text-top)
                             (t/scale font-size-multiplier font-size-multiplier)))
          (when (and show-minimap? (:show-minimap? buffer))
            (when-let [minimap (session/get-minimap session {:?id buffer-ptr})]
              (c/render game (:rects-entity minimap))
              (c/render game (-> (:text-entity minimap)
                                 (cond-> (not show-cursor?)
                                         (assoc-in [:uniforms 'u_alpha] colors/unfocused-alpha)))))))))))

(defn tick [game]
  (let [session @session/*session
        font-size-multiplier (:size (session/get-font session))
        current-tab (:id (session/get-current-tab session))
        current-buffer (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id current-buffer})
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
      (if (:paravim.core/clear? game)
        (c/render game (update screen-entity :viewport assoc :width game-width :height game-height))
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color colors/bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width game-height))))))
      (if (and ascii (= current-tab :files))
        (render-buffer game session constants font-size-multiplier game-width game-height current-tab ascii false false)
        (render-buffer game session constants font-size-multiplier game-width game-height current-tab current-buffer (not= mode 'COMMAND_LINE) (#{:files :repl-out} current-tab)))
      (case current-tab
        :repl-in (when-let [buffer-ptr (:buffer-id (session/get-tab session {:?id :repl-out}))]
                   (render-buffer game session constants font-size-multiplier game-width game-height :repl-out buffer-ptr false true))
        :repl-out (when-let [buffer-ptr (:buffer-id (session/get-tab session {:?id :repl-in}))]
                    (render-buffer game session constants font-size-multiplier game-width game-height :repl-in buffer-ptr false false))
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
              :let [bounding-box (session/get-bounding-box session {:?id k})
                    highlight-entity (when control?
                                       (get highlight-text-entities k))]
              ;; hide the reload file button when necessary
              :when (or (not= k :reload-file)
                        (and (= current-tab :files)
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
    ;; insert/update the game record
    (if-let [game' (session/get-game session)]
      (let [;; read from command chan and update game record if necessary
            poll-input! (:paravim.core/poll-input! game)
            game (if poll-input!
                   (poll-input! game)
                   ;; if `game` has no poll-input, use the one from the session.
                   ;; this would only happen in old versions of the play-cljc template
                   ;; because it wasn't passing the latest game map to this function.
                   ((:paravim.core/poll-input! game') (merge game' game)))]
        ;; put new game record in the session
        (swap! session/*session
          (fn [session]
            (-> session
                (clarax/merge game' game)
                clara/fire-rules)))
        ;; return new game record
        game)
      ;; if game record doesn't exist, call `init` again.
      ;; this is only useful during development for code reloading.
      (init game))))

;; this dummy function is overwritten in paravim.start-dev
;; so we can run things whenever this ns is reloaded
(defonce ^:private on-reload (fn []))
(on-reload)

