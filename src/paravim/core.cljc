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
                                 ::session/cursor nil})
      o/fire-rules))

(defn update-window-size [m width height]
  (-> m
      (update :session
              (fn [session]
                (-> session
                    (clarax/merge (clara/query session ::session/get-window) {:width width :height height})
                    clara/fire-rules)))
      (update :osession
              (fn [osession]
                (-> osession
                    (o/insert ::session/window {::session/width width ::session/height height})
                    o/fire-rules)))))

(defn update-current-tab [m id]
  (-> m
      (update :session
              (fn [session]
                (-> session
                    (clarax/merge (clara/query session ::session/get-current-tab) {:id id})
                    clara/fire-rules)))
      (update :osession
              (fn [osession]
                (-> osession
                    (o/insert ::session/tab ::session/current id)
                    o/fire-rules)))))

(defn new-tab! [game session tab-id]
  (let [buffer-id-or-path (or (:buffer-id (session/get-tab session {:?id tab-id}))
                              ;; if no buffer exists, open a new one
                              (get constants/tab->path tab-id))]
    (async/put! (::command-chan game) [:new-buf buffer-id-or-path])))

(defn shift-current-tab! [game session osession direction]
  (let [current-tab (session/get-current-tab osession)
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

(defn insert-buffer-update [session osession bu]
  (if-let [buffer (session/get-buffer session {:?id (:buffer-id bu)})]
    (let [constants (session/get-constants osession)]
      (as-> buffer $
            (buffers/update-text-buffer $ constants (:lines bu) (:first-line bu) (:line-count-change bu))
            (assoc $ :needs-clojure-refresh? (:clojure? buffer))
            (clarax/merge session buffer $)
            (clara/fire-rules $)))
    session))

(defn insert-buffer-refresh [session osession buffer-id]
  (if-let [buffer (session/get-buffer session {:?id buffer-id})]
    (let [constants (session/get-constants osession)
          vim (session/get-vim osession)
          font-multiplier (:multiplier (session/get-font osession))
          text-box (session/get-text-box osession (:tab-id buffer))
          window (session/get-window osession)]
      (as-> buffer $
            (if (:needs-clojure-refresh? buffer)
              (-> $
                  (buffers/parse-clojure-buffer (:mode vim))
                  (buffers/update-clojure-buffer constants)
                  (assoc :needs-clojure-refresh? false))
              $)
            (buffers/update-cursor $ (:mode vim) font-multiplier text-box constants window)
            (buffers/update-highlight $ constants)
            (buffers/update-selection $ constants (:visual-range vim))
            (buffers/update-search-highlights $ constants vim)
            (clarax/merge session buffer $)
            (clara/fire-rules $)))
    session))

(defn change-font-size [{:keys [osession] :as m} diff-multiplier]
  (let [font (session/get-font osession)
        font-height (:font-height (session/get-constants osession))
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
      (-> m
          (update :session
                  (fn [session]
                    (-> session
                        (clarax/merge (clara/query session ::session/get-font) {:size new-val})
                        clara/fire-rules)))
          (update :osession
                  (fn [osession]
                    (-> osession
                        (o/insert ::session/font ::session/size new-val)
                        o/fire-rules))))
      m)))

(defn font-dec [m]
  (change-font-size m (- constants/font-size-step)))

(defn font-inc [m]
  (change-font-size m constants/font-size-step))

(defn font-multiply [m n]
  (let [font-size (:multiplier (session/get-font (:osession m)))]
    (change-font-size m (- (* font-size n) font-size))))

(defn upsert-buffer [session buffer]
  (let [total-time (-> session session/get-game :total-time)
        buffer (assoc buffer :last-update total-time)]
    (if-let [existing-buffer (session/get-buffer session {:?id (:id buffer)})]
      (-> session
          (clarax/merge existing-buffer buffer)
          clara/fire-rules)
      (-> session
          (clara/insert (session/map->Buffer buffer))
          (clara/insert (session/map->Minimap {:buffer-id (:id buffer)}))
          clara/fire-rules))))

(defn remove-buffer [session buffer-id]
  (let [buffer (session/get-buffer session {:?id buffer-id})
        minimap (session/get-minimap session {:?id buffer-id})]
    (-> session
        (cond-> buffer (clara/retract buffer))
        (cond-> minimap (clara/retract minimap))
        clara/fire-rules)))

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

(defn click-mouse! [game {:keys [session osession]} button]
  (let [mouse (session/get-mouse osession)
        current-tab (session/get-current-tab osession)
        buffer-id (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id buffer-id})]
    (when (= :left button)
      (let [{:keys [target]} mouse]
        (if (constants/tab? target)
          (new-tab! game session target)
          (case target
            ::session/font-dec (swap! session/*session font-dec)
            ::session/font-inc (swap! session/*session font-inc)
            ::session/reload-file (when (and buffer (reload-file! buffer (::pipes game) (:id current-tab)))
                                    (new-tab! game session ::session/repl-in))
            :text (when buffer
                    (let [text-box (session/get-text-box osession (:id current-tab))
                          window (session/get-window osession)
                          constants (session/get-constants osession)
                          font-multiplier (:multiplier (session/get-font osession))]
                      (async/put! (::command-chan game)
                                  [:move-cursor (mouse->cursor-position buffer mouse font-multiplier text-box constants window)])))
            nil))))))

(defn scroll! [{:keys [session osession]} xoffset yoffset]
  (let [current-tab (session/get-current-tab osession)
        tab (session/get-tab session {:?id (:id current-tab)})
        buffer-id (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id buffer-id})]
    (when buffer
      (swap! session/*session update :session upsert-buffer (merge buffer (scroll/start-scrolling-camera buffer xoffset yoffset))))))

(defn get-mode []
  (:mode (session/get-vim (:osession @session/*session))))

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
  (let [clojure? (or (= current-tab ::session/repl-in)
                     (and (clojure-path? path)
                          ;; disable clojure support in large files for now,
                          ;; because it will be too slow to type
                          (< (count lines) constants/max-clojure-lines)))]
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
     :needs-parinfer-init? clojure?
     :last-update 0}))

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
   :tab-id ::session/files
   :last-update 0})

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
               text-boxes {::session/files {:left 0 :right 0 :top snap-to-top :bottom snap-to-bottom}
                           ::session/repl-in {:left 0 :right 0 :top repl-in-top :bottom snap-to-bottom}
                           ::session/repl-out {:left 0 :right 0 :top snap-to-top :bottom repl-out-bottom}}]
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

; cache the entities so they aren't recaculated after reloading the session
(defonce ^:private *entity-cache (atom nil))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; initialize session
  (session/def-queries
    (swap! session/*session assoc :session
            (clara/insert @session/*initial-session
              (session/map->Game game)
              (session/->Window (utils/get-width game) (utils/get-height game))
              (session/->CurrentTab ::session/files)
              (session/->Tab ::session/files nil)
              (session/->Tab ::session/repl-in nil)
              (session/->Tab ::session/repl-out nil)
              (session/->Font 0) ;; initialized in the font rule
              (session/->FontMultiplier constants/default-font-multiplier))))
  (swap! session/*session assoc :osession
         (-> @session/*initial-osession
             (o/insert ::session/vim {::session/mode 'NORMAL
                                      ::session/ascii nil
                                      ::session/control? false
                                      ::session/show-search? false
                                      ::session/visual-range nil
                                      ::session/highlights []
                                      ::session/message nil
                                      ::session/command-start nil
                                      ::session/command-text nil
                                      ::session/command-completion nil
                                      ::session/command-text-entity nil
                                      ::session/command-cursor-entity nil})
             (o/insert ::session/font {::session/size 0 ;; initialized in the font rule
                                       ::session/multiplier constants/default-font-multiplier})
             (o/insert ::session/window {::session/width (utils/get-width game)
                                         ::session/height (utils/get-height game)})
             (o/insert ::session/tab ::session/current ::session/files)))
  ;; initialize entities
  (let [callback (fn [{:keys [constants text-boxes bounding-boxes] :as entities}]
                   (reset! *entity-cache entities)
                   (swap! session/*session update :osession
                          (fn [session]
                            (as-> session $
                                  (o/insert $ ::session/constant constants)
                                  (reduce-kv
                                    (fn [session id text-box]
                                      (reduce-kv
                                        (fn [session attr value]
                                          ;; FIXME: temporary hack
                                          (o/insert session id (keyword "paravim.session" (name attr)) value))
                                        session
                                        text-box))
                                    $
                                    text-boxes)
                                  (reduce-kv
                                    (fn [session id bounding-box]
                                      (reduce-kv
                                        (fn [session attr value]
                                          ;; FIXME: temporary hack
                                          (o/insert session id (keyword "paravim.session" (name attr)) value))
                                        session
                                        bounding-box))
                                    $
                                    bounding-boxes)
                                  (o/fire-rules $))))
                   (swap! session/*session update :session
                          (fn [session]
                            (as-> session $
                                  (reduce-kv
                                    (fn [session id text-box]
                                      (clara/insert session (session/map->TextBox (assoc text-box :id id))))
                                    $
                                    text-boxes)
                                  (reduce-kv
                                    (fn [session id bounding-box]
                                      (clara/insert session (session/map->BoundingBox (assoc bounding-box :id id))))
                                    $
                                    bounding-boxes)))))]
    (if-let [entities @*entity-cache]
      (callback entities)
      (init-entities game callback)))
  ;; fire rules
  (swap! session/*session update :session clara/fire-rules)
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

(defn render-buffer [game session osession constants font-size-multiplier game-width game-height current-tab buffer-ptr show-cursor? show-minimap?]
  (when-let [{:keys [rects-entity text-entity parinfer-text-entity camera]
              :as buffer} (session/get-buffer session {:?id buffer-ptr})]
    (when-let [text-box (session/get-text-box osession current-tab)]
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
            (when-let [minimap (session/get-minimap session {:?id buffer-ptr})]
              (c/render game (:rects-entity minimap))
              (c/render game (-> (:text-entity minimap)
                                 (cond-> (not show-cursor?)
                                         (assoc-in [:uniforms 'u_alpha] colors/unfocused-alpha)))))))))))

(defn tick [game]
  (when @session/*reload?
    (reset! session/*reload? false)
    (init game))
  (let [{:keys [session osession]} @session/*session
        current-tab (:id (session/get-current-tab osession))
        current-buffer (session/get-current-buffer session)
        buffer (session/get-buffer session {:?id current-buffer})
        font-size-multiplier (:multiplier (session/get-font osession))
        {game-width :width game-height :height :as window} (session/get-window osession)
        {:keys [base-rect-entity base-rects-entity
                base-text-entity base-font-entity
                font-height
                toolbar-text-entities highlight-text-entities]
         :as constants} (session/get-constants osession)
        {:keys [mode ascii control?
                command-text-entity command-cursor-entity]
         :as vim} (session/get-vim osession)]
    (when (and window (pos? game-width) (pos? game-height))
      (if (::clear? game)
        (c/render game (update screen-entity :viewport assoc :width game-width :height game-height))
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color colors/bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width game-height))))))
      (if (and ascii (= current-tab ::session/files))
        (render-buffer game session osession constants font-size-multiplier game-width game-height current-tab ascii false false)
        (render-buffer game session osession constants font-size-multiplier game-width game-height current-tab current-buffer (not= mode 'COMMAND_LINE) (#{::session/files ::session/repl-out} current-tab)))
      (case current-tab
        ::session/repl-in (when-let [buffer-ptr (:buffer-id (session/get-tab session {:?id ::session/repl-out}))]
                            (render-buffer game session osession constants font-size-multiplier game-width game-height ::session/repl-out buffer-ptr false true))
        ::session/repl-out (when-let [buffer-ptr (:buffer-id (session/get-tab session {:?id ::session/repl-in}))]
                             (render-buffer game session osession constants font-size-multiplier game-width game-height ::session/repl-in buffer-ptr false false))
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
              :when (or (not= k ::session/reload-file)
                        (and (= current-tab ::session/files)
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
    (let [game' (session/get-game session)
          ;; read from command chan and update game record if necessary
          poll-input! (::poll-input! game)
          game (if poll-input!
                 (poll-input! game)
                 ;; if `game` has no poll-input, use the one from the session.
                 ;; this would only happen in old versions of the play-cljc template
                 ;; because it wasn't passing the latest game map to this function.
                 ((::poll-input! game') (merge game' game)))]
      ;; put new game record in the session
      (swap! session/*session update :session
        (fn [session]
          (when session ;; this could be momentarily nil while reloading the paravim.session ns
            (-> session
                (clarax/merge game' game)
                clara/fire-rules))))
      ;; return new game record
      game)))

