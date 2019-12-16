(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [paravim.session :as session]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            [parinferish.core :as ps]
            [clojure.string :as str]
            [play-cljc.gl.utils :as u]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.text :as text]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.primitives-2d :as primitives]
            [clojure.core.rrb-vector :as rrb]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-font-cljs]])))

(def orig-camera (e/->camera true))
(def tabs [{:id :files
            :text "Files"}
           {:id :repl-in
            :text "REPL In"}
           {:id :repl-out
            :text "REPL Out"}])
(def buttons [{:id :font-inc
               :text "Font +"
               :shortcut-char 5}
              {:id :font-dec
               :text "Font -"
               :shortcut-char 5}
              {:id :reload-file
               :text "Reload File"
               :shortcut-char 7}])
(def tab-ids (mapv :id tabs))
(def tab? (set tab-ids))
(def tab->path {:files "scratch.clj"
                :repl-in "repl.in"
                :repl-out "repl.out"})

(def font-size-step (/ 1 16))
(def min-font-size (/ 1 8))
(def max-font-size 1)

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :current-buffer nil
                       :buffers {}
                       :buffer-updates []
                       :current-tab :files
                       :tab->buffer {}
                       :font-size-multiplier (/ 1 4)
                       :text-boxes {}
                       :bounding-boxes {}
                       :show-search? false}))

(let [query-fns (clarax/query-fns @session/*session)]
  (def get-game (:get-game query-fns))
  (def get-window (:get-window query-fns))
  (def get-mouse (:get-mouse query-fns)))

(defn update-mouse-coords! [x y]
  (swap! session/*session
    (fn [session]
      (as-> session $
            (get-mouse $)
            (clarax/merge session $ {:x x :y y})
            (clara/fire-rules $)))))

(defn update-window-size! [width height]
  (swap! session/*session
    (fn [session]
      (as-> session $
            (get-window $)
            (clarax/merge session $ {:width width :height height})
            (clara/fire-rules $)))))

(def bg-color [(/ 52 255) (/ 40 255) (/ 42 255) 0.95])

(def text-color [1 1 1 1])
(def cursor-color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
(def select-color [(/ 148 255) (/ 69 255) (/ 5 255) 0.8])
(def search-color [(/ 127 255) (/ 52 255) (/ 83 255) 0.8])

(def text-alpha 1.0)
(def parinfer-alpha 0.15)
(def highlight-alpha 0.05)
(def unfocused-alpha 0.5)
(def completion-alpha 0.65)

(def yellow-color [(/ 255 255) (/ 193 255) (/ 94 255) 1])
(def tan-color [(/ 209 255) (/ 153 255) (/ 101 255) 1])
(def cyan-color [(/ 86 255) (/ 181 255) (/ 194 255) 1])
(def gray-color [(/ 150 255) (/ 129 255) (/ 133 255) 1])

(def colors {:number yellow-color
             :string tan-color
             :keyword cyan-color
             :comment gray-color})

(def orange-color [(/ 220 255) (/ 103 255) (/ 44 255) 1])
(def red-color [(/ 210 255) (/ 45 255) (/ 58 255) 1])
(def green-color [(/ 65 255) (/ 174 255) (/ 122 255) 1])

(def rainbow-colors [orange-color
                     red-color
                     green-color])

(defn get-color [class-name depth]
  (or (colors class-name)
      (case class-name
        :delimiter (nth rainbow-colors (mod depth (count rainbow-colors)))
        text-color)))

(defn set-alpha [color alpha]
  (assoc color 3 alpha))

(defn get-mode []
  (:mode @*state))

(defn clojurify-lines
  ([text-entity font-entity parsed-code parinfer?]
   (let [*line-num (volatile! 0)
         *char-num (volatile! -1)
         *char-counts (volatile! [])
         *collections (volatile! [])
         characters (reduce
                      (fn [characters data]
                        (clojurify-lines characters font-entity *line-num *char-num *char-counts *collections nil -1 data parinfer?))
                      (:characters text-entity)
                      parsed-code)
         ;; the last line's char count
         char-counts (vswap! *char-counts conj (inc @*char-num))
         ;; make sure the parinfer entity doesn't render any trailing characters it removed
         ;; see test: dedent-function-body
         characters (if (and (seq characters) parinfer?)
                      (reduce-kv
                        (fn [characters line-num char-count]
                          (update characters line-num
                                  (fn [char-entities]
                                    (if (> (count char-entities) char-count)
                                      (rrb/subvec char-entities 0 char-count)
                                      char-entities))))
                        characters
                        char-counts)
                      characters)
         ;; add characters to the attributes
         text-entity (if (seq characters)
                       (reduce-kv
                         (fn [entity line-num char-entities]
                           (if (not= (get-in entity [:characters line-num]) char-entities)
                             (chars/assoc-line entity line-num char-entities)
                             entity))
                         text-entity
                         characters)
                       text-entity)]
     (assoc text-entity :collections @*collections)))
  ([characters font-entity *line-num *char-num *char-counts *collections class-name depth data parinfer?]
   (if (vector? data)
     (let [[class-name & children] data
           depth (cond-> depth
                         (= class-name :collection)
                         inc)
           line-num @*line-num
           char-num @*char-num
           characters (if (or (and parinfer? (-> data meta :action (= :remove)))
                              (and (not parinfer?) (-> data meta :action (= :insert))))
                        characters
                        (reduce
                          (fn [characters child]
                            (clojurify-lines characters font-entity *line-num *char-num *char-counts *collections class-name depth child parinfer?))
                          characters
                          children))]
       (when (= class-name :collection)
         (vswap! *collections conj {:start-line line-num
                                    :start-column (inc char-num)
                                    :end-line @*line-num
                                    :end-column (inc @*char-num)
                                    :depth depth}))
       characters)
     (let [color (get-color class-name depth)]
       (reduce
         (fn [characters ch]
           (if (= ch \newline)
             (do
               (vswap! *char-counts conj (inc @*char-num))
               (vswap! *line-num inc)
               (vreset! *char-num -1)
               characters)
             (update characters @*line-num
               (fn [char-entities]
                 (update char-entities (vswap! *char-num inc)
                   (fn [entity]
                     (-> (if (and parinfer?
                                  (-> entity :character (not= ch)))
                           (chars/crop-char font-entity ch)
                           entity)
                         (t/color color))))))))
         characters
         data)))))

(defn update-lines [lines new-lines first-line line-count-change]
  (if (= 0 line-count-change)
    (reduce-kv
      (fn [lines line-offset line]
        (assoc lines (+ first-line line-offset) line))
      lines
      new-lines)
    (let [lines-to-remove (if (neg? line-count-change)
                            (+ (* -1 line-count-change) (count new-lines))
                            (- (count new-lines) line-count-change))]
      (rrb/catvec
        (rrb/subvec lines 0 first-line)
        new-lines
        (if (seq lines) ;; see test: delete-all-lines
          (rrb/subvec lines (+ first-line lines-to-remove))
          [])))))

(defn replace-lines [text-entity font-entity new-lines first-line line-count-change]
  (let [new-chars (mapv
                    (fn [line]
                      (mapv
                        (fn [ch]
                          (chars/crop-char font-entity ch))
                        line))
                    new-lines)]
    (if (= 0 line-count-change)
      (reduce-kv
        (fn [text-entity line-offset char-entities]
          (chars/assoc-line text-entity (+ first-line line-offset) char-entities))
        text-entity
        new-chars)
      (let [lines-to-remove (if (neg? line-count-change)
                              (+ (* -1 line-count-change) (count new-lines))
                              (- (count new-lines) line-count-change))
            text-entity (if (seq (:characters text-entity)) ;; see test: delete-all-lines
                          (reduce
                            (fn [text-entity _]
                              (chars/dissoc-line text-entity first-line))
                            text-entity
                            (range 0 lines-to-remove))
                          text-entity)
            text-entity (reduce-kv
                          (fn [text-entity line-offset char-entities]
                            (chars/insert-line text-entity (+ first-line line-offset) char-entities))
                          text-entity
                          new-chars)]
        text-entity))))

(defn get-visible-lines [{:keys [characters] :as text-entity}
                         {:keys [font-height font-size-multiplier] :as state}
                         {:keys [top bottom] :as text-box}
                         game-height
                         camera-y]
  (let [text-height (- (bottom game-height font-size-multiplier)
                       (top game-height font-size-multiplier))
        char-height (* font-height font-size-multiplier)
        line-count (count characters)
        lines-to-skip-count (max 0 (min (int (/ camera-y char-height))
                                        line-count))
        lines-to-crop-count (max 0 (min (+ lines-to-skip-count
                                           (int (/ text-height char-height))
                                           1)
                                        line-count))]
    [lines-to-skip-count lines-to-crop-count]))

(defn get-visible-chars [{:keys [characters] :as text-entity} lines-to-skip-count lines-to-crop-count]
  (let [char-counts (get-in text-entity [:uniforms 'u_char_counts])
        chars-to-skip-count (reduce + 0 (subvec char-counts 0 lines-to-skip-count))
        char-counts (subvec char-counts lines-to-skip-count lines-to-crop-count)
        chars-to-crop-count (+ chars-to-skip-count (reduce + 0 char-counts))]
    [chars-to-skip-count chars-to-crop-count char-counts]))

(defn ->cursor-entity [{:keys [font-width font-height base-rect-entity font-size-multiplier mode] :as state} line-chars line column]
  (let [left-char (get line-chars (dec column))
        curr-char (get line-chars column)
        {:keys [left width height]} curr-char
        width (cond-> (or width font-width)
                      ('#{INSERT COMMAND_LINE} mode)
                      (/ 4))
        left (or left
                 (some-> (:left left-char)
                         (+ (:width left-char)))
                 0)
        top (* line font-height)
        height (or height font-height)]
    (-> base-rect-entity
        (t/color cursor-color)
        (t/translate left top)
        (t/scale width height)
        (assoc :left (* left font-size-multiplier)
               :top (* top font-size-multiplier)
               :width (* width font-size-multiplier)
               :height (* height font-size-multiplier)))))

(defn update-cursor [{:keys [text-entity cursor-line cursor-column text-box] :as buffer} {:keys [base-rects-entity font-size-multiplier] :as state} game]
  (let [line-chars (get-in buffer [:text-entity :characters cursor-line])
        {:keys [left top width height] :as cursor-entity} (->cursor-entity state line-chars cursor-line cursor-column)]
    (-> buffer
        (assoc :rects-entity (-> base-rects-entity
                                 (i/assoc 0 cursor-entity)
                                 (assoc :rect-count 1)))
        (as-> buffer
              (let [{:keys [camera camera-x camera-y]} buffer
                    game-width (utils/get-width game)
                    game-height (utils/get-height game)
                    text-top ((:top text-box) game-height font-size-multiplier)
                    text-bottom ((:bottom text-box) game-height font-size-multiplier)
                    cursor-bottom (+ top height)
                    cursor-right (+ left width)
                    text-height (- text-bottom text-top)
                    camera-bottom (+ camera-y text-height)
                    camera-right (+ camera-x game-width)
                    camera-x (cond
                               (< left camera-x)
                               left
                               (> cursor-right camera-right)
                               (- cursor-right game-width)
                               :else
                               camera-x)
                    camera-y (cond
                               (< top camera-y)
                               top
                               (> cursor-bottom camera-bottom 0)
                               (- cursor-bottom text-height)
                               :else
                               camera-y)
                    [lines-to-skip-count lines-to-crop-count] (get-visible-lines text-entity state text-box game-height camera-y)]
                (assoc buffer
                  :camera (t/translate orig-camera camera-x (- camera-y text-top))
                  :camera-x camera-x
                  :camera-y camera-y
                  :visible-start-line lines-to-skip-count
                  :visible-end-line lines-to-crop-count))))))

(defn update-mouse [{:keys [text-boxes current-tab bounding-boxes toolbar-text-entities font-size-multiplier] :as state} game x y]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [left right top bottom] :as text-box} (get text-boxes current-tab)
        hover (if (and text-box
                       (<= left x (- game-width right))
                       (<= (top game-height font-size-multiplier)
                           y
                           (bottom game-height font-size-multiplier)))
                :text
                (some
                  (fn [[k box]]
                    (let [{:keys [x1 y1 x2 y2 align]} box
                          x1 (cond->> (* x1 font-size-multiplier)
                                      (= :right align)
                                      (- game-width))
                          y1 (* y1 font-size-multiplier)
                          x2 (cond->> (* x2 font-size-multiplier)
                                      (= :right align)
                                      (- game-width))
                          y2 (* y2 font-size-multiplier)]
                      (when (and (<= x1 x x2) (<= y1 y y2))
                        k)))
                  bounding-boxes))]
    (assoc state
      :mouse-x x
      :mouse-y y
      :mouse-hover hover
      :mouse-type (cond
                    (= hover :text)
                    :ibeam
                    (contains? toolbar-text-entities hover)
                    :hand))))

(defn change-font-size [{:keys [font-size-multiplier current-buffer] :as state} game diff]
  (let [new-val (+ font-size-multiplier diff)]
    (if (<= min-font-size new-val max-font-size)
      (let [state (assoc state :font-size-multiplier new-val)]
        (if current-buffer
          (update-in state [:buffers current-buffer] update-cursor state game)
          state))
      state)))

(defn font-dec [state game]
  (change-font-size state game (- font-size-step)))

(defn font-inc [state game]
  (change-font-size state game font-size-step))

(defn click-mouse [{:keys [mouse-hover] :as state} game reload-file]
  (if (tab? mouse-hover)
    (assoc state :current-tab mouse-hover)
    (case mouse-hover
      :font-dec (font-dec state game)
      :font-inc (font-inc state game)
      :reload-file (reload-file state)
      state)))

(defn change-tab [{:keys [current-tab] :as state} direction]
  (let [index (+ (.indexOf tab-ids current-tab)
                 direction)
        index (cond
                (neg? index) (dec (count tab-ids))
                (= index (count tab-ids)) 0
                :else index)]
    (assoc state :current-tab (nth tab-ids index))))

(defn update-uniforms [{:keys [characters] :as text-entity} font-height alpha]
  (update text-entity :uniforms assoc
      'u_char_counts (mapv count characters)
      'u_font_height font-height
      'u_alpha alpha
      'u_start_line 0))

(defn assoc-command-text [state text completion]
  (assoc state :command-text text :command-completion (when (some-> text str/trim seq) completion)))

(defn assoc-command-entity [state text-entity cursor-entity]
  (assoc state :command-text-entity text-entity :command-cursor-entity cursor-entity))

(defn update-command [{:keys [base-text-entity base-font-entity base-rects-entity font-height command-start command-text command-completion] :as state} position]
  (if command-text
    (let [char-entities (mapv #(-> base-font-entity
                                   (chars/crop-char %)
                                   (t/color bg-color))
                          (str command-start command-text))
          completion-entities (when command-completion
                                (mapv #(-> base-font-entity
                                           (chars/crop-char %)
                                           (t/color (set-alpha bg-color completion-alpha)))
                                  (subs
                                    (str command-start
                                         (some->> (str/last-index-of command-text " ") inc (subs command-text 0))
                                         command-completion)
                                    (count char-entities))))
          char-entities (into char-entities completion-entities)
          command-text-entity (-> (chars/assoc-line base-text-entity 0 char-entities)
                                  (update-uniforms font-height text-alpha))
          line-chars (get-in command-text-entity [:characters 0])
          command-cursor-entity (i/assoc base-rects-entity 0 (->cursor-entity state line-chars 0 (inc position)))]
      (assoc-command-entity state command-text-entity command-cursor-entity))
    (assoc-command-entity state nil nil)))

(defn range->rects [text-entity font-width font-height {:keys [start-line start-column end-line end-column] :as rect-range}]
  (vec (for [line-num (range start-line (inc end-line))]
         (let [line-chars (-> text-entity :characters (nth line-num))
               start-column (if (= line-num start-line) start-column 0)
               end-column (if (= line-num end-line) end-column (count line-chars))]
           {:left (* font-width start-column)
            ;; to support variable width fonts, we would need...
            ;; :left (->> (subvec line-chars 0 start-column) (map :width) (reduce +))
            :top (* line-num font-height)
            :width (* font-width (- end-column start-column))
            ;; to support variable width fonts, we would need...
            ;; :width (->> (subvec line-chars start-column end-column) (map :width) (reduce +))
            :height font-height}))))

(defn assoc-rects [{:keys [rect-count] :as rects-entity} rect-entity color rects]
  (reduce-kv
    (fn [rects-entity i {:keys [left top width height]}]
      (i/assoc rects-entity (+ i rect-count)
               (-> rect-entity
                   (t/color color)
                   (t/translate left top)
                   (t/scale width height))))
    (update rects-entity :rect-count + (count rects))
    rects))

(defn update-highlight [{:keys [text-entity cursor-line cursor-column] :as buffer} {:keys [font-width font-height base-rect-entity] :as state}]
  (if-let [coll (->> (:collections text-entity)
                     (filter (fn [{:keys [start-line start-column end-line end-column]}]
                               (and (or (< start-line cursor-line)
                                        (and (= start-line cursor-line)
                                             (<= start-column cursor-column)))
                                    (or (> end-line cursor-line)
                                        (and (= end-line cursor-line)
                                             (> end-column cursor-column))))))
                     first)]
    (let [color (set-alpha (get-color :delimiter (:depth coll)) highlight-alpha)
          rects (range->rects text-entity font-width font-height coll)]
      (update buffer :rects-entity assoc-rects base-rect-entity color rects))
    buffer))

(defn update-selection [{:keys [text-entity] :as buffer} {:keys [font-width font-height base-rect-entity] :as state} visual-range]
  (let [{:keys [start-line start-column end-line end-column]} visual-range
        ;; make sure the range is always going the same direction
        visual-range (if (or (> start-line end-line)
                             (and (= start-line end-line)
                                  (> start-column end-column)))
                       {:start-line end-line
                        :start-column end-column
                        :end-line start-line
                        :end-column start-column}
                       visual-range)
        ;; the column the cursor is in doesn't seem to be included in the range
        ;; add it manually so it is included in the selection
        visual-range (update visual-range :end-column inc)
        rects (range->rects text-entity font-width font-height visual-range)]
    (update buffer :rects-entity assoc-rects base-rect-entity select-color rects)))

(defn update-search-highlights [{:keys [text-entity] :as buffer} {:keys [font-width font-height base-rect-entity] :as state} highlights]
  (let [rects (vec (mapcat (partial range->rects text-entity font-width font-height) highlights))]
    (update buffer :rects-entity assoc-rects base-rect-entity search-color rects)))

(defn get-extension
  [path]
  (some->> (str/last-index-of path ".")
           (+ 1)
           (subs path)
           str/lower-case))

(def clojure-exts #{"clj" "cljs" "cljc" "edn"})

(defn get-buffer [state buffer-ptr]
  (get-in state [:buffers buffer-ptr]))

(defn clojure-path? [path]
  (-> path get-extension clojure-exts))

(defn assoc-lines [text-entity font-entity font-height lines]
  (-> (reduce-kv
        (fn [entity line-num line]
          (chars/assoc-line entity line-num (mapv #(chars/crop-char font-entity %) line)))
        text-entity
        lines)
      (update-uniforms font-height text-alpha)))

(defn ->buffer [{:keys [base-font-entity base-text-entity font-height text-boxes] :as state} path file-name lines current-tab]
  {:text-entity (assoc-lines base-text-entity base-font-entity font-height lines)
   :camera (t/translate orig-camera 0 0)
   :camera-x 0
   :camera-y 0
   :path path
   :file-name file-name
   :lines lines
   :text-box (get text-boxes current-tab)
   :clojure? (or (= current-tab :repl-in)
                 (clojure-path? path))})

(defn ->ascii [{:keys [base-font-entity base-text-entity font-height current-tab text-boxes] :as state} lines]
  {:text-entity (assoc-lines base-text-entity base-font-entity font-height lines)
   :camera (t/translate orig-camera 0 0)
   :camera-x 0
   :camera-y 0
   :lines lines
   :text-box (get text-boxes current-tab)})

(defn parse-clojure-buffer [{:keys [lines cursor-line cursor-column] :as buffer} {:keys [mode] :as state} init?]
  (let [parse-opts (cond
                     init? {:mode :paren} ;; see test: fix-bad-indentation
                     (= 'INSERT mode) {:mode :smart :cursor-line cursor-line :cursor-column cursor-column}
                     :else {:mode :indent})
        parsed-code (ps/parse (str/join "\n" lines) parse-opts)]
    (assoc buffer
      :parsed-code parsed-code
      :needs-parinfer? true)))

(defn update-clojure-buffer [{:keys [text-entity parsed-code lines] :as buffer} {:keys [base-font-entity font-height] :as state}]
  (let [text-entity (clojurify-lines text-entity base-font-entity parsed-code false)
        parinfer-text-entity (clojurify-lines text-entity base-font-entity parsed-code true)]
    (assoc buffer
      :text-entity (update-uniforms text-entity font-height text-alpha)
      :parinfer-text-entity (update-uniforms parinfer-text-entity font-height parinfer-alpha))))

(defn update-text-buffer [{:keys [lines] :as buffer} {:keys [base-font-entity font-height] :as state} new-lines first-line line-count-change]
  (-> buffer
      (assoc :lines (update-lines lines new-lines first-line line-count-change))
      (update :text-entity
              (fn [text-entity]
                (-> text-entity
                    (replace-lines base-font-entity new-lines first-line line-count-change)
                    (update-uniforms font-height text-alpha))))))

(defn update-buffers [state]
  (if-let [updates (not-empty (:buffer-updates state))]
    (let [buffer-ptrs (set (map :buffer-ptr updates))]
      (as-> state state
            (reduce
              (fn [state {:keys [buffer-ptr lines first-line line-count-change]}]
                (update-in state [:buffers buffer-ptr] update-text-buffer state lines first-line line-count-change))
              state
              updates)
            (assoc state :buffer-updates [])
            (reduce (fn [state buffer-ptr]
                      (if (:clojure? (get-buffer state buffer-ptr))
                        (update-in state [:buffers buffer-ptr]
                          (fn [buffer]
                            (-> buffer
                                (parse-clojure-buffer state false)
                                (update-clojure-buffer state))))
                        state))
                    state buffer-ptrs)))
    state))

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
  ;; insert game record
  (swap! session/*session
    (fn [session]
      (-> session
          (clara/insert
            (session/map->Game game)
            (session/->Window (utils/get-width game) (utils/get-height game)))
          clara/fire-rules)))
  ;; create rect entities
  (let [rect-entity (e/->entity game primitives/rect)
        rects-entity (c/compile game (i/->instanced-entity rect-entity))]
    (swap! *state assoc
      :base-rect-entity rect-entity
      :base-rects-entity rects-entity))
  ;; load fonts
  (#?(:clj load-font-clj :cljs load-font-cljs) :firacode
     (fn [{:keys [data]} baked-font]
       (let [font-entity (-> (text/->font-entity game data baked-font)
                             (t/color text-color))
             text-entity (-> (i/->instanced-entity font-entity)
                             (assoc :vertex chars/instanced-font-vertex-shader
                                    :fragment chars/instanced-font-fragment-shader
                                    :characters [])
                             assoc-attr-lengths)
             text-entity (c/compile game text-entity)
             font-width (-> baked-font :baked-chars (nth (- 115 (:first-char baked-font))) :xadv)
             font-height (:font-height baked-font)
             snap-to-top (fn [game-height multiplier] (* font-height multiplier))
             snap-to-bottom (fn [game-height multiplier] (- game-height (* font-height multiplier)))
             repl-in-top (fn [game-height multiplier] (- game-height (* 5 font-height multiplier)))
             repl-out-bottom (fn [game-height multiplier] (- game-height (* 6 font-height multiplier)))
             text-boxes {:files {:left 0 :right 0 :top snap-to-top :bottom snap-to-bottom}
                         :repl-in {:left 0 :right 0 :top repl-in-top :bottom snap-to-bottom}
                         :repl-out {:left 0 :right 0 :top snap-to-top :bottom repl-out-bottom}}]
         (swap! *state assoc
           :font-width font-width
           :font-height font-height
           :base-font-entity font-entity
           :base-text-entity text-entity
           :text-boxes text-boxes)
         (swap! session/*session
           (fn [session]
             (->> text-boxes
                  (reduce-kv
                    (fn [session id text-box]
                      (clara/insert session (session/map->TextBox (assoc text-box :id id))))
                    session)
                  clara/fire-rules)))
         (#?(:clj load-font-clj :cljs load-font-cljs) :roboto
          (fn [{:keys [data]} baked-font]
            (let [font-entity (-> (text/->font-entity game data baked-font)
                                  (t/color text-color))
                  text-entity (-> (i/->instanced-entity font-entity)
                                  (assoc :vertex chars/instanced-font-vertex-shader
                                         :fragment chars/instanced-font-fragment-shader
                                         :characters [])
                                  assoc-attr-lengths)
                  text-entity (c/compile game text-entity)
                  tab-spacing (* font-width 2)
                  tab-entities (reduce
                                 (fn [m {:keys [id text]}]
                                   (assoc m id (assoc-lines text-entity font-entity font-height [text])))
                                 {}
                                 tabs)
                  bounding-boxes (reduce-kv
                                   (fn [m i {:keys [id]}]
                                     (let [last-tab (some->> (get tabs (dec i)) :id (get m))
                                           left (if last-tab (+ (:x2 last-tab) tab-spacing) 0)
                                           right (-> tab-entities (get id) :characters first last :x-total (+ left))]
                                       (assoc m id {:x1 left :y1 0 :x2 right :y2 font-height})))
                                   {}
                                   tabs)
                  button-entities (reduce
                                    (fn [m {:keys [id text]}]
                                      (assoc m id (assoc-lines text-entity font-entity font-height [text])))
                                    {}
                                    buttons)
                  bounding-boxes (reduce-kv
                                   (fn [m i {:keys [id]}]
                                     (let [last-button (some->> (get buttons (dec i)) :id (get m))
                                           right (if last-button (+ (:x1 last-button) tab-spacing) 0)
                                           left (-> button-entities (get id) :characters first last :x-total (+ right))]
                                      (assoc m id {:x1 left :y1 0 :x2 right :y2 font-height :align :right})))
                                   bounding-boxes
                                   buttons)
                  ;; when the user holds down control, we want to show the shortcut character
                  ;; for each button in a different color so they can learn the shortcut
                  highlight-button-entities (reduce
                                              (fn [m {:keys [id text shortcut-char]}]
                                                (let [character (get-in m [id :characters 0 shortcut-char])]
                                                  (update m id i/assoc shortcut-char
                                                          (-> character
                                                              (chars/update-translation-matrix (:left character) 0)
                                                              (t/color yellow-color)))))
                                              button-entities
                                              buttons)]
              (swap! *state assoc
                :roboto-font-entity font-entity
                :roboto-text-entity text-entity
                :toolbar-text-entities (merge tab-entities button-entities)
                :highlight-text-entities highlight-button-entities
                :bounding-boxes bounding-boxes)
              (swap! session/*session
                (fn [session]
                  (->> bounding-boxes
                       (reduce-kv
                         (fn [session id bounding-box]
                           (clara/insert session (session/map->BoundingBox (assoc bounding-box :id id))))
                         session)
                       clara/fire-rules))))))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color bg-color :depth 1}})

(defn crop-text-entity [text-entity lines-to-skip-count lines-to-crop-count]
  (let [[chars-to-skip-count chars-to-crop-count char-counts] (get-visible-chars text-entity lines-to-skip-count lines-to-crop-count)]
    (reduce-kv
      (fn [text-entity attr-name length]
        (update-in text-entity [:attributes attr-name :data] rrb/subvec
                   (* length chars-to-skip-count)
                   (* length chars-to-crop-count)))
      (update text-entity :uniforms assoc
              'u_char_counts char-counts
              'u_start_line lines-to-skip-count)
      (:attribute-lengths text-entity))))

(defn render-buffer [game {:keys [buffers text-boxes font-size-multiplier] :as state} game-width game-height current-tab buffer-ptr show-cursor?]
  (when-let [{:keys [rects-entity text-entity parinfer-text-entity camera camera-y]} (get buffers buffer-ptr)]
    (when-let [text-box (get text-boxes current-tab)]
      (when (and rects-entity show-cursor?)
        (c/render game (-> rects-entity
                           (t/project game-width game-height)
                           (t/camera camera)
                           (t/scale font-size-multiplier font-size-multiplier))))
      (let [[lines-to-skip-count lines-to-crop-count] (get-visible-lines text-entity state text-box game-height camera-y)]
        (when parinfer-text-entity
          (c/render game (-> parinfer-text-entity
                             (crop-text-entity lines-to-skip-count lines-to-crop-count)
                             (t/project game-width game-height)
                             (t/camera camera)
                             (t/scale font-size-multiplier font-size-multiplier))))
        (c/render game (-> text-entity
                           (crop-text-entity lines-to-skip-count lines-to-crop-count)
                           (cond-> (not show-cursor?)
                                   (assoc-in [:uniforms 'u_alpha] unfocused-alpha))
                           (t/project game-width game-height)
                           (t/camera camera)
                           (t/scale font-size-multiplier font-size-multiplier)))))))

(defn tick [game]
  (let [session @session/*session
        {game-width :width game-height :height :as window} (get-window session)
        {:keys [current-buffer buffers control?
                base-rect-entity base-rects-entity
                command-text-entity command-completion-text-entity command-cursor-entity
                font-height mode font-size-multiplier ascii
                toolbar-text-entities bounding-boxes current-tab tab->buffer highlight-text-entities]
         :as state} @*state]
    (when (and (pos? game-width) (pos? game-height))
      (if (::clear? game)
        (c/render game (update screen-entity :viewport assoc :width game-width :height game-height))
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width game-height))))))
      (if (and ascii (= current-tab :files))
        (render-buffer game state game-width game-height current-tab ascii false)
        (render-buffer game state game-width game-height current-tab current-buffer true))
      (case current-tab
        :repl-in (when-let [buffer-ptr (tab->buffer :repl-out)]
                   (render-buffer game state game-width game-height :repl-out buffer-ptr false))
        :repl-out (when-let [buffer-ptr (tab->buffer :repl-in)]
                    (render-buffer game state game-width game-height :repl-in buffer-ptr false))
        nil)
      (when (and base-rects-entity base-rect-entity)
        (c/render game (-> base-rects-entity
                           (t/project game-width game-height)
                           (i/assoc 0 (-> base-rect-entity
                                          (t/color bg-color)
                                          (t/translate 0 0)
                                          (t/scale game-width (* font-size-multiplier font-height))))
                           (i/assoc 1 (-> base-rect-entity
                                          (t/color (if (= 'COMMAND_LINE mode) tan-color bg-color))
                                          (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                                          (t/scale game-width (* font-size-multiplier font-height)))))))
      (doseq [[k entity] toolbar-text-entities
              :let [bounding-box (k bounding-boxes)
                    highlight-entity (when control?
                                       (get highlight-text-entities k))]
              ;; hide the reload file button when necessary
              :when (or (not= k :reload-file)
                        (and (= current-tab :files)
                             (->> current-buffer (get buffers) :clojure?)))]
        (c/render game (-> (or highlight-entity entity)
                           (assoc-in [:uniforms 'u_alpha] (if (or (= k current-tab)
                                                                  highlight-entity)
                                                            text-alpha
                                                            unfocused-alpha))
                           (t/project game-width game-height)
                           (t/translate (-> bounding-box :x1 (* font-size-multiplier)
                                            (cond->> (= :right (:align bounding-box))
                                                     (- game-width)))
                                        (:y1 bounding-box))
                           (t/scale font-size-multiplier font-size-multiplier))))
      (when (and (= mode 'COMMAND_LINE)
                 command-text-entity
                 command-cursor-entity)
        (c/render game (-> command-cursor-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                           (t/scale font-size-multiplier font-size-multiplier)))
        (c/render game (-> command-text-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                           (t/scale font-size-multiplier font-size-multiplier)))))
    ;; insert/update the game record
    (if-let [game' (get-game session)]
      (swap! session/*session
        (fn [session]
          (-> session
              (clarax/merge game' game)
              clara/fire-rules)))
      (init game)))
  ;; return the game map
  game)

