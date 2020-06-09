(ns paravim.buffers
  (:require [paravim.chars :as chars]
            [paravim.colors :as colors]
            [paravim.constants :as constants]
            [parinferish.core :as ps]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [clojure.core.rrb-vector :as rrb]
            [clojure.string :as str]))

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
     (let [color (colors/get-color class-name depth)]
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

(defn parse-clojure-buffer [{:keys [lines cursor-line cursor-column needs-parinfer-init?] :as buffer} vim-mode]
  (let [parse-opts (cond
                     needs-parinfer-init? {:mode :paren} ;; see test: fix-bad-indentation
                     (= 'INSERT vim-mode) {:mode :smart :cursor-line cursor-line :cursor-column cursor-column}
                     :else {:mode :indent})
        parsed-code (ps/parse (str/join "\n" lines) parse-opts)]
    (assoc buffer
      :parsed-code parsed-code
      :needs-parinfer? true
      :needs-parinfer-init? false)))

(defn update-clojure-buffer [{:keys [text-entity parsed-code lines] :as buffer} {:keys [base-font-entity font-height] :as constants}]
  (let [text-entity (clojurify-lines text-entity base-font-entity parsed-code false)
        parinfer-text-entity (clojurify-lines text-entity base-font-entity parsed-code true)]
    (assoc buffer
      :text-entity (chars/update-uniforms text-entity font-height colors/text-alpha)
      :parinfer-text-entity (chars/update-uniforms parinfer-text-entity font-height colors/parinfer-alpha))))

(defn update-text-buffer [{:keys [lines] :as buffer} {:keys [base-font-entity font-height] :as constants} new-lines first-line line-count-change]
  (-> buffer
      (assoc :lines (update-lines lines new-lines first-line line-count-change))
      (update :text-entity
              (fn [text-entity]
                (-> text-entity
                    (replace-lines base-font-entity new-lines first-line line-count-change)
                    (chars/update-uniforms font-height colors/text-alpha))))))

(defn get-visible-lines [{:keys [characters] :as text-entity}
                         {:keys [font-height] :as constants}
                         {:keys [top bottom] :as text-box}
                         game-height
                         camera-y
                         font-size]
  (let [text-height (- (bottom game-height font-size)
                       (top game-height font-size))
        char-height (* font-height font-size)
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

(defn ->cursor-entity [vim-mode {:keys [font-width font-height base-rect-entity] :as constants} line-chars line column font-size]
  (let [left-char (get line-chars (dec column))
        curr-char (get line-chars column)
        {:keys [left width height]} curr-char
        width (cond-> (or width font-width)
                      ('#{INSERT COMMAND_LINE} vim-mode)
                      (/ 4))
        left (or left
                 (some-> (:left left-char)
                         (+ (:width left-char)))
                 0)
        top (* line font-height)
        height (or height font-height)]
    (-> base-rect-entity
        (t/color colors/cursor-color)
        (t/translate left top)
        (t/scale width height)
        (assoc :left (* left font-size)
               :top (* top font-size)
               :width (* width font-size)
               :height (* height font-size)))))

(defn adjust-camera [buffer camera-x camera-y font-size text-box {:keys [font-width font-height] :as constants} window]
  (let [{game-width :width game-height :height} window
        text-top ((:top text-box) game-height font-size)
        text-bottom ((:bottom text-box) game-height font-size)
        char-counts (get-in buffer [:text-entity :uniforms 'u_char_counts])
        max-line-count (if (seq char-counts)
                         (apply max char-counts)
                         0)
        text-width (* max-line-count font-size font-width)
        text-height (* (count char-counts) font-size font-height)
        max-x (- text-width game-width)
        max-y (- text-height (- text-bottom text-top))]
    [(-> camera-x (min max-x) (max 0))
     (-> camera-y (min max-y) (max 0))]))

(defn update-camera [buffer camera-x camera-y font-size text-box window]
  (let [{game-height :height} window
        text-top ((:top text-box) game-height font-size)]
    (assoc buffer
      :camera (t/translate constants/orig-camera camera-x (- camera-y text-top))
      :camera-x camera-x
      :camera-y camera-y
      :camera-target-x camera-x
      :camera-target-y camera-y)))

(defn update-cursor [{:keys [text-entity cursor-line cursor-column tab-id] :as buffer} vim-mode font-size text-box {:keys [base-rects-entity] :as constants} window]
  (let [line-chars (get-in buffer [:text-entity :characters cursor-line])
        {:keys [left top width height] :as cursor-entity} (->cursor-entity vim-mode constants line-chars cursor-line cursor-column font-size)]
    (-> buffer
        (assoc :rects-entity (-> base-rects-entity
                                 (i/assoc 0 cursor-entity)
                                 (assoc :rect-count 1)))
        (as-> buffer
              (let [{:keys [camera camera-x camera-y]} buffer
                    {game-width :width game-height :height} window
                    text-top ((:top text-box) game-height font-size)
                    text-bottom ((:bottom text-box) game-height font-size)
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
                               camera-y)]
                (assoc buffer
                  :camera (t/translate constants/orig-camera camera-x (- camera-y text-top))
                  :camera-x camera-x
                  :camera-y camera-y
                  :camera-target-x camera-x
                  :camera-target-y camera-y))))))

(defn range->rects [text-entity font-width font-height {:keys [start-line start-column end-line end-column] :as rect-range}]
  (vec (for [line-num (range start-line (inc end-line))
             :let [line-chars (-> text-entity :characters (get line-num))]
             :when line-chars]
         (let [start-column (if (= line-num start-line) start-column 0)
               end-column (if (= line-num end-line) end-column (count line-chars))]
           {:left (* font-width start-column)
            ;; to support variable width fonts, we would need...
            ;; :left (->> (subvec line-chars 0 start-column) (map :width) (reduce +))
            :top (* line-num font-height)
            :width (* font-width (- end-column start-column))
            ;; to support variable width fonts, we would need...
            ;; :width (->> (subvec line-chars start-column end-column) (map :width) (reduce +))
            :height font-height}))))

(defn range->text [buffer {:keys [start-line start-column end-line end-column] :as rect-range}]
  (vec (for [line-num (range start-line (inc end-line))]
         (let [line (-> buffer :lines (nth line-num))
               start-column (if (= line-num start-line) start-column 0)
               end-column (if (= line-num end-line) end-column (count line))]
           (subs line start-column (min end-column (count line)))))))

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

(defn update-highlight [{:keys [text-entity cursor-line cursor-column] :as buffer} {:keys [font-width font-height base-rect-entity] :as constants}]
  (if-let [coll (->> (:collections text-entity)
                     (filter (fn [{:keys [start-line start-column end-line end-column]}]
                               (and (or (< start-line cursor-line)
                                        (and (= start-line cursor-line)
                                             (<= start-column cursor-column)))
                                    (or (> end-line cursor-line)
                                        (and (= end-line cursor-line)
                                             (> end-column cursor-column))))))
                     first)]
    (let [color (colors/set-alpha (colors/get-color :delimiter (:depth coll)) colors/highlight-alpha)
          rects (range->rects text-entity font-width font-height coll)]
      (update buffer :rects-entity assoc-rects base-rect-entity color rects))
    buffer))

(defn update-selection [{:keys [text-entity] :as buffer} {:keys [font-width font-height base-rect-entity] :as constants} visual-range]
  (if visual-range
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
      (-> buffer
          (update :rects-entity assoc-rects base-rect-entity colors/select-color rects)
          (assoc :selected-text (not-empty (str/join \newline (range->text buffer visual-range))))))
    (assoc buffer :selected-text nil)))

(defn update-search-highlights [{:keys [text-entity] :as buffer} {:keys [font-width font-height base-rect-entity] :as constants} highlights]
  (let [rects (vec (mapcat (partial range->rects text-entity font-width font-height) highlights))]
    (update buffer :rects-entity assoc-rects base-rect-entity colors/search-color rects)))

