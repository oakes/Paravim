(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [parinferish.core :as ps]
            [clojure.string :as str]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.text :as text]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.primitives-2d :as primitives]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [paravim.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [paravim.text :refer [load-font-cljs]])))

(def orig-camera (e/->camera true))
(def bg-color [(/ 42 255) (/ 34 255) (/ 36 255) 0.95])
(def font-size-multiplier (/ 1 2))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :current-buffer nil
                       :buffers {}
                       :buffer-updates []}))

(def text-color [1 1 1 1])
(def cursor-color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
(def select-color [(/ 148 255) (/ 69 255) (/ 5 255) 0.5])

(def colors {:number [(/ 255 255) (/ 193 255) (/ 94 255) 1]
             :string [(/ 209 255) (/ 153 255) (/ 101 255) 1]
             :keyword [(/ 86 255) (/ 181 255) (/ 194 255) 1]
             :comment [(/ 150 255) (/ 129 255) (/ 133 255) 1]})

(def rainbow-colors [[(/ 220 255) (/ 103 255) (/ 44 255) 1]
                     [(/ 223 255) (/ 107 255) (/ 117 255) 1]
                     [(/ 199 255) (/ 116 255) (/ 151 255) 1]
                     [(/ 65 255) (/ 174 255) (/ 122 255) 1]])

(defn get-color [class-name depth]
  (or (colors class-name)
      (case class-name
        :delimiter (nth rainbow-colors (mod depth (count rainbow-colors)))
        text-color)))

(defn set-alpha [color alpha]
  (assoc color 3 alpha))

(defn assoc-lines [text-entity font-entity lines]
  (reduce-kv
    (fn [entity line-num line]
      (chars/assoc-line entity line-num (mapv #(chars/crop-char font-entity %) line)))
    text-entity
    lines))

(defn clojurify-lines
  ([text-entity font-entity parsed-code parinfer?]
   (let [*line-num (volatile! 0)
         *char-num (volatile! -1)
         *collections (volatile! [])]
     (as-> parsed-code $
           (reduce
             (fn [characters data]
               (clojurify-lines characters font-entity *line-num *char-num *collections nil -1 data parinfer?))
             (:characters text-entity)
             $)
           (reduce-kv
             (fn [entity line-num char-entities]
               (if (not= (get-in entity [:characters line-num]) char-entities)
                 (chars/assoc-line entity line-num char-entities)
                 entity))
             text-entity
             $)
           (assoc $ :collections @*collections))))
  ([characters font-entity *line-num *char-num *collections class-name depth data parinfer?]
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
                            (clojurify-lines characters font-entity *line-num *char-num *collections class-name depth child parinfer?))
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
             (let [line-num @*line-num
                   char-num (inc @*char-num)]
               (vswap! *line-num inc)
               (vreset! *char-num -1)
               (if parinfer?
                 (update characters line-num
                   (fn [char-entities]
                     (if (not= (count char-entities) char-num)
                       (into [] (subvec char-entities 0 char-num))
                       char-entities)))
                 characters))
             (update characters @*line-num
               (fn [char-entities]
                 (update char-entities (vswap! *char-num inc)
                   (fn [entity]
                     (-> (if (and parinfer?
                                  (-> entity :character (not= ch)))
                           (chars/crop-char font-entity ch)
                           entity)
                         (t/color (cond-> color parinfer? (set-alpha (if (= class-name :delimiter) 0.25 0.05)))))))))))
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
                            (- (count new-lines) line-count-change))
          lines (->> (subvec lines (+ first-line lines-to-remove))
                     (into (subvec lines 0 first-line))
                     (into []))
          lines (->> (subvec lines first-line)
                     (into new-lines)
                     (into (subvec lines 0 first-line))
                     (into []))]
      lines)))

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
            text-entity (reduce
                          (fn [text-entity _]
                            (chars/dissoc-line text-entity first-line))
                          text-entity
                          (range 0 lines-to-remove))
            text-entity (reduce-kv
                          (fn [text-entity line-offset char-entities]
                            (chars/insert-line text-entity (+ first-line line-offset) char-entities))
                          text-entity
                          new-chars)]
        text-entity))))

(defn ->cursor-entity [{:keys [font-width font-height base-rect-entity] :as state} line-chars line column]
  (let [left-char (get line-chars (dec column))
        curr-char (get line-chars column)
        {:keys [left width height]} curr-char
        width (or width font-width)
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

(defn update-cursor [{:keys [font-height base-rects-entity] :as state} game buffer-ptr]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [cursor-line cursor-column] :as buffer}]
      (let [line-chars (get-in buffer [:text-entity :characters cursor-line])
            {:keys [left top width height] :as cursor-entity} (->cursor-entity state line-chars cursor-line cursor-column)]
        (-> buffer
            (assoc :rects-entity (-> base-rects-entity
                                     (i/assoc 0 cursor-entity)
                                     (assoc :rect-count 1)))
            (as-> buffer
                  (let [{:keys [camera camera-x camera-y]} buffer
                        cursor-bottom (+ top height)
                        cursor-right (+ left width)
                        game-width (utils/get-width game)
                        game-height (- (utils/get-height game) height)
                        camera-bottom (+ camera-y game-height)
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
                                   (> cursor-bottom camera-bottom)
                                   (- cursor-bottom game-height)
                                   :else
                                   camera-y)]
                    (assoc buffer
                      :camera (t/translate orig-camera camera-x camera-y)
                      :camera-x camera-x
                      :camera-y camera-y))))))))

(defn update-command [{:keys [base-text-entity base-font-entity base-rects-entity] :as state} text position]
  (let [command-text-entity (when text
                              (chars/assoc-line base-text-entity 0 (mapv #(chars/crop-char base-font-entity %) (str ":" text))))
        command-rects-entity (when text
                               (let [line-chars (get-in command-text-entity [:characters 0])]
                                 (-> base-rects-entity
                                     (i/assoc 0 (->cursor-entity state line-chars 0 (inc position))))))]
    (assoc state
      :command-text text
      :command-text-entity command-text-entity
      :command-rects-entity command-rects-entity)))

(defn range->rects [text-entity font-height {:keys [start-line start-column end-line end-column] :as rect-range}]
  (let [{:keys [start-line start-column end-line end-column]}
        (if (or (> start-line end-line)
                (and (= start-line end-line)
                     (> start-column end-column)))
          {:start-line end-line
           :start-column end-column
           :end-line start-line
           :end-column start-column}
          rect-range)]
    (vec (for [line-num (range start-line (inc end-line))]
           (let [line-chars (-> text-entity :characters (nth line-num))
                 start-column (if (= line-num start-line) start-column 0)
                 end-column (if (= line-num end-line) end-column (count line-chars))
                 empty-columns (subvec line-chars 0 start-column)
                 filled-columns (subvec line-chars start-column end-column)]
             {:left (->> empty-columns (map :width) (reduce +))
              :top (* line-num font-height)
              :width (->> filled-columns (map :width) (reduce +))
              :height font-height})))))

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

(defn update-highlight [{:keys [font-height base-rect-entity] :as state} buffer-ptr]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [text-entity cursor-line cursor-column] :as buffer}]
      (if-let [coll (->> (:collections text-entity)
                         (filter (fn [{:keys [start-line start-column end-line end-column]}]
                                   (and (or (< start-line cursor-line)
                                            (and (= start-line cursor-line)
                                                 (<= start-column cursor-column)))
                                        (or (> end-line cursor-line)
                                            (and (= end-line cursor-line)
                                                 (> end-column cursor-column))))))
                         first)]
        (let [color (assoc (get-color :delimiter (:depth coll)) 3 0.05)
              rects (range->rects text-entity font-height coll)]
          (update buffer :rects-entity assoc-rects base-rect-entity color rects))
        buffer))))

(defn update-selection [{:keys [font-height base-rect-entity] :as state} buffer-ptr visual-range]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [text-entity] :as buffer}]
      (let [rects (range->rects text-entity font-height visual-range)]
        (update buffer :rects-entity assoc-rects base-rect-entity select-color rects)))))

(defn get-extension
  [path]
  (some->> (str/last-index-of path ".")
           (+ 1)
           (subs path)
           str/lower-case))

(def clojure-exts #{"clj" "cljs" "cljc" "edn"})

(defn update-uniforms [{:keys [characters] :as text-entity} font-height]
  (-> text-entity
      (assoc-in [:uniforms 'u_char_counts] (mapv count characters))
      (assoc-in [:uniforms 'u_font_height] font-height)))

(defn assoc-buffer [{:keys [base-font-entity base-text-entity font-height] :as state} buffer-ptr path lines]
  (let [text-entity (assoc-lines base-text-entity base-font-entity lines)
        parsed-code (when (clojure-exts (get-extension path))
                      (ps/parse (str/join "\n" lines)))]
    (assoc-in state [:buffers buffer-ptr]
      {:text-entity (cond-> text-entity
                            parsed-code
                            (clojurify-lines base-font-entity parsed-code false)
                            true
                            (update-uniforms font-height))
       :parinfer-text-entity (when parsed-code
                               (-> text-entity
                                   (clojurify-lines base-font-entity parsed-code true)
                                   (update-uniforms font-height)))
       :camera (t/translate orig-camera 0 0)
       :camera-x 0
       :camera-y 0
       :path path
       :lines lines
       :parsed-code parsed-code})))

(defn modify-buffer [{:keys [base-font-entity font-height] :as state} game buffer-ptr new-lines first-line line-count-change]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [text-entity parinfer-text-entity path lines cursor-line cursor-column] :as buffer}]
      (let [lines (update-lines lines new-lines first-line line-count-change)
            parsed-code (when (clojure-exts (get-extension path))
                          (ps/parse (str/join "\n" lines) {:mode :smart :cursor-line cursor-line :cursor-column cursor-column}))]
        (assoc buffer
          :lines lines
          :parsed-code parsed-code
          :needs-parinfer? (some? parsed-code)
          :text-entity
          (cond-> (replace-lines text-entity base-font-entity new-lines first-line line-count-change)
                  parsed-code
                  (clojurify-lines base-font-entity parsed-code false)
                  true
                  (update-uniforms font-height))
          :parinfer-text-entity
          (when parsed-code
            (-> (replace-lines parinfer-text-entity base-font-entity new-lines first-line line-count-change)
                (clojurify-lines base-font-entity parsed-code true)
                (update-uniforms font-height))))))))

(def ^:private instanced-font-vertex-shader
  {:inputs
   '{a_position vec2
     a_color vec4
     a_translate_matrix mat3
     a_texture_matrix mat3
     a_scale_matrix mat3}
   :uniforms
   '{u_matrix mat3
     u_char_counts [int 1000]
     u_font_height float}
   :outputs
   '{v_tex_coord vec2
     v_color vec4}
   :signatures
   '{main ([] void)}
   :functions
   '{main ([]
           (=int total_char_count 0)
           (=int current_line 0)
           ("for" "(int i=0; i<1000; ++i)"
             (+= total_char_count [u_char_counts i])
             ("if" (> total_char_count gl_InstanceID) "break")
             ("else" (+= current_line 1)))
           (=mat3 translate_matrix a_translate_matrix)
           (+= [translate_matrix 2 1] (* u_font_height current_line))
           (= gl_Position
              (vec4
                (.xy (* u_matrix
                        translate_matrix
                        a_scale_matrix
                        (vec3 a_position 1)))
                0 1))
           (= v_tex_coord (.xy (* a_texture_matrix (vec3 a_position 1))))
           (= v_color a_color))}})

(def ^:private instanced-font-fragment-shader
  {:precision "mediump float"
   :uniforms
   '{u_image sampler2D}
   :inputs
   '{v_tex_coord vec2
     v_color vec4}
   :outputs
   '{o_color vec4}
   :signatures
   '{main ([] void)}
   :functions
   '{main ([]
           (= o_color (texture u_image v_tex_coord))
           ("if" (== (.rgb o_color) (vec3 "0.0" "0.0" "0.0"))
             (= o_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= o_color v_color))
           ;; the size of one pixel
           (=vec2 one_pixel (/ (vec2 1) (vec2 (textureSize u_image 0))))
           ;; left
           (=vec4 left_color (texture u_image (+ v_tex_coord (vec2 (.x one_pixel) "0.0"))))
           ("if" (== (.rgb left_color) (vec3 "0.0" "0.0" "0.0"))
             (= left_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= left_color v_color))
           ;; right
           (=vec4 right_color (texture u_image (+ v_tex_coord (vec2 (- 0 (.x one_pixel)) "0.0"))))
           ("if" (== (.rgb right_color) (vec3 "0.0" "0.0" "0.0"))
             (= right_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= right_color v_color))
           ;; top
           (=vec4 top_color (texture u_image (+ v_tex_coord (vec2 "0.0" (.y one_pixel)))))
           ("if" (== (.rgb top_color) (vec3 "0.0" "0.0" "0.0"))
             (= top_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= top_color v_color))
           ;; bottom
           (=vec4 bottom_color (texture u_image (+ v_tex_coord (vec2 "0.0" (- 0 (.y one_pixel))))))
           ("if" (== (.rgb bottom_color) (vec3 "0.0" "0.0" "0.0"))
             (= bottom_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= bottom_color v_color))
           ;; average
           (= o_color
             (/ (+ o_color left_color right_color top_color bottom_color)
                "5.0"))
           ;; discard transparent pixels
           ("if" (== (.w o_color) "0.0")
             "discard"))}})

(defn init [game callback]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs)
     (fn [{:keys [data]} baked-font]
       (let [font-entity (-> (text/->font-entity game data baked-font)
                             (t/color text-color))
             text-entity (c/compile game (assoc (i/->instanced-entity font-entity)
                                                :vertex instanced-font-vertex-shader
                                                :fragment instanced-font-fragment-shader
                                                :characters []))
             rect-entity (e/->entity game primitives/rect)
             rects-entity (c/compile game (i/->instanced-entity rect-entity))
             command-bg-entity (c/compile game (t/color (e/->entity game primitives/rect) bg-color))]
         (swap! *state assoc
           :font-width (-> baked-font :baked-chars (nth (- 115 (:first-char baked-font))) :w)
           :font-height (:font-height baked-font)
           :base-font-entity font-entity
           :base-text-entity text-entity
           :base-rect-entity rect-entity
           :base-rects-entity rects-entity
           :command-bg-entity command-bg-entity)
         (callback)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color bg-color :depth 1}})

(defn tick [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [current-buffer buffers
                command-bg-entity command-text-entity command-rects-entity
                font-height mode]} @*state]
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    (when-let [{:keys [rects-entity text-entity parinfer-text-entity camera]} (get buffers current-buffer)]
      (c/render game (-> rects-entity
                         (t/project game-width game-height)
                         (t/camera camera)
                         (t/scale font-size-multiplier font-size-multiplier)))
      (when parinfer-text-entity
        (c/render game (-> parinfer-text-entity
                           (t/project game-width game-height)
                           (t/camera camera)
                           (t/scale font-size-multiplier font-size-multiplier))))
      (c/render game (-> text-entity
                         (t/project game-width game-height)
                         (t/camera camera)
                         (t/scale font-size-multiplier font-size-multiplier))))
    (when command-bg-entity
      (c/render game (-> command-bg-entity
                         (t/project game-width game-height)
                         (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                         (t/scale game-width (* font-size-multiplier font-height))))
      (when (and (= mode 'COMMAND_LINE)
                 command-rects-entity
                 command-text-entity)
        (c/render game (-> command-rects-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                           (t/scale font-size-multiplier font-size-multiplier)))
        (c/render game (-> command-text-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                           (t/scale font-size-multiplier font-size-multiplier))))))
  ;; return the game map
  game)

