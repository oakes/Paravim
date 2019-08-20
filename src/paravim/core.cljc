(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [paren-salsa.core :as ps]
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
(def bg-color [(/ 0 255) (/ 16 255) (/ 64 255) 0.9])
(def font-size-multiplier (/ 1 2))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :current-buffer nil
                       :buffers {}}))

(def text-color [1 1 1 1])

(def colors {:number [1 1 0 1]
             :string [1 0 0 1]
             :keyword [0 0 1 1]})

(def rainbow-colors [[0 1 1 1] ;; aqua
                     [1 (/ 165 255) 0 1] ;; orange
                     [(/ 250 255) (/ 128 255) (/ 114 255) 1] ;; salmon
                     [1 0 1 1] ;; fuchsia
                     [0 1 0 1] ;; lime
                     ])

(defn get-color [class-name depth]
  (or (colors class-name)
      (case class-name
        :delimiter (nth rainbow-colors (mod depth (count rainbow-colors)))
        text-color)))

(defn assoc-lines [text-entity font-entity lines]
  (reduce-kv
    (fn [entity line-num line]
      (chars/assoc-line entity line-num (mapv #(chars/crop-char font-entity %) line)))
    text-entity
    lines))

(defn clojurify-lines
  ([text-entity lines]
   (let [*line-num (volatile! 0)
         *char-num (volatile! -1)]
     (->> (ps/parse (str/join "\n" lines))
          (reduce
            (fn [characters data]
              (clojurify-lines characters *line-num *char-num nil -1 data))
            (:characters text-entity))
          (reduce-kv
            (fn [entity line-num char-entities]
              (if (not= (get-in entity [:characters line-num]) char-entities)
                (chars/assoc-line entity line-num char-entities)
                entity))
            text-entity))))
  ([characters *line-num *char-num class-name depth data]
   (if (vector? data)
     (let [[class-name & children] data
           depth (cond-> depth
                         (= class-name :collection)
                         inc)]
       (reduce
         (fn [characters child]
           (clojurify-lines characters *line-num *char-num class-name depth child))
         characters
         children))
     (let [color (get-color class-name depth)]
       (reduce
         (fn [characters ch]
           (if (= ch \newline)
             (do
               (vswap! *line-num inc)
               (vreset! *char-num -1)
               characters)
             (update characters @*line-num
               (fn [char-entities]
                 (update char-entities (vswap! *char-num inc) t/color color)))))
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
        (t/color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
        (t/translate left top)
        (t/scale width height)
        (assoc :left (* left font-size-multiplier)
               :top (* top font-size-multiplier)
               :width (* width font-size-multiplier)
               :height (* height font-size-multiplier)))))

(defn update-cursor [{:keys [font-height] :as state} game buffer-ptr line column]
  (update-in state [:buffers buffer-ptr]
    (fn [buffer]
      (let [line-chars (get-in buffer [:text-entity :characters line])
            {:keys [left top width height] :as cursor-entity} (->cursor-entity state line-chars line column)]
        (-> buffer
            (update :rects-entity #(i/assoc % 0 cursor-entity))
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

(defn get-extension
  [path]
  (some->> (str/last-index-of path ".")
           (+ 1)
           (subs path)
           str/lower-case))

(def clojure-exts #{"clj" "cljs" "cljc" "edn"})

(defn update-uniforms [text-entity font-height lines]
  (-> text-entity
      (assoc-in [:uniforms 'u_char_counts] (mapv count lines))
      (assoc-in [:uniforms 'u_font_height] font-height)))

(defn assoc-buffer [{:keys [base-font-entity base-text-entity base-rects-entity font-height] :as state} buffer-ptr path lines]
  (assoc-in state [:buffers buffer-ptr]
    {:text-entity (cond-> (assoc-lines base-text-entity base-font-entity lines)
                          true
                          (update-uniforms font-height lines)
                          (clojure-exts (get-extension path))
                          (clojurify-lines lines))
     :rects-entity base-rects-entity
     :camera (t/translate orig-camera 0 0)
     :camera-x 0
     :camera-y 0
     :path path
     :lines lines}))

(defn modify-buffer [{:keys [base-font-entity font-height] :as state} game buffer-ptr new-lines first-line line-count-change]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [text-entity path lines] :as buffer}]
      (let [lines (update-lines lines new-lines first-line line-count-change)]
        (assoc buffer
          :lines lines
          :text-entity
          (cond-> (replace-lines text-entity base-font-entity new-lines first-line line-count-change)
                  true
                  (update-uniforms font-height lines)
                  (clojure-exts (get-extension path))
                  (clojurify-lines lines)))))))

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
           ("for" "(int i=0; i<1024; ++i)"
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
                                                :vertex instanced-font-vertex-shader))
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
    (when-let [{:keys [rects-entity text-entity camera]} (get buffers current-buffer)]
      (c/render game (-> rects-entity
                         (t/project game-width game-height)
                         (t/camera camera)
                         (t/scale font-size-multiplier font-size-multiplier)))
      (c/render game (-> text-entity
                         (t/project game-width game-height)
                         (t/camera camera)
                         (t/scale font-size-multiplier font-size-multiplier))))
    (when command-bg-entity
      (c/render game (-> command-bg-entity
                         (t/project game-width game-height)
                         (t/translate 0 (- game-height (* font-size-multiplier font-height)))
                         (t/scale game-width font-height)))
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

