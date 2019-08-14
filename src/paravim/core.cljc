(ns paravim.core
  (:require [paravim.utils :as utils]
            [paravim.chars :as chars]
            [html-soup.core :as hs]
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

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :current-buffer nil
                       :buffers {}}))

(def text-color [1 1 1 1])

(def colors {"number" [1 1 0 1]
             "string" [1 0 0 1]
             "keyword" [0 0 1 1]})

(def rainbow-colors [[0 1 1 1] ;; aqua
                     [1 (/ 165 255) 0 1] ;; orange
                     [(/ 250 255) (/ 128 255) (/ 114 255) 1] ;; salmon
                     [1 0 1 1] ;; fuchsia
                     [0 1 0 1] ;; lime
                     ])

(defn get-color [class-name depth]
  (or (colors class-name)
      (case class-name
        "delimiter" (nth rainbow-colors (mod depth (count rainbow-colors)))
        text-color)))

(defn assoc-lines [text-entity font-entity lines]
  (reduce-kv
    (fn [entity line-num line]
      (chars/assoc-line entity line-num (mapv #(chars/crop-char font-entity %) line)))
    text-entity
    lines))

(defn clojurify-lines
  ([text-entity lines]
   (->> (hs/code->hiccup (str/join "\n" lines))
        (clojurify-lines (:characters text-entity) (volatile! 0) (volatile! 0) nil -1)
        (reduce-kv
          (fn [entity line-num char-entities]
            (if (not= (get-in entity [:characters line-num]) char-entities)
              (chars/assoc-line entity line-num char-entities)
              entity))
          text-entity)))
  ([characters *line-num *char-num class-name depth hiccup]
   (cond
     (vector? hiccup)
     (let [[_ attr-map & children] hiccup
           class-name (:class attr-map)
           depth (cond-> depth
                         (some-> class-name (str/starts-with? "collection"))
                         inc)]
       (reduce
         (fn [characters child]
           (clojurify-lines characters *line-num *char-num class-name depth child))
         characters
         children))
     (= "\n" hiccup)
     (do
       (vswap! *line-num inc)
       (vreset! *char-num 0)
       characters)
     (string? hiccup)
     (let [color (get-color class-name depth)
           char-num @*char-num]
       (update characters @*line-num
         (fn [char-entities]
           (reduce
             (fn [char-entities i]
               (update char-entities i t/color color))
             char-entities
             (range char-num (vswap! *char-num + (count hiccup))))))))))

(defn replace-lines [{:keys [characters] :as text-entity} font-entity new-lines first-line line-count-change]
  (let [new-chars (mapv
                    (fn [line]
                      (mapv
                        (fn [ch]
                          (chars/crop-char font-entity ch))
                        line))
                    new-lines)]
    (if (= 0 line-count-change)
      (reduce-kv
        (fn [entity line-offset char-entities]
          (chars/assoc-line entity (+ first-line line-offset) char-entities))
        text-entity
        new-chars)
      (let [chars-before (subvec characters 0 first-line)
            lines-to-remove (if (neg? line-count-change)
                              (* -1 line-count-change)
                              0)
            lines-to-add (vec (repeat line-count-change []))
            chars-after (subvec characters (+ first-line lines-to-remove))
            new-characters (->> chars-after
                                (into lines-to-add)
                                (into chars-before)
                                (into []))
            new-characters (reduce
                             (fn [new-characters i]
                               (assoc new-characters (+ first-line i) (nth new-chars i)))
                             new-characters
                             (range (count new-chars)))]
        (reduce
          (fn [entity line-num]
            (if-let [char-entity (get new-characters line-num)]
              (chars/assoc-line entity line-num char-entity)
              (chars/dissoc-line entity (count new-characters))))
          text-entity
          (range first-line (max (count new-characters) (count characters))))))))

(defn ->cursor-entity [{:keys [font-width font-height base-rect-entity] :as state} line-chars line column]
  (let [left-char (get line-chars (dec column))
        curr-char (get line-chars column)
        {:keys [left top width height]} curr-char
        width (or width font-width)
        left (or left
                 (some-> (:left left-char)
                         (+ (:width left-char)))
                 0)
        top (or top (* line font-height))
        height (or height font-height)]
    (-> base-rect-entity
        (t/color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
        (t/translate left top)
        (t/scale width height)
        (assoc :left left :top top :width width :height height))))

(defn update-cursor [{:keys [font-height] :as state} game buffer-ptr line column]
  (update-in state [:buffers buffer-ptr]
    (fn [buffer]
      (let [line-chars (get-in buffer [:text-entity :characters line])
            {:keys [left top width height] :as cursor-entity} (->cursor-entity state line-chars line column)]
        (-> buffer
            (update :rects-entity #(i/assoc % 0 cursor-entity))
            (as-> buffer
                  (let [{:keys [camera camera-x camera-y]} buffer
                        cursor-bottom (+ top font-height)
                        cursor-right (+ left width)
                        game-width (utils/get-width game)
                        game-height (- (utils/get-height game) font-height)
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

(defn assoc-buffer [{:keys [base-font-entity base-text-entity base-rects-entity] :as state} buffer-ptr path lines]
  (assoc-in state [:buffers buffer-ptr]
    {:text-entity (cond-> (assoc-lines base-text-entity base-font-entity lines)
                          (clojure-exts (get-extension path))
                          (clojurify-lines lines))
     :rects-entity base-rects-entity
     :camera (t/translate orig-camera 0 0)
     :camera-x 0
     :camera-y 0
     :path path}))

(defn modify-buffer [{:keys [base-font-entity] :as state} game buffer-ptr lines first-line line-count-change all-lines]
  (update-in state [:buffers buffer-ptr]
    (fn [{:keys [text-entity path] :as buffer}]
      (assoc buffer :text-entity
        (cond-> (replace-lines text-entity base-font-entity lines first-line line-count-change)
                (clojure-exts (get-extension path))
                (clojurify-lines all-lines))))))

(defn init [game callback]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs)
     (fn [{:keys [data]} baked-font]
       (let [font-entity (t/color (text/->font-entity game data baked-font) text-color)
             text-entity (c/compile game (i/->instanced-entity font-entity))
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
                         (t/camera camera)))
      (c/render game (-> text-entity
                         (t/project game-width game-height)
                         (t/camera camera))))
    (when command-bg-entity
      (c/render game (-> command-bg-entity
                         (t/project game-width game-height)
                         (t/translate 0 (- game-height font-height))
                         (t/scale game-width font-height)))
      (when (and (= mode 'COMMAND_LINE)
                 command-rects-entity
                 command-text-entity)
        (c/render game (-> command-rects-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height font-height))))
        (c/render game (-> command-text-entity
                           (t/project game-width game-height)
                           (t/translate 0 (- game-height font-height)))))))
  ;; return the game map
  game)

