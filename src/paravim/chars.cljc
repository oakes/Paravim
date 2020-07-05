(ns paravim.chars
  (:require [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]
            [paravim.constants :as constants]
            [com.rpl.specter :as specter]
            [clojure.core.rrb-vector :as rrb]))

(def instanced-font-vertex-shader
  {:inputs
   '{a_position vec2
     a_color vec4
     a_translate_matrix mat3
     a_texture_matrix mat3
     a_scale_matrix mat3}
   :uniforms
   {'u_matrix 'mat3
    'u_char_counts ['int constants/max-visible-lines]
    'u_start_line 'int
    'u_start_column 'int
    'u_font_height 'float}
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
             ("if" (> total_char_count gl_InstanceID)
               (-= total_char_count [u_char_counts i])
               "break")
             ("else"
               (+= current_line 1)))
           (=mat3 translate_matrix a_translate_matrix)
           (+= [translate_matrix 2 1] (* u_font_height (+ u_start_line current_line)))
           (=int current_column (- gl_InstanceID total_char_count))
           ("if" (> u_start_column current_column)
             (= v_color (vec4 "0.0" "0.0" "0.0" "0.0"))
             "return")
           (= gl_Position
              (vec4
                (.xy (* u_matrix
                        translate_matrix
                        a_scale_matrix
                        (vec3 a_position 1)))
                0 1))
           (= v_tex_coord (.xy (* a_texture_matrix (vec3 a_position 1))))
           (= v_color a_color))}})

(def instanced-font-fragment-shader
  {:precision "mediump float"
   :uniforms
   '{u_image sampler2D
     u_alpha float
     u_show_blocks int}
   :inputs
   '{v_tex_coord vec2
     v_color vec4}
   :outputs
   '{o_color vec4}
   :signatures
   '{main ([] void)}
   :functions
   '{main ([]
           ;; get the color from the attributes
           (=vec4 input_color v_color)
           ;; set its alpha color if necessary
           ("if" (== (.w input_color) "1.0")
             (= (.w input_color) u_alpha))
           ;; render the chars as blocks for the minimap
           ("if" (== u_show_blocks 1)
             (= o_color input_color)
             "return")
           ;; get the color from the texture
           (= o_color (texture u_image v_tex_coord))
           ;; if it's black, make it a transparent pixel
           ("if" (== (.rgb o_color) (vec3 "0.0" "0.0" "0.0"))
             (= o_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ;; otherwise, use the input color
           ("else"
             (= o_color input_color))
           ;; the size of one pixel
           (=vec2 one_pixel (/ (vec2 1) (vec2 (textureSize u_image 0))))
           ;; left
           (=vec4 left_color (texture u_image (+ v_tex_coord (vec2 (.x one_pixel) "0.0"))))
           ("if" (== (.rgb left_color) (vec3 "0.0" "0.0" "0.0"))
             (= left_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= left_color input_color))
           ;; right
           (=vec4 right_color (texture u_image (+ v_tex_coord (vec2 (- 0 (.x one_pixel)) "0.0"))))
           ("if" (== (.rgb right_color) (vec3 "0.0" "0.0" "0.0"))
             (= right_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= right_color input_color))
           ;; top
           (=vec4 top_color (texture u_image (+ v_tex_coord (vec2 "0.0" (.y one_pixel)))))
           ("if" (== (.rgb top_color) (vec3 "0.0" "0.0" "0.0"))
             (= top_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= top_color input_color))
           ;; bottom
           (=vec4 bottom_color (texture u_image (+ v_tex_coord (vec2 "0.0" (- 0 (.y one_pixel))))))
           ("if" (== (.rgb bottom_color) (vec3 "0.0" "0.0" "0.0"))
             (= bottom_color (vec4 "0.0" "0.0" "0.0" "0.0")))
           ("else"
             (= bottom_color input_color))
           ;; average
           (= o_color
             (/ (+ o_color left_color right_color top_color bottom_color)
                "5.0"))
           ;; discard transparent pixels
           ("if" (== (.w o_color) "0.0")
             "discard"))}})

(defn update-uniforms [{:keys [characters] :as text-entity} font-height alpha]
  (update text-entity :uniforms assoc
      'u_char_counts (mapv count characters)
      'u_font_height font-height
      'u_alpha alpha
      'u_start_line 0
      'u_start_column 0
      'u_show_blocks 0))

(def ^:const instanced-font-attrs->unis
  '{a_translate_matrix u_translate_matrix
    a_scale_matrix u_scale_matrix
    a_texture_matrix u_texture_matrix
    a_color u_color})

(defn- replace-instance-attr [start-index end-index entities instanced-entity attr-name uni-name]
  (let [new-data (into [] (specter/traverse-all [:uniforms uni-name specter/ALL]) entities)
        data-len (specter/select-any [:attribute-lengths attr-name] instanced-entity)
        start-offset (* start-index data-len)
        end-offset (* end-index data-len)]
    (update-in instanced-entity [:attributes attr-name]
               (fn [attr]
                 (if attr
                   (update attr :data
                     (fn [data]
                       (let [v1 (rrb/subvec data 0 start-offset)
                             v2 (rrb/subvec data end-offset)]
                         (rrb/catvec v1 new-data v2))))
                   {:data (vec new-data)
                    :divisor 1})))))

(defn- dissoc-instance-attr [start-index end-index instanced-entity attr-name]
  (if (= start-index end-index)
    instanced-entity
    (let [data-len (specter/select-any [:attribute-lengths attr-name] instanced-entity)
          start-offset (* start-index data-len)
          end-offset (* end-index data-len)]
      (update-in instanced-entity [:attributes attr-name]
                 (fn [attr]
                     (update attr :data
                             (fn [data]
                               (let [v1 (rrb/subvec data 0 start-offset)
                                     v2 (rrb/subvec data end-offset)]
                                 (rrb/catvec v1 v2)))))))))

(defn- assoc-line* [instanced-entity i entities]
  (let [characters (:characters instanced-entity)
        prev-lines (subvec characters 0 i)
        prev-count (reduce + 0 (map count prev-lines))
        curr-count (count (get characters i))
        total-count (+ prev-count curr-count)]
    (reduce-kv
      (partial replace-instance-attr prev-count total-count entities)
      instanced-entity
      instanced-font-attrs->unis)))

(defn- insert-line* [instanced-entity i entities]
  (let [characters (:characters instanced-entity)
        prev-lines (subvec characters 0 i)
        prev-count (reduce + 0 (map count prev-lines))]
    (reduce-kv
      (partial replace-instance-attr prev-count prev-count entities)
      instanced-entity
      instanced-font-attrs->unis)))

(defn- dissoc-line* [instanced-entity i]
  (let [characters (:characters instanced-entity)
        prev-lines (subvec characters 0 i)
        prev-count (reduce + 0 (map count prev-lines))
        curr-count (count (get characters i))
        total-count (+ prev-count curr-count)]
    (reduce
      (partial dissoc-instance-attr prev-count total-count)
      instanced-entity
      (keys instanced-font-attrs->unis))))

(defn- get-char-code [ch first-char]
  (- #?(:clj (int ch) :cljs (.charCodeAt ch 0)) first-char))

(def crop-char
  (memoize
    (fn [{:keys [baked-font] :as font-entity} ch]
      (let [{:keys [baked-chars baseline first-char]} baked-font
            char-code (get-char-code ch first-char)
            baked-char (or (get baked-chars char-code)
                           (nth baked-chars (get-char-code \space first-char)))
            baked-char (cond-> baked-char (= ch \tab) (update :xadv * 2))
            {:keys [x y w h xoff yoff]} baked-char]
        (-> font-entity
            (t/crop x y w h)
            (->> (specter/setval [:uniforms 'u_scale_matrix] (m/scaling-matrix w h))
                 (specter/setval [:uniforms 'u_translate_matrix] (m/translation-matrix xoff (+ baseline yoff))))
            (assoc :baked-char baked-char :character ch))))))

(defn assoc-char
  ([text-entity index char-entity]
   (assoc-char text-entity 0 index char-entity))
  ([{:keys [baked-font characters] :as text-entity} line-num index {:keys [baked-char] :as char-entity}]
   (let [characters (loop [chars characters]
                      (if (<= (count chars) line-num)
                        (recur (conj chars []))
                        chars))
         line (get characters line-num)
         prev-char (get line (dec index))
         prev-x-total (or (:x-total prev-char) 0)
         x-total (+ (:xadv baked-char) prev-x-total)
         line (assoc line index (-> char-entity
                                    (assoc :x-total x-total
                                           :left prev-x-total
                                           :width (:xadv baked-char)
                                           :height (:font-height baked-font))))]
     (assoc text-entity :characters (assoc characters line-num line)))))

(defn translate [matrix x y]
  (m/multiply-matrices 3 (m/translation-matrix x y) matrix))

(defn update-translation-matrix [entity x y]
  (update-in entity [:uniforms 'u_translate_matrix] translate x y))

(defn assoc-line [text-entity line-num char-entities]
  (let [new-text-entity (specter/setval [:characters line-num] [] text-entity)
        new-text-entity (if (seq char-entities)
                          (reduce-kv
                            (fn [new-text-entity char-num char-entity]
                              (assoc-char new-text-entity line-num char-num char-entity))
                            new-text-entity
                            char-entities)
                          new-text-entity)
        new-line (specter/select-any [:characters line-num] new-text-entity)
        adjusted-new-line (specter/transform
                            [specter/ALL (specter/collect-one :left) :uniforms 'u_translate_matrix]
                            (fn [left matrix]
                              (translate matrix left 0))
                            new-line)]
    (-> text-entity
        (assoc-line* line-num adjusted-new-line)
        (->> (specter/setval [:characters line-num] new-line)))))

(defn insert-line [{:keys [characters] :as text-entity} line-num char-entities]
  (let [new-characters (rrb/catvec
                         (rrb/subvec characters 0 line-num)
                         [[]]
                         (rrb/subvec characters line-num))
        text-entity (assoc text-entity :characters new-characters)]
    (assoc-line text-entity line-num char-entities)))

(defn dissoc-line [text-entity line-num]
  (-> text-entity
      (dissoc-line* line-num)
      (update :characters (fn [characters]
                            (let [v1 (rrb/subvec characters 0 line-num)
                                  v2 (rrb/subvec characters (inc line-num))]
                              (rrb/catvec v1 v2))))))

