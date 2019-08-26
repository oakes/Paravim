(ns paravim.chars
  (:require [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]
            [play-cljc.gl.utils :as u]))

(defn- replace-instance-attr [start-index end-index entities instanced-entity attr-name uni-name]
  (let [new-data (reduce
                   (fn [v entity]
                     (if (:program entity)
                       (throw (ex-info "Only uncompiled entities can be assoc'ed to an instanced entity" {}))
                       (into v (get-in entity [:uniforms uni-name]))))
                   []
                   entities)]
    (update-in instanced-entity [:attributes attr-name]
               (fn [attr]
                 (if attr
                   (let [{:keys [size iter]} (u/merge-attribute-opts instanced-entity attr-name attr)
                         data-len (* size iter)
                         start-offset (* start-index data-len)
                         end-offset (* end-index data-len)]
                     (update attr :data
                       (fn [data]
                         (let [v1 (subvec data 0 start-offset)
                               v2 (subvec data start-offset end-offset)
                               v3 (subvec data end-offset)]
                           (->> v3
                                (into new-data)
                                (into v1)
                                (into []))))))
                   {:data (vec new-data)
                    :divisor 1})))))

(defn- dissoc-instance-attr [start-index end-index instanced-entity attr-name]
  (update-in instanced-entity [:attributes attr-name]
             (fn [attr]
               (let [{:keys [size iter]} (u/merge-attribute-opts instanced-entity attr-name attr)
                     data-len (* size iter)
                     start-offset (* start-index data-len)
                     end-offset (* end-index data-len)]
                 (update attr :data
                         (fn [data]
                           (let [v1 (subvec data 0 start-offset)
                                 v2 (subvec data start-offset end-offset)
                                 v3 (subvec data end-offset)]
                             (into (into [] v1) v3))))))))

(def ^:private instanced-font-attrs->unis
  '{a_translate_matrix u_translate_matrix
    a_scale_matrix u_scale_matrix
    a_texture_matrix u_texture_matrix
    a_color u_color})

(defn assoc-line* [instanced-entity i entities]
  (let [characters (:characters instanced-entity)
        prev-lines (subvec characters 0 i)
        prev-count (reduce + 0 (map count prev-lines))
        curr-count (count (get characters i))
        total-count (+ prev-count curr-count)]
    (reduce-kv
      (partial replace-instance-attr prev-count total-count entities)
      instanced-entity
      instanced-font-attrs->unis)))

(defn insert-line* [instanced-entity i entities]
  (let [characters (:characters instanced-entity)
        prev-lines (subvec characters 0 i)
        prev-count (reduce + 0 (map count prev-lines))]
    (reduce-kv
      (partial replace-instance-attr prev-count prev-count entities)
      instanced-entity
      instanced-font-attrs->unis)))

(defn dissoc-line* [instanced-entity i]
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

(defn crop-char [{:keys [baked-font] :as font-entity} ch]
  (let [{:keys [baked-chars baseline first-char]} baked-font
        char-code (get-char-code ch first-char)
        baked-char (or (get baked-chars char-code)
                       (nth baked-chars (get-char-code \space first-char)))
        baked-char (cond-> baked-char (= ch \tab) (update :xadv * 2))
        {:keys [x y w h xoff yoff]} baked-char]
    (-> font-entity
        (t/crop x y w h)
        (assoc-in [:uniforms 'u_scale_matrix]
                  (m/scaling-matrix w h))
        (assoc-in [:uniforms 'u_translate_matrix]
                  (m/translation-matrix xoff (+ baseline yoff)))
        (assoc :baked-char baked-char :character ch))))

(defn assoc-char
  ([text-entity index char-entity]
   (assoc-char text-entity 0 index char-entity))
  ([{:keys [baked-font characters] :as text-entity} line-num index {:keys [baked-char] :as char-entity}]
   (let [characters (loop [chars characters]
                      (if (<= (count chars) line-num)
                        (recur (conj chars []))
                        chars))
         line (get characters line-num)
         prev-chars (subvec line 0 index)
         prev-xadv (reduce + 0 (map #(-> % :baked-char :xadv) prev-chars))
         x-total (+ (:xadv baked-char) prev-xadv)
         y-total (* line-num (:font-height baked-font))
         prev-lines (subvec characters 0 line-num)
         prev-count (reduce + 0 (map count prev-lines))
         replaced-char (get line index)
         line (assoc line index (-> char-entity
                                    (assoc :x-total x-total
                                           :left prev-xadv
                                           :width (:xadv baked-char)
                                           :height (:font-height baked-font))))
         next-char (get line (inc index))]
     (-> text-entity
         (assoc :characters (assoc characters line-num line))
         ;; adjust the next char if its horizontal position changed
         (cond-> (and next-char (not= (:x-total replaced-char) x-total))
                 (assoc-char line-num (inc index) next-char))))))

(defn assoc-line [text-entity line-num char-entities]
  (let [new-text-entity (reduce-kv
                          (fn [text-entity char-num entity]
                            (assoc-char text-entity line-num char-num entity))
                          (assoc-in text-entity [:characters line-num] [])
                          char-entities)
        new-line (get-in new-text-entity [:characters line-num])
        adjusted-new-line (mapv
                            (fn [{:keys [left] :as char-entity}]
                              (update-in char-entity [:uniforms 'u_translate_matrix]
                                #(m/multiply-matrices 3 (m/translation-matrix left 0) %)))
                            new-line)]
    (-> text-entity
        (assoc-line* line-num adjusted-new-line)
        (assoc-in [:characters line-num] new-line))))

(defn insert-line [{:keys [characters] :as text-entity} line-num char-entities]
  (let [new-characters (->> (subvec characters line-num)
                            (into [[]])
                            (into (subvec characters 0 line-num))
                            (into []))
        new-text-entity (reduce-kv
                          (fn [text-entity char-num entity]
                            (assoc-char text-entity line-num char-num entity))
                          (assoc text-entity :characters new-characters)
                          char-entities)
        new-line (get-in new-text-entity [:characters line-num])
        adjusted-new-line (mapv
                            (fn [{:keys [left] :as char-entity}]
                              (update-in char-entity [:uniforms 'u_translate_matrix]
                                #(m/multiply-matrices 3 (m/translation-matrix left 0) %)))
                            new-line)]
    (-> text-entity
        (insert-line* line-num adjusted-new-line)
        (assoc :characters (:characters new-text-entity)))))

(defn dissoc-char
  ([text-entity index]
   (dissoc-char text-entity 0 index))
  ([{:keys [characters] :as text-entity} line-num index]
   (let [line (nth characters line-num)
         prev-lines (subvec characters 0 line-num)
         prev-count (reduce + 0 (map count prev-lines))
         v1 (subvec line 0 index)
         v2 (subvec line (inc index))
         line (into (into [] v1) v2)
         next-char (get line index)]
     (-> text-entity
         (assoc-in [:characters line-num] line)
         (i/dissoc (+ index prev-count))))))

(defn dissoc-line [text-entity line-num]
  (-> text-entity
      (dissoc-line* line-num)
      (update :characters (fn [characters]
                            (let [v1 (subvec characters 0 line-num)
                                  v2 (subvec characters (inc line-num))]
                              (->> v2
                                   (into v1)
                                   (into [])))))))

