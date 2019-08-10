(ns paravim.chars
  (:require [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]))

(defn crop-char [{:keys [baked-font] :as font-entity} ch]
  (let [{:keys [baked-chars baseline first-char]} baked-font
        char-code (- #?(:clj (int ch) :cljs (.charCodeAt ch 0)) first-char)
        baked-char (nth baked-chars char-code)
        {:keys [x y w h xoff yoff]} baked-char]
    (-> font-entity
        (t/crop x y w h)
        (assoc-in [:uniforms 'u_scale_matrix]
                  (m/scaling-matrix w h))
        (assoc-in [:uniforms 'u_translate_matrix]
                  (m/translation-matrix xoff (+ baseline yoff)))
        (assoc :baked-char baked-char))))

(defn assoc-char
  ([line text-entity index char-entity]
   (assoc-char line text-entity 0 index char-entity))
  ([line {:keys [baked-font characters] :or {characters []} :as text-entity} line-num index {:keys [baked-char] :as char-entity}]
   (let [characters (loop [chars characters]
                      (if (<= (count chars) line-num)
                        (recur (conj chars []))
                        chars))
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
                                           :top y-total
                                           :width (+ (:w baked-char) (:xoff baked-char))
                                           :height (:font-height baked-font))))
         next-char (get line (inc index))]
     (-> line
         ;; adjust the next char if its horizontal position changed
         (cond-> (and next-char (not= (:x-total replaced-char) x-total))
                 (assoc-char text-entity line-num (inc index) next-char))))))

(defn assoc-line [text-entity line-num char-entities]
  (let [new-line (reduce-kv
                   (fn [line char-num entity]
                     (assoc-char line text-entity line-num char-num entity))
                   []
                   char-entities)
        adjusted-new-line (mapv
                            (fn [{:keys [left top] :as char-entity}]
                              (update-in char-entity [:uniforms 'u_translate_matrix]
                                #(m/multiply-matrices 3 (m/translation-matrix left top) %)))
                            new-line)]
    (-> text-entity
        (i/assoc line-num adjusted-new-line)
        (assoc-in [:characters line-num] new-line))))

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
  (-> (reduce
        (fn [entity i]
          (dissoc-char entity line-num 0))
        text-entity
        (range (count (get-in text-entity [:characters line-num]))))
      (update :characters (fn [characters]
                            (let [v1 (subvec characters 0 line-num)
                                  v2 (subvec characters (inc line-num))]
                              (->> v2
                                   (into v1)
                                   (into [])))))))

