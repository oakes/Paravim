(ns paravim.chars
  (:require [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]
            [com.rpl.specter :as specter]
            [clojure.core.rrb-vector :as rrb]))

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

(def ^:const instanced-font-attrs->unis
  '{a_translate_matrix u_translate_matrix
    a_scale_matrix u_scale_matrix
    a_texture_matrix u_texture_matrix
    a_color u_color})

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
                                           :height (:font-height baked-font))))]
     (assoc text-entity :characters (assoc characters line-num line)))))

(defn assoc-line [text-entity line-num char-entities]
  (let [new-text-entity (reduce-kv
                          (fn [text-entity char-num char-entity]
                            (assoc-char text-entity line-num char-num char-entity))
                          (specter/setval [:characters line-num] [] text-entity)
                          char-entities)
        new-line (specter/select-any [:characters line-num] new-text-entity)
        adjusted-new-line (specter/transform
                            [specter/ALL (specter/collect-one :left) :uniforms 'u_translate_matrix]
                            (fn [left matrix]
                              (m/multiply-matrices 3 (m/translation-matrix left 0) matrix))
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

