(ns paravim.scroll
  (:require [paravim.constants :as constants]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [math]]
               :cljs [play-cljc.macros-js :refer-macros [math]])))

(def ^:const scroll-speed 40)
(def ^:const scroll-limit 10) ;; per scroll, not cumulative limit
(def ^:const min-scroll-speed 5)
(def ^:const deceleration 0.8)

(defn decelerate
  [speed]
  (let [speed (* speed deceleration)]
    (if (< speed min-scroll-speed)
      min-scroll-speed
      speed)))

(defn start-scrolling-camera [{:keys [camera-x camera-target-x camera-target-y scroll-speed-x scroll-speed-y] :as buffer} xoffset yoffset]
  (let [;; make the left edge "sticky" so it doesn't move unintentionally
        xoffset (if (and (== camera-x 0)
                         (< (math abs (long xoffset)) 2.5))
                  0
                  xoffset)
        ;; restrict the offsets to discard excessive values
        xoffset (-> xoffset (min scroll-limit) (max (- scroll-limit)))
        yoffset (-> yoffset (min scroll-limit) (max (- scroll-limit)))
        ;; flip the sign because the camera must go the opposite direction
        xdiff (* -1 scroll-speed xoffset)
        ydiff (* -1 scroll-speed yoffset)]
    {:camera-target-x (+ camera-target-x xdiff)
     :camera-target-y (+ camera-target-y ydiff)
     :scroll-speed-x (+ scroll-speed-x (math abs (long xdiff)))
     :scroll-speed-y (+ scroll-speed-y (math abs (long ydiff)))}))

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

(defn rubber-band-camera [{:keys [camera-target-x camera-target-y scroll-speed-x scroll-speed-y] :as buffer} font-size text-box constants window]
  (let [[new-x new-y] (adjust-camera buffer camera-target-x camera-target-y font-size text-box constants window)]
    (when (or (not (== camera-target-x new-x))
              (not (== camera-target-y new-y)))
      {:camera-target-x new-x
       :camera-target-y new-y
       :scroll-speed-x (if (not (== camera-target-x new-x))
                         min-scroll-speed
                         scroll-speed-x)
       :scroll-speed-y (if (not (== camera-target-y new-y))
                         min-scroll-speed
                         scroll-speed-y)})))

(defn update-camera [buffer camera-x camera-y font-size text-box window]
  (let [{game-height :height} window
        text-top ((:top text-box) game-height font-size)]
    (assoc buffer
      :camera (t/translate constants/orig-camera camera-x (- camera-y text-top))
      :camera-x camera-x
      :camera-y camera-y
      :camera-target-x camera-x
      :camera-target-y camera-y)))

(defn animate-camera [{:keys [camera-x camera-y
                              camera-target-x camera-target-y
                              scroll-speed-x scroll-speed-y] :as buffer}
                      font-size text-box window delta-time]
  (let [min-diff 1
        x-diff (long (- camera-target-x camera-x))
        y-diff (long (- camera-target-y camera-y))
        new-x (if (< (math abs x-diff) min-diff)
                camera-target-x
                (+ camera-x (* x-diff (min 1 (* delta-time scroll-speed-x)))))
        new-y (if (< (math abs y-diff) min-diff)
                camera-target-y
                (+ camera-y (* y-diff (min 1 (* delta-time scroll-speed-y)))))
        new-speed-x (if (== new-x camera-target-x)
                      0
                      (decelerate scroll-speed-x))
        new-speed-y (if (== new-y camera-target-y)
                      0
                      (decelerate scroll-speed-y))]
    (assoc (update-camera buffer new-x new-y font-size text-box window)
      :scroll-speed-x new-speed-x
      :scroll-speed-y new-speed-y)))

(defn move-camera-to-cursor [buffer font-size text-box window {:keys [left top width height] :as cursor-entity}]
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
      :camera-target-y camera-y)))

