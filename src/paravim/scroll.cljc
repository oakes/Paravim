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

(defn adjust-camera [text-entity show-minimap? camera-x camera-y font-size text-box {:keys [font-width font-height] :as constants} window]
  (let [{game-width :width game-height :height} window
        text-top ((:top text-box) game-height font-size)
        text-bottom ((:bottom text-box) game-height font-size)
        char-counts (get-in text-entity [:uniforms 'u_char_counts])
        max-char-count (if (seq char-counts)
                         (apply max char-counts)
                         0)
        text-width (* max-char-count font-size font-width)
        text-height (* (count char-counts) font-size font-height)
        text-view-width (if show-minimap?
                          (- game-width (/ game-width constants/minimap-scale))
                          game-width)
        max-x (- text-width text-view-width)
        max-y (- text-height (- text-bottom text-top))]
    [(-> camera-x (min max-x) (max 0))
     (-> camera-y (min max-y) (max 0))]))

(defn rubber-band-camera [text-entity show-minimap?
                          camera-target-x camera-target-y
                          scroll-speed-x scroll-speed-y
                          font-size text-box constants window]
  (let [[new-x new-y] (adjust-camera text-entity show-minimap? camera-target-x camera-target-y font-size text-box constants window)]
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

(defn animate-camera [camera-x camera-y
                      camera-target-x camera-target-y
                      scroll-speed-x scroll-speed-y
                      delta-time]
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
    {:camera (t/translate constants/orig-camera new-x new-y)
     :camera-x new-x
     :camera-y new-y
     :scroll-speed-x new-speed-x
     :scroll-speed-y new-speed-y}))

(defn move-camera-to-cursor [buffer font-size text-box window {:keys [left top width height] :as cursor-entity}]
  (let [{:keys [camera camera-x camera-y]} buffer
        {game-width :width game-height :height} window
        text-top ((:top text-box) game-height font-size)
        text-bottom ((:bottom text-box) game-height font-size)
        cursor-bottom (+ top height)
        cursor-right (+ left width)
        text-view-width (if (:show-minimap? buffer)
                          (- game-width (/ game-width constants/minimap-scale))
                          game-width)
        text-view-height (- text-bottom text-top)
        camera-bottom (+ camera-y text-view-height)
        camera-right (+ camera-x text-view-width)
        camera-x (cond
                   (< left camera-x)
                   left
                   (> cursor-right camera-right)
                   (- cursor-right text-view-width)
                   :else
                   camera-x)
        camera-y (cond
                   (< top camera-y)
                   top
                   (> cursor-bottom camera-bottom 0)
                   (- cursor-bottom text-view-height)
                   :else
                   camera-y)]
    (assoc buffer
      :camera-target-x camera-x
      :camera-target-y camera-y)))

