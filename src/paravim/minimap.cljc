(ns paravim.minimap
  (:require [paravim.constants :as constants]
            [paravim.colors :as colors]
            [paravim.buffers :as buffers]
            [play-cljc.instances :as i]
            [play-cljc.transforms :as t]))

(defn ->minimap [{:keys [text-entity lines camera-x camera-y] :as buffer}
                 {:keys [base-rects-entity base-rect-entity] :as constants}
                 font-size-multiplier game-width game-height text-box]
  (let [text-top ((:top text-box) game-height font-size-multiplier)
        text-bottom ((:bottom text-box) game-height font-size-multiplier)
        font-width (* (:font-width constants) font-size-multiplier)
        font-height (* (:font-height constants) font-size-multiplier)
        minimap-width (/ game-width constants/minimap-scale)
        minimap-height (max 0 (- text-bottom text-top))
        minimap-font-size (/ font-size-multiplier constants/minimap-scale)
        minimap-font-width (* (:font-width constants) minimap-font-size)
        minimap-font-height (* (:font-height constants) minimap-font-size)
        minimap-line-count (int (min (/ minimap-height minimap-font-height) constants/max-visible-lines))
        minimap-chars (int (/ minimap-width minimap-font-width))
        line-count (count lines)
        minimap-is-overflowing (> line-count minimap-line-count)
        start-line (if minimap-is-overflowing
                     (int
                       (min
                         ; lines above
                         (/ (max camera-y 0) font-height)
                         ; lines below
                         (- line-count minimap-line-count)))
                     0)
        end-line (min line-count minimap-line-count)
        start-column (int (/ camera-x font-width))
        visible-lines (int (/ minimap-height font-height))]
    (hash-map
      :buffer-id (:id buffer)
      :show? (and (> minimap-chars constants/minimap-min-chars)
                  (> line-count visible-lines))
      :rects-entity
      (-> base-rects-entity
         (t/project game-width game-height)
         (i/assoc 0 (-> base-rect-entity
                        (t/color colors/bg-color)
                        (t/translate (- game-width minimap-width) text-top)
                        (t/scale minimap-width minimap-height)))
         (i/assoc 1 (-> base-rect-entity
                        (t/color colors/minimap-text-view-color)
                        (t/translate (- game-width minimap-width) text-top)
                        (t/translate 0 (- (/ camera-y constants/minimap-scale)
                                          (* start-line minimap-font-height)))
                        (t/scale minimap-width (/ minimap-height constants/minimap-scale)))))
      :text-entity
      (-> text-entity
          (cond-> minimap-is-overflowing
                  (buffers/crop-text-entity
                    start-line
                    (min
                      (+ minimap-line-count start-line)
                      line-count)))
          (assoc-in [:uniforms 'u_start_line] start-line)
          (assoc-in [:uniforms 'u_start_column] start-column)
          (assoc-in [:uniforms 'u_show_blocks]
                    (if (< minimap-font-size constants/minimap-min-size-to-show-chars) 1 0))
          (t/project game-width game-height)
          (t/translate (- game-width minimap-width) text-top)
          (cond-> (> start-column 0)
                  (t/translate (- (* start-column minimap-font-width)) 0))
          (cond-> (> start-line 0)
                  (t/translate 0 (- (* start-line minimap-font-height))))
          (t/scale font-size-multiplier font-size-multiplier)
          (t/scale (/ 1 constants/minimap-scale) (/ 1 constants/minimap-scale))))))
