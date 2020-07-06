(ns paravim.minimap
  (:require [paravim.constants :as constants]
            [paravim.colors :as colors]
            [paravim.buffers :as buffers]
            [play-cljc.instances :as i]
            [play-cljc.transforms :as t]))

;(set! *unchecked-math* :warn-on-boxed)

(defn ->minimap [{:keys [text-entity lines] :as buffer}
                 {:keys [base-rects-entity base-rect-entity] :as constants}
                 font-size-multiplier game-width game-height text-box]
  (let [camera-x (float (:camera-x buffer))
        camera-y (float (:camera-y buffer))
        font-size-multiplier (float font-size-multiplier)
        game-width (int game-width)
        game-height (int game-width)
        text-top (float ((:top text-box) game-height font-size-multiplier))
        text-bottom (float ((:bottom text-box) game-height font-size-multiplier))
        constant-font-width (float (:font-width constants))
        constant-font-height (float (:font-height constants))
        font-width (* constant-font-width font-size-multiplier)
        font-height (* constant-font-height font-size-multiplier)
        ;; integer representing how much bigger the normal text is compared to the minimap's
        minimap-scale (int constants/minimap-scale)
        ;; the min chars that should fit horizontally (hide minimap if less)
        minimap-min-chars (int constants/minimap-min-chars)
        ;; the min font multiplier required to show chars rather than blocks
        minimap-min-size-to-show-chars (float constants/minimap-min-size-to-show-chars)
        minimap-width (float (/ game-width minimap-scale))
        minimap-height (float (max 0.0 (- text-bottom text-top)))
        minimap-font-size (float (/ font-size-multiplier minimap-scale))
        minimap-font-width (float (* constant-font-width minimap-font-size))
        minimap-font-height (float (* constant-font-height minimap-font-size))
        minimap-line-count (int (min (/ minimap-height minimap-font-height)
                                     (int constants/max-visible-lines)))
        minimap-chars (int (/ minimap-width minimap-font-width))
        line-count (int (count lines))
        minimap-is-overflowing (> line-count minimap-line-count)
        start-line (int
                     (if minimap-is-overflowing
                       (min
                         ; lines above
                         (/ (max camera-y 0.0) font-height)
                         ; lines below
                         (- line-count minimap-line-count))
                       0))
        end-line (min line-count minimap-line-count)
        start-column (int (/ camera-x font-width))
        visible-lines (int (/ minimap-height font-height))]
    {:buffer-id (:id buffer)
     :show? (and (> minimap-chars minimap-min-chars)
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
                        (t/translate 0 (- (/ camera-y minimap-scale)
                                          (* start-line minimap-font-height)))
                        (t/scale minimap-width (/ minimap-height minimap-scale)))))
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
                   (if (< minimap-font-size minimap-min-size-to-show-chars) 1 0))
         (t/project game-width game-height)
         (t/translate (- game-width minimap-width) text-top)
         (cond-> (> start-column 0)
                 (t/translate (- (* start-column minimap-font-width)) 0))
         (cond-> (> start-line 0)
                 (t/translate 0 (- (* start-line minimap-font-height))))
         (t/scale font-size-multiplier font-size-multiplier)
         (t/scale (/ 1 minimap-scale) (/ 1 minimap-scale)))}))
