(ns prey.chart
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def offset 50)
(def size 500)
(def nlines 5)
(def total-size (+ size (* 2 offset)))

(defn reduce-sampling-by [dataset n]
  (->> (partition n dataset)
       (map (fn [chunk]
              (/ (reduce + chunk) n)))))

(defn point [x y]
  [(+ x offset)
   (- total-size (+ y offset))])

(defn adapt-points [data]
  (if (> (count data) size)
    (loop [n 2]
      (let [new-dataset (reduce-sampling-by data (inc n))]
        (if (> (count new-dataset) size)
          (recur (inc n))
          new-dataset)))
    data))

(defn draw-size-lines [{:keys [max-point height rounding-at]}]
  (let [c rounding-at
        nearest-round (* c (quot (quot max-point nlines)
                                   c))
        nearest-round (if (zero? nearest-round) c nearest-round)
        lines (quot max-point nearest-round)]
    (doseq [s (range (inc lines))]
      (q/fill 0)
      (q/text (str (* s nearest-round)) 20 (- total-size (+ offset (* height s nearest-round))))
      (q/stroke 0)
      (q/stroke 221 219 221)
      (q/line (point 0 (* height s nearest-round)) (point size (* height s nearest-round)))
      )))


(defn points [ps width height]
  (map-indexed (fn [i p]
                 (point (* width i) (* height p)))
               ps))

(defn draw [options state]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (when (seq state)
    (let [ps (if-let [truncate-at (:truncate-at options)]
               (take-last truncate-at state)  ;; maybe use subvec
               state)
          #_(adapt-points (:data state))
          width (/ size (count ps))
          max-point (apply max ps)
          height (/ size max-point)
          points (points ps width height)]

      (when max-point (draw-size-lines {:max-point max-point
                                        :height height
                                        :rounding-at (:rounding-at options)}))
      (q/stroke-weight 1)
      (q/stroke 255 0 0)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 points)]
          (q/line x1 y1 x2 y2))))))

(defn draw-min-max-avg [options state]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (when (seq state)
    (let [point-sets (if-let [truncate-at (:truncate-at options)]
                       (take-last truncate-at state)  ;; maybe use subvec
                       state)
          mins (map :min point-sets)
          maxs (map :max point-sets)
          avgs (map :avg point-sets)
          width (/ size (count point-sets))
          max-point (apply max maxs)
          height (/ size max-point)
          min-points (points mins width height)
          max-points (points maxs width height)
          avg-points (points avgs width height)]

      (when max-point (draw-size-lines {:max-point max-point
                                        :height height
                                        :rounding-at (:rounding-at options)}))
      (q/stroke-weight 1)
      (q/stroke 255 0 0)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 avg-points)]
          (q/line x1 y1 x2 y2)))
      (q/stroke 0 255 0)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 min-points)]
          (q/line x1 y1 x2 y2)))
      (q/stroke 0 0 255)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 max-points)]
          (q/line x1 y1 x2 y2))))))


(defn line-chart [input]
  (q/sketch
    :title (:title input)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly input)
    ; update-state is called on each iteration before draw-state.
    :update identity
    :draw (partial draw {})
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )


(defn live-line-chart [input-atom plot-key options]
  (q/sketch
    :title (:title options)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly (get @input-atom plot-key))
    ; update-state is called on each iteration before draw-state.
    :update (fn [_state] (get-in (deref input-atom) [:data plot-key]))
    :draw (partial draw (merge options {:truncate-at size}))
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )

(defn live-min-max-avg-chart [input-atom plot-key options]
  (q/sketch
    :title (:title options)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly (get @input-atom plot-key))
    ; update-state is called on each iteration before draw-state.
    :update (fn [_state] (get-in (deref input-atom) [:data plot-key]))
    :draw (partial draw-min-max-avg (merge options {:truncate-at size}))
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )