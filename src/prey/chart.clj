(ns prey.chart
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def offset 50)
(def vlines-at 250)
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

(defn draw-vertical-lines [truncate-at last-tick]
  (let [first-tick (- last-tick truncate-at)
        last-line (quot last-tick vlines-at)
        first-line (inc (quot first-tick vlines-at))
        lines (map #(* vlines-at %) (range first-line (inc last-line)))]
    (doseq [l lines]
      (q/fill 0)
      (q/text (str l) (+ offset (- l first-tick 10)) (- total-size 20))
      (q/stroke 221 219 221)
      (q/line (point (- l first-tick) 0)
              (point (- l first-tick) size))))
  )

(defn points [ps width height]
  (map-indexed (fn [i p]
                 (point (* width i) (* height p)))
               ps))

(def colors [[255 0 0]
              [0 0 255]])

(defn draw-multiple [{:keys [truncate-at adapt? rounding-at] :as _options} {:keys [data last-tick]}]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (let [datasets (map (fn [d]
                        (cond
                          truncate-at (take-last truncate-at d) ;; maybe use subvec) (if-let [truncate-at (:truncate-at options)]
                          adapt? (adapt-points d)
                          :else d))
                      data)
        all-data (apply concat datasets)]
    (when (seq all-data)
      (when (and truncate-at
                 (>= last-tick truncate-at))
        (draw-vertical-lines truncate-at last-tick))
      (let [max-point (apply max all-data)
            height (/ size max-point)]
        (when max-point (draw-size-lines {:max-point   max-point
                                          :height      height
                                          :rounding-at (or rounding-at 1)}))
        (doall
          (map-indexed
            (fn [i d]
              (let [ps d
                    width (/ size (count ps))

                    points (points ps width height)]
                (q/stroke-weight 1)
                (apply q/stroke (get colors i))
                (doall
                  (for [[[x1 y1] [x2 y2]] (partition 2 1 points)]
                    (q/line x1 y1 x2 y2)))))
            datasets))))))


(defn draw [{:keys [truncate-at adapt? rounding-at] :as _options} {:keys [data last-tick]}]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (when (seq data)
    (let [ps (cond
               truncate-at (take-last truncate-at data)     ;; maybe use subvec) (if-let [truncate-at (:truncate-at options)]
               adapt? (adapt-points data)
               :else data)
          width (/ size (count ps))
          max-point (apply max ps)
          height (/ size max-point)
          points (points ps width height)]
      (when (and truncate-at
                 (>= last-tick truncate-at))
        (draw-vertical-lines truncate-at last-tick))
      (when max-point (draw-size-lines {:max-point   max-point
                                        :height      height
                                        :rounding-at (or rounding-at 1)}))
      (q/stroke-weight 1)
      (q/stroke 255 0 0)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 points)]
          (q/line x1 y1 x2 y2))))))

(defn draw-min-max-avg [options {:keys [data last-tick]}]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (when (seq data)
    (let [point-sets (if-let [truncate-at (:truncate-at options)]
                       (take-last truncate-at data)         ;; maybe use subvec
                       data)
          mins (map :min point-sets)
          maxs (map :max point-sets)
          avgs (map :avg point-sets)
          width (/ size (count point-sets))
          max-point (apply max maxs)
          height (/ size max-point)
          min-points (points mins width height)
          max-points (points maxs width height)
          avg-points (points avgs width height)]
      (when (and (:truncate-at options)
                 (>= last-tick (:truncate-at options)))
        (draw-vertical-lines (:truncate-at options) last-tick))
      (when max-point (draw-size-lines {:max-point   max-point
                                        :height      height
                                        :rounding-at (or (:rounding-at options) 1)}))
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

(def lim 1002)

(def test-data
  {:data      (range 1 lim)
   :last-tick lim})

(defn test-line-chart [input]
  (q/sketch
    :title (:title input)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly input)
    ; update-state is called on each iteration before draw-state.
    :update identity
    :draw (partial draw {:rounding-at 1
                         :truncate-at size})
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )

(defn line-chart [input options]
  (q/sketch
    :title (:title options)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly input)
    ; update-state is called on each iteration before draw-state.
    :update identity
    :draw (partial draw (merge options {}))
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
    :update (fn [_state] {:data      (get-in (deref input-atom) [:data plot-key])
                          :last-tick (get-in (deref input-atom) [:data :last-tick])})
    :draw (partial draw (merge options {:truncate-at size}))
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )

(defn live-lines-chart [input-atom plot-keys options]
  (q/sketch
    :title (:title options)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly (map #(get @input-atom %) plot-keys))
    ; update-state is called on each iteration before draw-state.
    :update (fn [_state] {:data      (map #(get-in (deref input-atom) [:data %]) plot-keys)
                          :last-tick (get-in (deref input-atom) [:data :last-tick])})
    :draw (partial draw-multiple (merge options {:truncate-at size}))
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
    :update (fn [_state] {:data      (get-in (deref input-atom) [:data plot-key])
                          :last-tick (get-in (deref input-atom) [:data :last-tick])})
    :draw (partial draw-min-max-avg (merge options {:truncate-at size}))
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )
