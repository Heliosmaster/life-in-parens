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

(defn draw-size-lines [max-point height]
  (let [c 10
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
      ))
  )



(defn points [ps width height]
  (map-indexed (fn [i p]
                 (point (* width i) (* height p)))
               ps))

(defn draw [state]
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 total-size total-size)
  (q/fill 255)
  (q/rect offset offset size size)
  (when (seq (:data state))
    (let [ps (take-last size (:data state)) ;; maybe use subvec
          #_(adapt-points (:data state))
          width (/ size (count ps))
          max-point (apply max ps)
          height (/ size max-point)
          points (points ps width height)]

      (when max-point (draw-size-lines max-point height))
      (q/stroke-weight 1)
      (q/stroke 255 0 0)
      (doall
        (for [[[x1 y1] [x2 y2]] (partition 2 1 points)]
          (q/line x1 y1 x2 y2)))

      )))


(def test-data
  {:title "Test"
   :data (range 1050)})


(defn line-chart [{:keys [title data] :as input}]
  (q/sketch
    :title title
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly input)
    ; update-state is called on each iteration before draw-state.
    :update identity
    :draw draw
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )


(defn live-line-chart [input-atom]
  (q/sketch
    :title (:title @input-atom)
    :size [total-size total-size]
    ; setup function called only once, during sketch initialization.
    :setup (constantly @input-atom)
    ; update-state is called on each iteration before draw-state.
    :update (fn [_state] (deref input-atom))
    :draw draw
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode])
  )
