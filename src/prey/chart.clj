(ns prey.chart
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def offset 20)
(def size 500)

(defn reduce-sampling-by [dataset n]
  (->> (partition n dataset)
       (map (fn [chunk]
              (/ (reduce + chunk) n)))))

(defn point [x y]
  [(+ x offset)
   (- size (+ y offset))])

(defn adapt-points [state]
  (loop [n 1]
    (let [new-dataset (reduce-sampling-by (:data state) (inc n))]
      (if (> (count new-dataset) size)
        (recur (inc n))
        new-dataset))))

(defn draw [state]
  (q/no-stroke)
  (q/rect offset offset size size)
  (q/stroke 0 0 0)
  (q/stroke-weight 1)
  (let [ps (adapt-points state)
        width (/ size (count ps))
        max-point (apply max ps)
        height (/ size (+ max-point 2 (* 2 offset)))
        points (map-indexed (fn [i p]
                              (point (* width i) (* height p)))
                            ps)]

    (doall
           (for [[[x1 y1] [x2 y2]] (partition 2 1 points)]
             (q/line x1 y1 x2 y2)))

    ))


(def test-data
  {:title "Test"
   :data (repeatedly 1000 #(rand-int 500))})


(defn line-chart [{:keys [title data] :as input}]
  (q/sketch
    :title title
    :size [(+ (* 2 offset) size)
           (+ (* 2 offset) size)]
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
