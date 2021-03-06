(ns life-in-parens.scratch
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [life-in-parens.util :as util]
            [life-in-parens.config :as config]))

(defn move [being]
  (let [pos (select-keys being [:x :y])
        valid-directions (filter #(util/valid-direction? pos % {})
                                 [:north :south :east :west])
        direction-inertia (:direction-inertia being)
        current-direction (:direction being)]
    (if (seq valid-directions)
      (let [new-direction (if (and (contains? (set valid-directions) current-direction)
                                   (pos? direction-inertia))
                            current-direction
                            (rand-nth valid-directions))
            new-inertia (if (= current-direction new-direction)
                          (dec direction-inertia)
                          (:default-inertia being))
            new-pos (util/take-one-step being new-direction)]
        (merge being {:x                 (:x new-pos)
                      :y                 (:y new-pos)
                      :direction-inertia new-inertia
                      :direction         new-direction}))
      being)))

(defn ->size [i] (* i config/unit-size))

(defn setup []
  (q/frame-rate 10)
  (q/background 255)
  (let [dir 10
        being {:x                 20
               :y                 20
               :direction-inertia dir
               :default-inertia   dir
               :direction         :north}
        moves (->> (iterate move being)
                   (take 300)
                   (map #(select-keys % [:x :y])))]
    moves))

(defn draw [moves]
  (let [points-seq (partition 2 1 moves)]
    (doseq [[p1 p2] points-seq]
      (q/stroke 255 0 0)
      (q/line (->size (:x p1))
              (->size (:y p1))
              (->size (:x p2))
              (->size (:y p2))))))

(defn dir-rand-walk []
  (q/sketch
    :size [500 500]
    :setup setup
    :draw draw
    :update identity
    :features [:keep-on-top]
    :middleware [m/fun-mode]
    ))

;;;;

(def size 500)

(defn setup2 []
  (q/frame-rate 5)
  (q/background 255)
  [{:x 0 :y 0}])

(defn update2 [state]
  (conj state {:x (rand-int 500)
               :y (rand-int 500)}))

(defn draw2 [state]
  (doseq [[p1 p2] (partition 2 1 state)]
    (q/stroke (rand-int 255) (rand-int 255) (rand-int 255))
    (q/line (:x p1) (:y p1)
            (:x p2) (:y p2))))

(defn sample-sketch []
  (q/sketch
    :size [500 500]
    :setup setup2
    :draw draw2
    :update update2
    :features [:keep-on-top]
    :middleware [m/fun-mode]))