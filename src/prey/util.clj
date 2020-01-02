(ns prey.util
  (:require [prey.config :as config]))


(defn new-position [source direction]
  (let [step-length (get-in config/config [(:type source) :speed])
        deltax (case direction
                 :north 0
                 :east step-length
                 :south 0
                 :west (- step-length))
        deltay (case direction
                 :north (- step-length)
                 :east 0
                 :south step-length
                 :west 0)]
    {:x (+ (:x source) deltax)
     :y (+ (:y source) deltay)}))

(defn valid-direction? [source direction terrain]
  (let [new-pos (new-position source direction)]
    (and (<= 0 (:x new-pos) (dec config/grid-size))
         (<= 0 (:y new-pos) (dec config/grid-size))
         (not (contains? terrain [(:x new-pos) (:y new-pos)])))))

(defn move-towards [source target terrain]
  (let [directions (cond-> []
                     (pos? (- (:x target) (:x source))) (conj :east)
                     (neg? (- (:x target) (:x source))) (conj :west)
                     (pos? (- (:y target) (:y source))) (conj :south)
                     (neg? (- (:y target) (:y source))) (conj :north))
        valid-directions (filter #(valid-direction? source % terrain)
                                 directions)]
    (if (seq valid-directions)
      (merge source (new-position source (rand-nth valid-directions)))
      source)))

(defn move-randomly [source terrain]
  (let [valid-directions (filter #(valid-direction? source % terrain)
                                 [:north :south :east :west])
        current-direction (:direction source)]
    (if (seq valid-directions)
      (let [new-direction (if (and (contains? (set valid-directions) current-direction)
                                   (pos? (:direction-inertia source)))
                            current-direction
                            (rand-nth valid-directions))
            new-inertia (if (= current-direction new-direction)
                          (dec (:direction-inertia source))
                          (get-in config/config [(:type source) :direction-inertia]))]
        (-> source
            (merge (new-position source new-direction))
            (assoc :direction new-direction)
            (assoc :direction-inertia new-inertia)))
      source)))
