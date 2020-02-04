(ns prey.util
  (:require [prey.config :as config])
  (:import [java.util UUID]))

(defn new-id []
  (UUID/randomUUID))

(defn sight-box [being]
  (let [sr config/sight-radius]
    {:xmin (- (:x being) sr)
     :xmax (+ (:x being) sr)
     :ymin (- (:y being) sr)
     :ymax (+ (:y being) sr)
     :size (inc (* 2 sr))}))

(defn position [being]
  (select-keys being [:x :y]))

(defn manhattan-distance [a b]
  (when (and a b)
    (+ (Math/abs ^Integer (- (:x a) (:x b)))
       (Math/abs ^Integer (- (:y a) (:y b))))))

(defn distance [a b]
  (when (and a b)
    (Math/sqrt (+ (Math/pow (- (:x a) (:x b)) 2)
                  (Math/pow (- (:y a) (:y b)) 2)))))

(defn viable-random-position [terrain grid-size]
  (let [x (rand-int grid-size)
        y (rand-int grid-size)]
    (if (contains? terrain [x y])
      (viable-random-position terrain grid-size)
      [x y])))

(defn take-one-step [{:keys [x y] :as _position} direction]
  (let [deltax (case direction
                 :north 0
                 :east 1
                 :south 0
                 :west (- 1))
        deltay (case direction
                 :north (- 1)
                 :east 0
                 :south 1
                 :west 0)]
    {:x (+ x deltax)
     :y (+ y deltay)}))

(defn valid-direction? [position direction terrain]
  (let [new-pos (take-one-step position direction)]
    (and (<= 0 (:x new-pos) (dec config/grid-size))
         (<= 0 (:y new-pos) (dec config/grid-size))
         (not (contains? terrain [(:x new-pos) (:y new-pos)])))))

(defn move-randomly-tx [source terrain]
  (loop [step-n 1
         direction-inertia (:direction-inertia source)
         current-direction (:direction source)
         pos (position source)]
    (let [valid-directions (filter #(valid-direction? pos % terrain)
                                   [:north :south :east :west])]
      (if (seq valid-directions)
        (let [new-direction (if (and (contains? (set valid-directions) current-direction)
                                     (pos? direction-inertia))
                              current-direction
                              (rand-nth valid-directions))
              new-inertia (if (= current-direction new-direction)
                            (dec direction-inertia)
                            (get-in config/config [(:type source) :direction-inertia]))
              new-pos (take-one-step source new-direction)]
          (if (= step-n (get-in source [:dna :speed]))
            [{:type        :move
              :actor-id    (:id source)
              :actor-type  (:type source)
              :destination new-pos
              :direction   new-direction
              :new-inertia new-inertia}]
            (recur (inc step-n)
                   new-inertia
                   new-direction
                   new-pos)))
        (if (= pos (position source))
          [{:type     :wait
            :actor-id (:id source)
            :actor-type (:type source)}]
          [{:type        :move
            :actor-id    (:id source)
            :actor-type  (:type source)
            :destination pos
            :direction   current-direction
            :new-inertia direction-inertia}])))))

(defn move-towards-tx [source target terrain]
  (loop [step-n 1
         new-source source]
    (let [{:keys [x y]} new-source
          there? (and (= (:x target) x)
                      (= (:y target) y))
          position {:x x :y y}
          directions (cond-> []
                             (pos? (- (:x target) x)) (conj :east)
                             (neg? (- (:x target) x)) (conj :west)
                             (pos? (- (:y target) y)) (conj :south)
                             (neg? (- (:y target) y)) (conj :north))
          valid-directions (filter #(valid-direction? position % terrain)
                                   directions)]
      (cond
        there? [{:type        :move
                 :actor-id    (:id new-source)
                 :actor-type  (:type new-source)
                 :destination position}]
        (seq valid-directions) (let [new-pos (take-one-step position (rand-nth valid-directions))]
                                 (if (= step-n (get-in new-source [:dna :speed]))
                                   [{:type        :move
                                     :actor-id    (:id new-source)
                                     :actor-type  (:type new-source)
                                     :destination new-pos}]
                                   (recur (inc step-n) (merge new-source new-pos))))
        :else (move-randomly-tx new-source terrain)))))

(defn around
  ([things being]
   (around things being (constantly true)))
  ([things being filter-fn]
   (let [s (sight-box being)
         seen-things (filter (fn [[_id thing]]
                               (and (<= (:xmin s) (:x thing) (:xmax s))
                                    (<= (:ymin s) (:y thing) (:ymax s))
                                    (not= thing being)
                                    (filter-fn being thing)))
                             things)]
     seen-things)))


(defn avoid-things-tx [source targets terrain]
  (loop [step-n 1
         new-source source]
    (let [{:keys [x y]} new-source
          position {:x x :y y}
          valid-directions (:directions (reduce (fn [acc direction]
                                                  (let [new-pos (take-one-step position direction)
                                                        total-distance (->> targets
                                                                            (map (partial manhattan-distance new-pos))
                                                                            (reduce + 0))]
                                                    (cond
                                                      (or (empty? (:directions acc))
                                                          (> total-distance (:min-distance acc))) {:min-distance total-distance
                                                                                                   :directions   [direction]}
                                                      (= total-distance (:min-distance acc)) (update acc :directions conj direction)
                                                      :else acc)))
                                                {:min-distance nil
                                                 :directions   []}
                                                (filter #(valid-direction? position % terrain) [:north :south :east :west])))]
      (if (seq valid-directions)
        (let [new-position (take-one-step position (rand-nth valid-directions))]

          (if (= step-n (get-in source [:dna :speed]))
            [{:type        :move
              :actor-id    (:id new-source)
              :actor-type  (:type new-source)
              :destination new-position}]
            (recur (inc step-n) (merge new-source new-position))))
        (move-randomly-tx new-source terrain)))))

(defn closest
  ([things being]
   (closest things being (constantly true)))
  ([things being filter-fn]
   (first (sort-by (fn [[_id thing]] (distance being thing))
                   (around things being filter-fn)))))

(defn in-same-space
  ([things being]
   (in-same-space things being (constantly true)))
  ([things being filter-fn]
   (first (filter (fn [[_thing-id thing]] (and (= (:x being) (:x thing))
                                               (= (:y being) (:y thing))
                                               (not= (:id being) (:id thing))
                                               (filter-fn being thing)))
                  things))))


(defn death-probability [age max-age]
  (/ (* 7 (Math/pow age 6))
     (Math/pow max-age 7)))

(defn survival-probability [age max-age]
  (- 1 (death-probability age max-age)))

(defn mean [coll]
  (/ (reduce + 0 coll)
     (count coll)))

(defn rand-int-1 [max]
  (inc (rand-int (dec max))))