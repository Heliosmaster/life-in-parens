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

(defn new-position [source direction]
  (let [step-length (get-in source [:dna :speed])
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

(defn move-randomly-tx [source terrain]
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
        {:type :move
         :actor-id (:id source)
         :actor-type (:type source)
         :destination (new-position source new-direction)
         :direction new-direction
         :new-inertia new-inertia})
      {:type :wait
       :actor-id (:id source)})))

(defn move-towards-tx [source target terrain]
  (let [directions (cond-> []
                     (pos? (- (:x target) (:x source))) (conj :east)
                     (neg? (- (:x target) (:x source))) (conj :west)
                     (pos? (- (:y target) (:y source))) (conj :south)
                     (neg? (- (:y target) (:y source))) (conj :north))
        valid-directions (filter #(valid-direction? source % terrain)
                                 directions)]

    (if (seq valid-directions)
      {:type :move
       :actor-id (:id source)
       :actor-type (:type source)
       :destination (new-position source (rand-nth valid-directions))}
      (move-randomly-tx source terrain))))

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