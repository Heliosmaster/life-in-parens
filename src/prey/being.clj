(ns prey.being
  (:require [prey.util :as util]))

(defn viable-mate? [being candidate-mate]
  (not= (:gender being)
        (:gender candidate-mate)))

(defn mating? [being]
  (and (>= (:age being)
           (get-in being [:dna :maturity-at]))
       (>= (:desire being)
           (get-in being [:dna :desire-threshold]))))

(defn hungry? [being]
  (<= (:energy being)
      (get-in being [:dna :energy-threshold])))

(defn find-food-tx [being state]
  (let [food-key (if (= (:type being) :prey) ;; TODO refactor this (maybe as a key in the being itself?
                   :food
                   :preys)
        [_food-id food] (util/closest (get state food-key) being)
        competitors (util/around (:preys state) being)]
    (if (and food
             (or (not (get-in being [:dna :avoids-competitors?]))
                 (<= (count competitors) (get-in being [:dna :competition-threshold]))))
      (util/move-towards-tx being food (:terrain state))
      (util/move-randomly-tx being (:terrain state)))))


(defn find-mate-tx [being state]
  (when (mating? being)
    (let [mate-key (if (= (:type being) :prey) ;; TODO refactor this (maybe as a key in the being itself?
                     :preys
                     :predators)
          [_mate-id mate] (util/closest (get state mate-key) being viable-mate?)]
      (when mate
        (if (and (= :female (:gender being))
                 (<= (util/distance being mate)
                     (double 2)))
          {:type :wait
           :actor-id (:id being)}
          (util/move-towards-tx being mate (:terrain state)))))))


(defn fullfil-desires [being state] ;; TODO the priority of this could be DNA-encoded?
  (cond
    (hungry? being) (find-food-tx being state)
    (mating? being) (find-mate-tx being state)
    :else (util/move-randomly-tx being (:terrain state))))