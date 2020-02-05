(ns prey.being
  (:require [prey.util :as util]))

(defn alive? [being]
  (not (:dead? being)))

(defn die [being]
  (when (and (alive? being)
             (or (> (rand)
                    (util/survival-probability (:age being) (get-in being [:dna :max-age])))
                 (not (pos? (:energy being)))))
    [{:type       :die
      :actor-id   (:id being)
      :actor-type (:type being)}]))

(defn viable-mate? [being candidate-mate]
  (and (alive? candidate-mate)
       (not= (:gender being)
             (:gender candidate-mate))))

(defn mating? [being]
  (and (>= (:age being)
           (get-in being [:dna :maturity-at]))
       (>= (:desire being)
           (get-in being [:dna :desire-threshold]))))

(defn hungry? [being]
  (<= (:energy being)
      (get-in being [:dna :energy-threshold])))

(defn find-food-tx [being state]
  (let [food-key (if (= (:type being) :prey) :food :preys)  ;; TODO refactor this (maybe as a key in the being itself?
        [food-id food] (util/closest (get state food-key) being) ;; TODO Maybe not use the closest always? DNA?
        competitors (util/around (get state food-key) being (fn [_being other] (alive? other)))]
    (when (and food
               (or (not (get-in being [:dna :avoids-competitors?]))
                   (<= (count competitors) (get-in being [:dna :competition-threshold]))))
      (let [move-action (first (util/move-towards-tx being food (:terrain state)))]
        (if (and (= (:destination move-action)
                    (util/position food))
                 (:catch? being))
          [move-action
           {:type       :kill
            :actor-id   (:id being)
            :actor-type (:type being)
            :nutrition  (get-in being [:dna :nutrition])
            :target-id  food-id}]
          [move-action])))))


(defn find-mate-tx [being state]

  (when (mating? being)
    (let [mate-key (if (= (:type being) :prey) :preys :predators) ;; TODO refactor this (maybe as a key in the being itself?
          [_mate-id mate] (util/closest (get state mate-key) being viable-mate?)]
      (when mate
        (if (and (= :female (:gender being))
                 (<= (util/distance being mate)
                     (double 2)))
          [{:type       :wait
            :actor-type (:type being)
            :actor-id   (:id being)}]
          (util/move-towards-tx being mate (:terrain state)))))))


;; IDEA: Right now if a prey is hungry will always look for food instead of a mate
;; There could be a DNA switch that says "find any available among desires" or "find what you desire"e

(defn fullfil-desires [being state]                         ;; TODO the priority of this could be DNA-encoded?
  (or (and (hungry? being) (find-food-tx being state))
      (and (mating? being) (find-mate-tx being state))))