(ns prey.being
  (:require [prey.util :as util]))

(defn alive? [being]
  (not (:dead? being)))

(def relations
  {:prey     {:eat            :food
              :filter-food-fn (constantly true)
              :mate           :preys}
   :predator {:eat            :preys
              :filter-food-fn (fn [_being other] (:dead? other))
              :mate           :predators}})

(defn die [being]
  (when (and (alive? being)
             (or (> (rand)
                    (util/survival-probability (:age being) (get-in being [:dna :max-age])))
                 (not (pos? (:energy being)))))
    [{:type       :die
      :actor-id   (:id being)
      :actor-type (:type being)}]))


(defn mating? [being]
  (and (>= (:age being)
           (get-in being [:dna :maturity-at]))
       (not (:pregnant? being))
       (>= (:desire being)
           (get-in being [:dna :desire-threshold]))))

(defn viable-mate? [being candidate-mate]
  (and (alive? candidate-mate)
       (mating? candidate-mate)
       (not= (:gender being)
             (:gender candidate-mate))))

(defn hungry? [being]
  (<= (:energy being)
      (get-in being [:dna :energy-threshold])))

(defn find-food-tx [being state]
  (let [food-key (get-in relations [(:type being) :eat])    ;; TODO Maybe not use the closest always? DNA?
        [_food-id food] (util/closest (get state food-key) being (get-in relations [(:type being) :filter-food-fn]))
        competitors (util/around (get state food-key) being (fn [_being other] (alive? other)))]
    (when (and food
               (or (not (get-in being [:dna :avoids-competitors?]))
                   (<= (count competitors) (get-in being [:dna :competition-threshold]))))
      (let [move-action (first (util/move-towards-tx being food (:terrain state)))]
        [move-action]
        #_(if (and (= (:destination move-action)
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
    (let [mate-key (get-in relations [(:type being) :mate])
          [_mate-id mate] (util/closest (get state mate-key) being viable-mate?)]
      (when mate
        (if (and (= :female (:gender being))
                 (<= (util/distance being mate)
                     (double 2)))
          [{:type       :wait
            :actor-type (:type being)
            :actor-id   (:id being)}]
          (util/move-towards-tx being mate (:terrain state)))))))

(defn new-embryo [mother father mutate-fn]
  (let [dna (->> (keys (:dna mother))
                 (map (fn [gene]
                        [gene (rand-nth [(get-in mother [:dna gene])
                                         (get-in father [:dna gene])])]))
                 (map mutate-fn)
                 (into {}))]
    {;; TODO add positive effect to longer gestations
     :energy (:offspring-energy dna)
     :dna    dna}))

(defn give-birth [being new-being-fn]
  (when (and (= :female (:gender being))
             (:pregnant? being)
             (>= (:pregnancy being)
                 (get-in being [:dna :gestation])))
    [{:type       :spawn
      :actor-id   (:id being)
      :actor-type (:type being)
      :children   (map (fn [child] (new-being-fn (merge child (select-keys being [:x :y :generation]))))
                       (:children being))}]))

(defn interact [being state mutate-fn]
  (let [[mate-id mate] (util/in-same-space (get state (get-in relations [(:type being) :mate])) being viable-mate?)
        [food-id food] (util/in-same-space (get state (get-in relations [(:type being) :eat])) being)
        ready-to-mate? (and mate (mating? being) (mating? mate))
        ready-to-eat? (and food (hungry? being))
        needs->event {:mate (when ready-to-mate? [{:type       :mate
                                                   :actor-id   (:id being)
                                                   :actor-type (:type being)
                                                   :children   (repeatedly (get-in being [:dna :litter-size])
                                                                           #(new-embryo being mate mutate-fn))

                                                   :mate-id    mate-id}])
                      :food (when ready-to-eat? [{:type       :eat-food
                                                  :actor-id   (:id being)
                                                  :actor-type (:type being)
                                                  :nutrition  (get-in being [:dna :nutrition])
                                                  :target-id  food-id}])}]
    (some identity (map needs->event (get-in being [:dna :priority])))))


;; IDEA: Right now if a prey is hungry will always look for food instead of a mate
;; There could be a DNA switch that says "find any available among desires" or "find what you desire"e

(defn fulfil-desires [being state]                         ;; TODO the priority of this could be DNA-encoded?
  (or (and (hungry? being) (find-food-tx being state))
      (and (mating? being) (find-mate-tx being state))))