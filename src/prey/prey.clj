(ns prey.prey
  (:require [prey.util :as util]
            [prey.config :as config]))

(def prey-config (:prey config/config))

(def dna
  {:gestation 0 ;; shorter duration = weaker offspring
   :senses 0 ;; smaller = shorter sight range (will this work in a highly discretized scenario?)
   :reproductive-urge 0 ;; how much time it will devote to finding a mate vs finding food
   :desirability 0 ;; male-only (how much female will like them)
   :speed 0 ;; faster = more hungry
   })

(defn new-prey [{:keys [x y gender]}]
  {:x x
   :y y
   :hunger 0
   :desire 0
   :nutrition (:nutrition prey-config)
   :direction (rand-nth [:north :south :east :west])
   :speed (:speed prey-config)
   :gender (or gender (rand-nth [:male :female]))
   :direction-inertia (:direction-inertia prey-config)
   :id (util/new-id)
   :type :prey})

(defn crossover [mother father] ;; add more trait here
  {})


(defn initialize-preys [terrain]
  (->> (for [x (range 0 config/grid-size)
             y (range 0 config/grid-size)
             :let [p (rand)]
             :when (and (<= p (:initial-density prey-config))
                        (not (contains? terrain [x y])))]
         (new-prey {:x x :y y}))
       (map (juxt :id identity))
       (into {})))

(defn debug-initialize-preys []
  (->> [(new-prey {:x 0 :y 0 :gender :male})
        (new-prey {:x 2 :y 2 :gender :female})
        #_#_(new-prey {:x 5 :y 5 :gender :male})
            (new-prey {:x 6 :y 6 :gender :female})]
       (map (juxt :id identity))
       (into {})))


(defn find-food-tx [prey state]
  (let [[_food-id food] (util/closest (:food state) prey)]
    (if food
      (util/move-towards-tx prey food (:terrain state))
      (util/move-randomly-tx prey (:terrain state)))))

(defn viable-mate? [prey candidate-mate]
  (not= (:gender prey)
        (:gender candidate-mate)))

(defn mating? [prey]
  (>= (:desire prey)
      (:desire-threshold prey-config)))

(defn hungry? [prey]
  (>= (:hunger prey)
      (:hunger-threshold prey-config)))

(defn find-mate-tx [prey state]
  (when (mating? prey)
    (let [[_mate-id mate] (util/closest (:preys state) prey viable-mate?)]
      (when mate
        (if (and (= :female (:gender prey))
                 (<= (util/distance prey mate)
                     (double 2)))
          {:type :wait
           :actor-id (:id prey)}
          (util/move-towards-tx prey mate (:terrain state)))))))

;; IDEA: Right now if a prey is hungry will always look for food instead of a mate
;; There could be a DNA switch that says "find any available among desires" or "find what you desire"

(defn fullfil-desires [prey state] ;; TODO the priority of this could be DNA-encoded?
  (cond
    (hungry? prey) (find-food-tx prey state)
    (mating? prey) (find-mate-tx prey state)
    :else (util/move-randomly-tx prey (:terrain state))))

(defn die [prey]
  (when (>= (:hunger prey)
            (:starve-at prey-config))
    {:type :die
     :actor-id (:id prey)
     :actor-type :prey}))

(defn give-birth [prey]
  (when (and (= :female (:gender prey))
             (:pregnant? prey)
             (>= (:pregnancy prey) (:pregnancy-duration prey-config)))
    {:type :spawn
     :actor-id (:id prey)
     :children (map (fn [child] (new-prey (merge child (select-keys prey [:x :y]))))
                    (:children prey))
     :actor-type :prey}))

(defn interact [prey state] ;; TODO also priority of this could be DNA-encoded
  (let [[mate-id mate] (util/in-same-space (:preys state) prey viable-mate?)
        [food-id food] (util/in-same-space (:food state) prey)]
    (cond
      (and mate
           (mating? prey)
           (mating? mate)) {:type :mate
                            :actor-id (:id prey)
                            :actor-type :prey
                            :children [(crossover prey mate)
                                       (crossover prey mate)]
                            :mate-id mate-id}
      (and food
           (hungry? prey)) {:type :eat-food
                            :actor-id (:id prey)
                            :actor-type :prey
                            :target-id food-id})))

(defn take-decision [prey state]
  (or (die prey)
      (give-birth prey)
      (interact prey state)
      (fullfil-desires prey state)))

