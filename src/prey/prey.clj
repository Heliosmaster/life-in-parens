(ns prey.prey
  (:require [prey.util :as util]
            [prey.config :as config]))

(def prey-config (:prey config/config))

(def dna-config
  {:gestation {:init 0
               :delta 1} ;; shorter duration = weaker offspring
   ;; desire replenishment or maybe threshold
   :reproductive-urge {:init 0} ;; how much time it will devote to finding a mate vs finding food
   ; :food-bites 0 ;; bigger = more time it is idle eating, but more nutrition it gets. NOTE: only work with >1 food chunks.
   ; :desirability 0 ;; male-only (how much female will like them) NOTE: REMOVE MAYBE?
   :explorer 0 ;; the higher it is, the more directional inertia they will have when moving randomly
   :speed 0 ;; faster = more hungry
   :longevity 0 ;; longer lifespan, slower metabolism (speed + hunger)
   :development-phase 0 ;; longer = less time without making children, but stronger individual
   :litter-size 0 ;; smaller litter, stronger offspring
   })

(defn new-prey [{:keys [x y gender generation]}]
  {:x x
   :y y
   :age 0
   :generation (if generation (inc generation) 1)
   :hunger 0
   :desire 0
   :dna {:litter-size (:litter-size prey-config)
         :nutrition (:nutrition prey-config)
         :max-age (:max-age prey-config)
         :speed (:speed prey-config)}
   :direction-inertia (:direction-inertia prey-config)
   :direction (rand-nth [:north :south :east :west])
   :gender (or gender (rand-nth [:male :female]))
   :id (util/new-id)
   :type :prey})

(defn select-new-dna [mother father]
  {:dna (->> (keys (:dna mother))
             (map (fn [gene]
                    [gene (rand-nth [(get-in mother [:dna gene])
                                     (get-in father [:dna gene])])]))
             (into {}))})

(defn mutate [[gene value]]
  [gene value]
  )


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
  (when (or (> (rand)
               (util/survival-probability (:age prey) (get-in prey [:dna :max-age])))
            (>= (:hunger prey)
                (:starve-at prey-config)))
    {:type :die
     :actor-id (:id prey)
     :actor-type :prey}))

(defn give-birth [prey]
  (when (and (= :female (:gender prey))
             (:pregnant? prey)
             (>= (:pregnancy prey) (:pregnancy-duration prey-config)))
    {:type :spawn
     :actor-id (:id prey)
     :children (map (fn [child] (new-prey (merge child (select-keys prey [:x :y :generation]))))
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
                            :children (repeatedly (get-in prey [:dna :litter-size])
                                                  #(select-new-dna prey mate))

                            :mate-id mate-id}
      (and food
           (hungry? prey)) {:type :eat-food
                            :actor-id (:id prey)
                            :nutrition (get-in prey [:dna :nutrition])
                            :actor-type :prey
                            :target-id food-id})))

(defn take-decision [prey state]
  (or (die prey)
      (give-birth prey)
      (interact prey state)
      (fullfil-desires prey state)))

