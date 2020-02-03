(ns prey.prey
  (:require [prey.util :as util]
            [prey.config :as config]))

(def prey-config (:prey config/config))

;; Litter size = number of children. More children = weaker (every food eaten while pregnant is 'divided' among mother and children)
;; Competition threshold = integer value which describes how many other specimens is 'too crowded' when getting food.
;; Avoids competitor = true or false which controls whether the previous gene is dormant or not
;; Desire threshold = the higher this number is, the less often it will want to reproduce
;; Energy threshold = the lower it is, the more a specimen can avoid looking for food
;; Priority = vector with elements #{:food :mate}, it describes which of the two things has priority in case both are needing
;; Gestation = The length of pregnancy, longer gestation should yield a higher base energy (not counting food) for the children
;; Maturity at = the longer a specimen spends in 'infancy' i.e. without triggering reproduction ;; TODO another effect? (maybe 1 extra fast always when immature?)
;; Nutrition = the higher this is, the more nourishment the speciment gains from eating ;; TODO Currently no downsize
;; Max-age = the threshold in which the specimen will die of old age ;; TODO currently no downside
;; Speed = the speed at which the specimen can move
;; TODO Memory???



(defn new-genome []
  {:litter-size (util/rand-int-1 8)
   :competition-threshold (util/rand-int-1 16)
   :avoids-competitors? (rand-nth [true false])
   :desire-threshold (util/rand-int-1 1024)
   :energy-threshold (util/rand-int-1 128)
   :priority (shuffle [:food :mate])
   :gestation (util/rand-int-1 32)
   :maturity-at (util/rand-int-1 128)
   :nutrition (util/rand-int-1 32)
   :max-age (util/rand-int-1 1024)
   :speed (util/rand-int-1 8)})

(defn new-prey [{:keys [x y gender dna energy generation]}]
  {:x x
   :y y
   :age 0
   :generation (if generation (inc generation) 1)
   :energy (or energy (:initial-energy prey-config))
   :desire 0
   :dna (or dna {:litter-size (:litter-size prey-config)
                 :competition-threshold (:competition-threshold prey-config)
                 :avoids-competitors? false
                 :desire-threshold (:desire-threshold prey-config)
                 :energy-threshold (:energy-threshold prey-config)
                 :priority [:mate :food]
                 :gestation (:gestation prey-config)
                 :maturity-at (:maturity-at prey-config)
                 :nutrition (:nutrition prey-config)
                 :max-age (:max-age prey-config)
                 :speed (:speed prey-config)})
   :direction-inertia (:direction-inertia prey-config)
   :direction (rand-nth [:north :south :east :west])
   :gender (or gender (rand-nth [:male :female]))
   :id (util/new-id)
   :type :prey})

(defn mutate [[gene value]]
  (if (< (rand) (:mutation-probability prey-config))
    [gene (get (new-genome) gene)]
    [gene value]))

(defn new-embryo [mother father]
  {:energy 20 #_(* (get-in mother [:dna :gestation])) ;; TODO add positive effect to longer gestations
   :dna (->> (keys (:dna mother))
             (map (fn [gene]
                    [gene (rand-nth [(get-in mother [:dna gene])
                                     (get-in father [:dna gene])])]))
             (map mutate)
             (into {}))})




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
  (let [[_food-id food] (util/closest (:food state) prey)
        competitors (util/around (:preys state) prey)]
    (if (and food
             (or (not (get-in prey [:dna :avoids-competitors?]))
                 (<= (count competitors) (get-in prey [:dna :competition-threshold]))))
      (util/move-towards-tx prey food (:terrain state))
      (util/move-randomly-tx prey (:terrain state)))))

(defn viable-mate? [prey candidate-mate]
  (not= (:gender prey)
        (:gender candidate-mate)))

(defn mating? [prey]
  (and (>= (:age prey)
           (get-in prey [:dna :maturity-at]))
       (>= (:desire prey)
           (get-in prey [:dna :desire-threshold]))))

(defn hungry? [prey]
  (<= (:energy prey)
      (get-in prey [:dna :energy-threshold])))

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
            (not (pos? (:energy prey))))
    {:type :die
     :actor-id (:id prey)
     :actor-type :prey}))

(defn give-birth [prey]
  (when (and (= :female (:gender prey))
             (:pregnant? prey)
             (>= (:pregnancy prey)
                 (get-in prey [:dna :gestation])))
    {:type :spawn
     :actor-id (:id prey)
     :children (map (fn [child] (new-prey (merge child
                                                 (select-keys prey [:x :y :generation]))))
                    (:children prey))
     :actor-type :prey}))

(defn interact [prey state]
  (let [[mate-id mate] (util/in-same-space (:preys state) prey viable-mate?)
        [food-id food] (util/in-same-space (:food state) prey)
        ready-to-mate? (and mate (mating? prey) (mating? mate))
        ready-to-eat? (and food (hungry? prey))
        needs->event {:mate (when ready-to-mate? {:type :mate
                                                  :actor-id (:id prey)
                                                  :actor-type :prey
                                                  :children (repeatedly (get-in prey [:dna :litter-size])
                                                                        #(new-embryo prey mate))

                                                  :mate-id mate-id})
                      :food (when ready-to-eat? {:type :eat-food
                                                 :actor-id (:id prey)
                                                 :nutrition (get-in prey [:dna :nutrition])
                                                 :actor-type :prey
                                                 :target-id food-id})}]
    (some identity (map needs->event (get-in prey [:dna :priority])))))

(defn take-decision [prey state]
  (or (die prey)
      (give-birth prey)
      (interact prey state)
      (fullfil-desires prey state)))

