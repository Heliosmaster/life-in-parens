(ns prey.prey
  (:require [prey.util :as util]
            [prey.being :as being]
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
        #_(new-prey {:x 2 :y 2 :gender :female})
        #_(new-prey {:x 5 :y 5 :gender :male})
        #_(new-prey {:x 6 :y 6 :gender :female})]
       (map (juxt :id identity))
       (into {})))





(defn die [prey]
  (when (or (> (rand)
               (util/survival-probability (:age prey) (get-in prey [:dna :max-age])))
            (not (pos? (:energy prey))))
    [{:type       :die
      :actor-id   (:id prey)
      :actor-type :prey}]))

(defn give-birth [prey]
  (when (and (= :female (:gender prey))
             (:pregnant? prey)
             (>= (:pregnancy prey)
                 (get-in prey [:dna :gestation])))
    [{:type       :spawn
      :actor-id   (:id prey)
      :actor-type :prey
      :children   (map (fn [child] (new-prey (merge child
                                                    (select-keys prey [:x :y :generation]))))
                       (:children prey))}]))

(defn avoid-death [prey state]
  (let [predators (vals (util/around (:predators state) prey))]
    (util/avoid-things-tx prey predators (:terrain state))))

(defn interact [prey state]
  (let [[mate-id mate] (util/in-same-space (:preys state) prey being/viable-mate?)
        [food-id food] (util/in-same-space (:food state) prey)
        ready-to-mate? (and mate (being/mating? prey) (being/mating? mate))
        ready-to-eat? (and food (being/hungry? prey))
        needs->event {:mate (when ready-to-mate? [{:type       :mate
                                                   :actor-id   (:id prey)
                                                   :actor-type :prey
                                                   :children   (repeatedly (get-in prey [:dna :litter-size])
                                                                           #(new-embryo prey mate))

                                                   :mate-id    mate-id}])
                      :food (when ready-to-eat? [{:type       :eat-food
                                                  :actor-id   (:id prey)
                                                  :actor-type :prey
                                                  :nutrition  (get-in prey [:dna :nutrition])
                                                  :target-id  food-id}])}]
    (some identity (map needs->event (get-in prey [:dna :priority])))))

(defn take-decision [prey state]
  (or (die prey)
      (give-birth prey)
      (avoid-death prey state)
      (interact prey state)
      (being/fullfil-desires prey state)))

