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
  {:litter-size           (util/rand-int-1 8)
   :competition-threshold (util/rand-int-1 16)
   :avoids-competitors?   (rand-nth [true false])
   :desire-threshold      (util/rand-int-1 1024)
   :energy-threshold      (util/rand-int-1 128)
   :offspring-energy      (util/rand-between 15 30)
   :priority              (shuffle [:food :mate])
   :gestation             (util/rand-int-1 32)
   :maturity-at           (util/rand-int-1 128)
   :nutrition             (util/rand-int-1 32)
   :sight-radius          (util/rand-between 2 5)
   :max-age               (util/rand-int-1 1024)
   :speed                 (util/rand-int-1 8)})

(defn new-prey [{:keys [x y gender dna energy generation]}]
  {:x                 x
   :y                 y
   :age               0
   :generation        (if generation (inc generation) 1)
   :energy            (or energy (:initial-energy prey-config))
   :desire            0
   :dead?             false
   :dna               (or dna {:litter-size           (:litter-size prey-config)
                               :competition-threshold (:competition-threshold prey-config)
                               :avoids-competitors?   false
                               :offspring-energy      (:offspring-energy prey-config)
                               :desire-threshold      (:desire-threshold prey-config)
                               :energy-threshold      (:energy-threshold prey-config)
                               :priority              [:mate :food]
                               :sight-radius          (:sight-radius prey-config)
                               :gestation             (:gestation prey-config)
                               :maturity-at           (:maturity-at prey-config)
                               :nutrition             (:nutrition prey-config)
                               :max-age               (:max-age prey-config)
                               :speed                 (:speed prey-config)})
   :direction-inertia (:direction-inertia prey-config)
   :direction         (rand-nth [:north :south :east :west])
   :gender            (or gender (rand-nth [:male :female]))
   :id                (util/new-id)
   :type              :prey})

(defn mutate-prey [[gene value]]
  (if (< (rand) (:mutation-probability prey-config))
    [gene (get (new-genome) gene)]
    [gene value]))

(defn initialize [terrain]
  (->> (for [x (range 0 config/grid-size)
             y (range 0 config/grid-size)
             :let [p (rand)]
             :when (and (<= p (:initial-density prey-config))
                        (not (contains? terrain [x y])))]
         (new-prey {:x x :y y}))
       (map (juxt :id identity))
       (into {})))

(defn debug-initialize []
  (->> [(new-prey {:x 0 :y 0 :gender :male})
        (new-prey {:x 2 :y 2 :gender :female})
        #_(new-prey {:x 7 :y 5 :gender :male})
        #_(new-prey {:x 1 :y 6 :gender :female})]
       (map (juxt :id identity))
       (into {})))

(defn decompose [prey]
  (when (:dead? prey)
    (if (< (:dead-since prey) (:decompose-after prey-config))
      []
      [{:type       :decompose
        :actor-id   (:id prey)
        :actor-type :prey}])))

(defn escape [prey state]
  (let [predators (vals (util/around (:predators state) prey))]
    (when (seq predators)
      (util/avoid-things-tx prey predators (:terrain state)))))

(defn take-decision [prey state]
  (or (decompose prey)
      (being/die prey)
      (being/give-birth prey new-prey)
      (escape prey state)
      (being/interact prey state mutate-prey)
      (being/fulfil-desires prey state)
      (util/move-randomly-tx prey (:terrain state))))




