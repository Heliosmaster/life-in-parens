(ns prey.predator
  (:require
    [prey.being :as being]
    [prey.config :as config]
    [prey.util :as util]))

(def predator-config (:predator config/config))

(defn new-predator [{:keys [x y gender dna energy generation]}]
  {:x                 x
   :y                 y
   :age               0
   :generation        (if generation (inc generation) 1)
   :energy            (or energy 100 #_(:initial-energy predator-config))
   :desire            0
   :dead?             false
   :catch?            true                                  ;; TODO DNA encode this? Maybe see that it will be selected?
   :dna               {:speed            2
                       :energy-threshold 100
                       :desire-threshold 40
                       :nutrition        20
                       :maturity-at      10
                       :max-age          400}
   #_#_:dna (or dna {:litter-size           (:litter-size predator-config)
                     :competition-threshold (:competition-threshold predator-config)
                     :avoids-competitors?   false
                     :desire-threshold      (:desire-threshold predator-config)
                     :energy-threshold      (:energy-threshold predator-config)
                     :priority              [:mate :food]
                     :gestation             (:gestation predator-config)
                     :maturity-at           (:maturity-at predator-config)
                     :nutrition             (:nutrition predator-config)
                     :max-age               (:max-age predator-config)
                     :speed                 (:speed predator-config)})
   :direction-inertia (:direction-inertia predator-config)
   :direction         (rand-nth [:north :south :east :west])
   :gender            (or gender (rand-nth [:male :female]))
   :id                (util/new-id)
   :type              :predator})


(defn initialize [terrain]
  (->> #_(for [x (range 0 config/grid-size)
               y (range 0 config/grid-size)
               :let [p (rand)]
               :when (and (<= p (:initial-density predator-config))
                          (not (contains? terrain [x y])))]
           (new-predator {:x x :y y}))
    [(new-predator {:x 10 :y 10})]
    (map (juxt :id identity))
    (into {})))

(defn debug-initialize []
  (->> [(new-predator {:x 1 :y 2 :gender :male})
             #_(new-predator {:x 5 :y 5 :gender :female})]
            (map (juxt :id identity))
            (into {})))

(defn take-decision [predator state]
  (or (being/die predator)
      #_(give-birth prey)
      #_(interact prey state)
      (being/fullfil-desires predator state)
      (util/move-randomly-tx predator (:terrain state))))