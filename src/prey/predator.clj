(ns prey.predator
  (:require
    [prey.being :as being]
    [prey.config :as config]
    [prey.util :as util]))

(def predator-config (:predator config/config))

(defn new-genome []
  {:speed            2
   :energy-threshold 100
   :litter-size      1
   :desire-threshold 100
   :offspring-energy 50
   :gestation        20
   :priority         [:mate :food]
   :nutrition        200
   :maturity-at      10
   :max-age          1000})

(defn mutate-predator [[gene value]]
  (if (< (rand) (:mutation-probability predator-config))
    [gene (get (new-genome) gene)]
    [gene value]))

(defn new-predator [{:keys [x y gender dna generation]}]
  {:x                    x
   :y                    y
   :age                  0
   :generation           (if generation (inc generation) 1)
   :energy               100
   :desire               0
   :dead?                false
   :catch?               true                               ;; TODO DNA encode this? Maybe see that it will be selected?
   :mutation-probability (:mutation-probability predator-config)
   :dna                  (or dna (new-genome))
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
   :direction-inertia    (:direction-inertia predator-config)
   :direction            (rand-nth [:north :south :east :west])
   :gender               (or gender (rand-nth [:male :female]))
   :id                   (util/new-id)
   :type                 :predator})


(defn initialize [terrain]
  (->> #_(for [x (range 0 config/grid-size)
               y (range 0 config/grid-size)
               :let [p (rand)]
               :when (and (<= p (:initial-density predator-config))
                          (not (contains? terrain [x y])))]
           (new-predator {:x x :y y}))
    [(new-predator {:x 10 :y 10 :gender :male})
     (new-predator {:x 10 :y 11 :gender :female})]
    (map (juxt :id identity))
    (into {})))

(defn debug-initialize []
  {} #_(->> [(new-predator {:x 1 :y 2 :gender :male})
        (new-predator {:x 3 :y 3 :gender :female})]
       (map (juxt :id identity))
       (into {})))

(defn take-decision [predator state]
  (or (being/die predator)
      (being/give-birth predator new-predator)
      (being/interact predator state mutate-predator)
      (being/fullfil-desires predator state)
      (util/move-randomly-tx predator (:terrain state))))