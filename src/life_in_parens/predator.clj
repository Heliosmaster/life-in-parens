(ns life-in-parens.predator
  (:require
    [life-in-parens.being :as being]
    [life-in-parens.config :as config]
    [life-in-parens.util :as util]))

(def predator-config (:predator config/config))

(defn new-genome []
  {:speed            (util/rand-between 1 3)
   :energy-threshold (util/rand-between 1 256)
   :litter-size      (util/rand-between 1 4)
   :desire-threshold (util/rand-between 1 1024)
   :offspring-energy (util/rand-between 50 150)
   :gestation        (util/rand-between 16 48)
   :sight-radius     (util/rand-between 2 8)
   :priority         (shuffle [:mate :food])
   :nutrition        (util/rand-between 16 32)
   :maturity-at      (util/rand-between 1 128)
   :max-age          (util/rand-between 1 1024)})

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
   :dna                  (or dna {:speed            1
                                  :energy-threshold 100
                                  :litter-size      2
                                  :desire-threshold 100
                                  :offspring-energy 100
                                  :gestation        20
                                  :sight-radius     7
                                  :priority         [:mate :food]
                                  :nutrition        20
                                  :maturity-at      10
                                  :max-age          400})
   :direction-inertia    (:direction-inertia predator-config)
   :direction            (rand-nth [:north :south :east :west])
   :gender               (or gender (rand-nth [:male :female]))
   :id                   (util/new-id)
   :type                 :predator})

(defn hunt [being state]
  (let [[prey-id prey] (util/closest (:preys state) being (fn [_being prey] (being/alive? prey)))
        competitors (util/around (:preys state) being (fn [_being other] (being/alive? other)))]
    (when (and prey
               (or (not (get-in being [:dna :avoids-competitors?]))
                   (<= (count competitors) (get-in being [:dna :competition-threshold]))))
      (let [move-action (first (util/move-towards-tx being prey (:terrain state)))]
        (if (and (= (:destination move-action)
                    (util/position prey))
                 (:catch? being))
          [move-action
           {:type       :kill
            :actor-id   (:id being)
            :actor-type (:type being)
            :nutrition  (get-in being [:dna :nutrition])
            :target-id  prey-id}]
          [move-action])))))


(defn initialize [terrain]
  (->> (for [x (range 0 config/grid-size)
             y (range 0 config/grid-size)
             :let [p (rand)]
             :when (and (<= p (:initial-density predator-config))
                        (not (contains? terrain [x y])))]
         (new-predator {:x x :y y}))
       #_[(new-predator {:x 10 :y 10 :gender :male})
          (new-predator {:x 10 :y 11 :gender :female})]
       (map (juxt :id identity))
       (into {})))

(defn debug-initialize []
  (->> [(new-predator {:x 1 :y 2 :gender :male})
        #_(new-predator {:x 3 :y 3 :gender :female})]
       (map (juxt :id identity))
       (into {})))

(defn take-decision [predator state]
  (or (being/die predator)
      (being/give-birth predator new-predator)
      (being/interact predator state mutate-predator)
      (being/fulfil-desires predator state)
      (hunt predator state)
      (util/move-randomly-tx predator (:terrain state))))