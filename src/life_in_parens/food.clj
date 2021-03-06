(ns life-in-parens.food
  (:require [life-in-parens.util :as util]
            [life-in-parens.config :as config]))

(defn new-food [{:keys [x y]}]
  {:x    x
   :id   (util/new-id)
   :y    y
   :type :food})

(defn initialize-food [terrain]
  (->> (for [x (range 0 config/grid-size)
             y (range 0 config/grid-size)
             :let [p (rand)]
             :when (and (<= p (get-in config/config [:food :density]))
                        (not (contains? terrain [x y])))]
         (new-food {:x x :y y}))
       (map (juxt :id identity))
       (into {})))

(defn debug-initialize-food []
  (->> [(new-food {:x 5 :y 5})
        (new-food {:x 2 :y 0})]
       (map (juxt :id identity))
       (into {})))

(defn replenish-food-txs [state]
  (let [total-food-quantity (Math/round (double (* config/grid-size config/grid-size (get-in config/config [:food :density]))))
        food-to-spawn (- total-food-quantity (count (:food state)))
        food-coords (repeatedly food-to-spawn #(util/viable-random-position (:terrain state) config/grid-size))]
    (map (fn [[x y]]
           (let [food (new-food {:x x :y y})]
             [{:type       :new
               :actor-type :food
               :actor-id   (:id food)
               :food       food}]))
         food-coords)))