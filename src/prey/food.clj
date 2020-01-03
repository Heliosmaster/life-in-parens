(ns prey.food
  (:require [prey.util :as util]
            [prey.config :as config]))

(defn new-food [{:keys [x y]}]
  (let [id (util/new-id)]
    [id {:x x
         :id id
         :y y
         :type :food}]))

(defn initialize-food [terrain]
  (into {} (for [x (range 0 config/grid-size)
                 y (range 0 config/grid-size)
                 :let [p (rand)]
                 :when (and (<= p (get-in config/config [:food :density]))
                            (not (contains? terrain [x y])))]
             (new-food {:x x :y y}))))


(defn replenish-food [state]
  (let [total-food-quantity (Math/round (double (* config/grid-size config/grid-size (get-in config/config [:food :density]))))
        food-to-spawn (- total-food-quantity (count (:food state)))
        food-coords (repeatedly food-to-spawn #(util/viable-random-position (:terrain state) config/grid-size))
        fresh-food (into {} (map (fn [[x y]] (new-food {:x x :y y})) food-coords))]
    (merge (:food state) fresh-food)))