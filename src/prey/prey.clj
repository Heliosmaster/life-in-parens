(ns prey.prey
  (:require [prey.util :as util]))

;; Priority
;; Interact with space
;; Fullfil desire

(defn find-food-tx [prey state]
  (let [[_food-id food] (util/closest (:food state) prey)]
    (if food
      (util/move-towards-tx prey food (:terrain state))
      (util/move-randomly-tx prey (:terrain state)))))

(defn fullfil-desire [prey state]
  (let [action (find-food-tx prey state)]
    action))

(defn interact [prey state]
  (let [[food-id food] (first (filter (fn [[_food-id food]] (and (= (:x prey) (:x food))
                                                                 (= (:y prey) (:y food))))
                                      (:food state)))]
    (when food
      {:type :eat-food
       :actor-id (:id prey)
       :actor-type :prey
       :target-id food-id})))

(defn take-decision [prey state]
  (or (interact prey state)
      (fullfil-desire prey state)))
