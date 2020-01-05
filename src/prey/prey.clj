(ns prey.prey
  (:require [prey.util :as util]
            [prey.config :as config]))

;; Priority of actions
;;;;;;;;;;;;;;;;;;;;;
;; Interact with space
;; Fullfil desire
;;   - Find food (survive)
;;   - Mate (continue the species)


(defn new-prey [{:keys [x y]}] ;; mother and father should be used here
  {:x x
   :y y
   :hunger 0
   :desire 0
   :direction (rand-nth [:north :south :east :west])
   :speed (get-in config/config [:prey :speed])
   :gender (rand-nth [:male :female])
   :direction-inertia (get-in config/config [:prey :direction-inertia])
   :id (util/new-id)
   :type :prey})


(defn initialize-preys [terrain]
  (->> (for [x (range 0 config/grid-size)
             y (range 0 config/grid-size)
             :let [p (rand)]
             :when (and (<= p (get-in config/config [:prey :initial-density]))
                        (not (contains? terrain [x y])))]
         (new-prey {:x x :y y}))
       (map (juxt :id identity))
       (into {})))


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
