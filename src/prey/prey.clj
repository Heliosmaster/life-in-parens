(ns prey.prey
  (:require [prey.util :as util]
            [prey.config :as config]))

;; Priority of actions
;;;;;;;;;;;;;;;;;;;;;
;; Interact with space
;; Fullfil desire
;;   - Find food (survive)
;;   - Mate (continue the species)

(def prey-config (:prey config/config))

#_(defn spawn-offspring! [type being] ;; TODO use both parents
    (let [n-children (inc (rand-int 3))
          children (into {} (repeatedly n-children (fn [] (merge (new-prey being)
                                                                 ))))]
      (swap! interactions #(update-in % [:created type] merge children))))



#_(defn mate-male-prey [prey mate state]
    (if (pos? (distance prey mate))
      (move-towards prey mate (:terrain state))
      (assoc prey :desire 0)))

#_(defn mate-female-prey [prey mate]
    (if (zero? (distance prey mate))
      (merge prey {:desire 0
                   :pregnancy 0
                   :pregnant? true})
      prey))

(defn pregnancy-over? [prey]
  (>= (:pregnancy prey) (get-in config/config [:prey :pregnancy-duration])))

(defn new-prey [{:keys [x y gender]}] ;; mother and father should be used here
  {:x x
   :y y
   :hunger 0
   :desire 100
   :direction (rand-nth [:north :south :east :west])
   :speed (:speed prey-config)
   :gender (or gender (rand-nth [:male :female]))
   :direction-inertia (:direction-inertia prey-config)
   :id (util/new-id)
   :type :prey})


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
        (new-prey {:x 2 :y 2 :gender :female})]
       (map (juxt :id identity))
       (into {})))


(defn find-food-tx [prey state]
  (let [[_food-id food] (util/closest (:food state) prey)]
    (if food
      (util/move-towards-tx prey food (:terrain state))
      (util/move-randomly-tx prey (:terrain state)))))

(defn viable-mate? [prey candidate-mate]
  (not= (:gender prey)
        (:gender candidate-mate)))

(defn mating? [prey]
  (>= (:desire prey)
      (:desire-threshold prey-config)))

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

(defn fullfil-desire [desire prey state] ;; TODO the priority of this could be DNA-encoded?
  (case desire
    :food (find-food-tx prey state)
    :mate (find-mate-tx prey state)))

(defn die [prey]
  (when (>= (:hunger prey)
            (:starve-at prey-config))
    {:type :die
     :actor-id (:id prey)
     :actor-type :prey}))


(defn interact [prey state] ;; TODO also priority of this could be DNA-encoded
  (let [[mate-id mate] (util/in-same-space (:preys state) prey viable-mate?)
        [food-id food] (util/in-same-space (:food state) prey)]
    (cond
      (and mate (mating? prey)) {:type :mate
                                 :actor-id (:id prey)
                                 :actor-type :prey
                                 :mate-id mate-id}
      food {:type :eat-food
            :actor-id (:id prey)
            :actor-type :prey
            :target-id food-id})))

(defn take-decision [prey state]
  (or (die prey)
      (interact prey state)
      (fullfil-desire :mate prey state)
      (fullfil-desire :food prey state)))
