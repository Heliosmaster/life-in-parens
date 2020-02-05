(ns prey.actions)

(def being-key {:prey     :preys
                :predator :predators
                :food     :food})


(defmulti resolve-action
          (fn [_state action]
            [(:actor-type action) (:type action)]))

(defmethod resolve-action [:prey :die]
  [state action]
  (update-in state [:preys (:actor-id action)] merge {:dead? true
                                                      :dead-since 0}))

(defmethod resolve-action [:prey :decompose]
  [state action]
  (update-in state [:preys] dissoc (:actor-id action)))

(defmethod resolve-action [:prey :move]
  [state action]

  (update-in state [:preys (:actor-id action)]
             (fn [prey]
               (cond-> prey
                       true (merge (:destination action))
                       (:direction action) (assoc :direction (:direction action))
                       (:new-inertia action) (assoc :direction-inertia (:new-inertia action))))))

(defmethod resolve-action [:prey :mate]
  [state action]
  (update-in state [:preys (:actor-id action)]
             (fn [prey] (cond-> prey
                                :always (assoc :desire 0)
                                (= :female (:gender prey)) (merge {:pregnant? true
                                                                   :children  (:children action)})))))

(defn eat-while-pregnant [prey action]
  (-> prey
      (update :energy + (- (:nutrition action)
                           (count (:children prey))))
      (update :children (fn [children] (map (fn [child] (update child :energy inc))
                                            children)))))

(defmethod resolve-action [:prey :eat-food]
  [state action]
  (if (get-in state [:food (:target-id action)])
    (-> state
        (update-in [:preys (:actor-id action)]
                   (fn [prey]
                     (if (:pregnant? prey)
                       (eat-while-pregnant prey action)
                       (update prey :energy + (:nutrition action)))
                     ))
        (update-in [:food] dissoc (:target-id action)))
    state))

(defmethod resolve-action [:prey :spawn]
  [state action]
  (-> state
      (assoc-in [:preys (:actor-id action) :pregnant?] false)
      (assoc-in [:preys (:actor-id action) :pregnancy] 0)
      (update-in [:preys (:actor-id action)] dissoc :children)
      (update-in [:preys] merge (->> (:children action)
                                     (map (juxt :id identity))
                                     (into {})))))

(defmethod resolve-action [:prey :wait]
  [state _action]
  state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod resolve-action [:predator :move]                 ;; TODO refactor duped code?
  [state action]
  (update-in state [:predators (:actor-id action)]
             (fn [predator]
               (cond-> predator
                       true (merge (:destination action))
                       (:direction action) (assoc :direction (:direction action))
                       (:new-inertia action) (assoc :direction-inertia (:new-inertia action))))))

(defmethod resolve-action [:predator :kill]
  [state action]
  (if (get-in state [:preys (:target-id action)])
    (-> state
        (update-in [:preys] dissoc (:target-id action))
        (update-in [:predators (:actor-id action) :energy] + (:nutrition action)))
    state))

(defmethod resolve-action [:predator :die]
  [state action]
  (update-in state [:predators] dissoc (:actor-id action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod resolve-action [:food :new]
  [state action]
  (assoc-in state [:food (:actor-id action)] (:food action)))

(defmethod resolve-action :default [state _action]
  state)

(defn resolve-action* [state action]
  (if (or (= (:type action) :new)
          (get-in state [(being-key (:actor-type action)) (:actor-id action)]))
    (resolve-action state action)
    state))