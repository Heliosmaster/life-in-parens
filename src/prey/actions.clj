(ns prey.actions
  (:require [prey.config :as config]))

(defmulti resolve-action
  (fn [_state action]
    [(:actor-type action) (:type action)]))

(defmethod resolve-action [:prey :die]
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

(defmethod resolve-action [:prey :eat-food]
  [state action]
  (-> state
      (update-in [:preys (:actor-id action) :hunger]
                 (fn [old-hunger] (min 0 (- old-hunger (get-in config/config [:prey :nutrition])))))
      (update-in [:food] dissoc (:target-id action))))

(defmethod resolve-action [:prey :wait]
  [state _action]
  state)


(defmethod resolve-action [:food :new-food]
  [state action]
  (assoc-in state [:food (:actor-id action)] (:food action)))

(defmethod resolve-action :default [state _]
  state)