(ns prey.core
  (:require [quil.core :as q]
            [prey.food :as food]
            [prey.prey :as prey]
            [prey.actions :as actions]
            [prey.terrain :as terrain]
            [prey.config :as config]
            [prey.util :as util]
            [quil.middleware :as m]))

(def initial-interactions {:created {}
                           :destroyed []})

(def interactions (atom initial-interactions))

(defn stats [state]
  (let [preys (vals (:preys state))
        males (filter #(= :male (:gender %)) preys)
        females (filter #(= :female (:gender %)) preys)]
    {:nmales (count males)
     :nfemales (count females)}))

(defn print-stats [state]
  (let [{:keys [nmales nfemales]} (stats state)]
    (prn (format "%5d: Male: %5d - Female: %5d - Ratio: %2.2f" (q/frame-count) nmales nfemales (if (and (pos? nmales)
                                                                                                        (pos? nfemales))
                                                                                                 (double (/ nmales nfemales))
                                                                                                 0.0)))))
(defn color [being]
  (if (:gender being)
    (get-in config/config [(:type being) :color (:gender being)])
    (get-in config/config [(:type being) :color])))

(defn ->size [i] (* i config/unit-size))

(defn new-prey [{:keys [x y]}] ;; mother and father should be used here
  (let [id (util/new-id)]
    [id {:x x
         :y y
         :hunger 0
         :desire 0
         :direction (rand-nth [:north :south :east :west])
         :speed (get-in config/config [:prey :speed])
         :gender (rand-nth [:male :female])
         :direction-inertia (get-in config/config [:prey :direction-inertia])
         :id id
         :type :prey}]))


(defn initialize-preys [terrain]
  (into {} (for [x (range 0 config/grid-size)
                 y (range 0 config/grid-size)
                 :let [p (rand)]
                 :when (and (<= p (get-in config/config [:prey :initial-density]))
                            (not (contains? terrain [x y])))]
             (new-prey {:x x :y y}))))

(defn initialize-world []
  (let [terrain (terrain/initialize)]
    {:food (food/initialize-food terrain)
     :terrain terrain
     :preys (initialize-preys terrain)}))

(defn setup []
  (q/frame-rate config/fps)
  (initialize-world))

;;; update

(defn tick-prey [prey]
  (let [prey (-> prey
                 (update :desire inc)
                 (update :hunger inc))]
    (if (:pregnant? prey)
      (update prey :pregnancy inc)
      prey)))

(defn closest [things being]
  (let [s (util/sight-box being)
        seen-things (filter (fn [[_id thing]]
                              (and (<= (:xmin s) (:x thing) (:xmax s))
                                   (<= (:ymin s) (:y thing) (:ymax s))))
                            things)]
    (first (sort-by (fn [[_id thing]] (util/distance being thing)) seen-things))))


(defn destroy! [type {:keys [id] :as _being}]
  (swap! interactions #(update % :destroyed conj {:type type :id id})))

(defn spawn-offspring! [type being] ;; TODO use both parents
  (let [n-children (inc (rand-int 3))
        children (into {} (repeatedly n-children (fn [] (merge (new-prey being)
                                                               ))))]
    (swap! interactions #(update-in % [:created type] merge children))))

(defn closest-mate [world prey]
  (let [s (util/sight-box prey)
        seen-mates (filter (fn [[_id other-prey]]
                             (and (<= (:xmin s) (:x other-prey) (:xmax s))
                                  (<= (:ymin s) (:y other-prey) (:ymax s))
                                  (not= (:gender prey) (:gender other-prey))))
                           (:preys world))]
    (first (sort-by (fn [[_id mate]] (util/distance prey mate)) seen-mates))))


#_(defn find-prey-food [state prey]
    (let [[_food-id food] (closest (:food state) prey)]
      (if food
        (if (pos? (util/distance prey food))
          (util/move-towards prey food (:terrain state))
          (do
            (destroy! :food food)
            (update prey :hunger (fn [old-hunger] (min 0 (- old-hunger (get-in config/config [:prey :nutrition])))))))
        (util/move-randomly prey (:terrain state)))))

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

#_(defn find-prey-mate [state prey]
    (let [[_mate-id mate] (closest-mate state prey)]
      (if mate
        (if (= :female (:gender prey))
          (mate-female-prey prey mate)
          (mate-male-prey prey mate))
        (move-randomly prey (:terrain state)))))

(defn pregnancy-over? [prey]
  (>= (:pregnancy prey) (get-in config/config [:prey :pregnancy-duration])))

(defn starved? [prey]
  (>= (:hunger prey) (get-in config/config [:prey :starvation-hunger])))

(defn fertile? [prey]
  (> (:desire prey) (get-in config/config [:prey :desire-threshold])))


(defn update-prey [prey state]
  (let [action (prey/take-decision prey state)]
    (util/resolve-action prey action)))

#_(defn update-prey [state prey]
  (if (:pregnant? prey)
    (if (pregnancy-over? prey)
      (do (spawn-offspring! :preys prey)
          (merge prey {:pregnancy 0
                       :pregnant? false}))
      prey)
    (if (starved? prey)
      (do (destroy! :preys prey)
          prey)
      (find-prey-food state prey)
      #_(if (fertile? prey)
          (find-prey-mate state prey)
          (find-prey-food state prey)))))



(defn update-all-preys [state]
  (reduce (fn [acc [id prey]]
            (let [new-prey (tick-prey (update-prey prey state))]
              (assoc acc id new-prey)))
          {}
          (:preys state)))

(defn resolve-destroyed [state]
  (reduce (fn [acc {:keys [type id] :as _destroyed}]
            (update-in acc [type] dissoc id))
          state
          (:destroyed @interactions)))

(defn resolve-created [state]
  (reduce (fn [acc [type beings]]
            (update-in acc [type] merge beings))
          state
          (:created @interactions)))

(defn resolve-interactions [state]
  (let [new-state (-> state
                      (resolve-created)
                      (resolve-destroyed))]
    (reset! interactions initial-interactions)
    new-state))

(defn update-state [state]
  (let [actions (map (fn [[_prey-id prey]] (prey/take-decision prey state))
                     (:preys state))
        #_#_new-food (food/replenish-food state)]
    (reduce actions/resolve-action state actions)
    #_(-> state
        (assoc :preys new-preys)
        #_(assoc :food new-food)
        #_(resolve-interactions))))

;;; drawing

(defn draw-terrain [state]
  (doseq [terrain-pixel (:terrain state)]
    (q/fill 45 206 255)
    (q/no-stroke)
    (q/rect (->size (first terrain-pixel))
            (->size (second terrain-pixel))
            (->size 1)
            (->size 1))))

(defn draw-sight [being]
  (let [s (util/sight-box being)]
    (q/fill 0 0 0 0)
    (q/stroke-weight 1)
    (q/rect (->size (:xmin s))
            (->size (:ymin s))
            (->size (:size s))
            (->size (:size s)))))

(defn draw-state [state]
  (q/background 137 125 123)
  (draw-terrain state)
  (when (and config/debug?
             (zero? (mod (q/frame-count) 10)))
    (print-stats state))
  (doseq [being (concat (vals (:food state))
                        (vals (:preys state)))]
    (let [xx (->size (:x being))
          yy (->size (:y being))]
      (apply q/fill (color being))
      (q/no-stroke)
      (q/rect xx yy config/unit-size config/unit-size)
      (when (and config/debug? (= :prey (:type being)))
        (draw-sight being)))))

(q/defsketch prey
  :title "Ecosystem"
  :size [config/world-size config/world-size]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
