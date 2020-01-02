(ns prey.core
  (:require [quil.core :as q]
            [prey.terrain :as terrain]
            [prey.config :as config]
            [prey.util :as util]
            [quil.middleware :as m])
  (:import [java.util UUID]))

(defn new-id []
  (UUID/randomUUID))

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

(defn sight-box [being]
  (let [sr config/sight-radius]
    {:xmin (- (:x being) sr)
     :xmax (+ (:x being) sr)
     :ymin (- (:y being) sr)
     :ymax (+ (:y being) sr)
     :size (inc (* 2 sr))}))

(defn distance [a b]
  (when (and a b)
    (Math/sqrt (+ (Math/pow (- (:x a) (:x b)) 2)
                  (Math/pow (- (:y a) (:y b)) 2)))))


(defn new-food [{:keys [x y]}]
  (let [id (new-id)]
    [id {:x x
         :id id
         :y y
         :type :food}]))

(defn new-prey [{:keys [x y]}] ;; mother and father should be used here
  (let [id (new-id)]
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

(defn initialize-food [terrain]
  (into {} (for [x (range 0 config/grid-size)
                 y (range 0 config/grid-size)
                 :let [p (rand)]
                 :when (and (<= p (get-in config/config [:food :density]))
                            (not (contains? terrain [x y])))]
             (new-food {:x x :y y}))))

(defn initialize-preys [terrain]
  (into {} (for [x (range 0 config/grid-size)
                 y (range 0 config/grid-size)
                 :let [p (rand)]
                 :when (and (<= p (get-in config/config [:prey :initial-density]))
                            (not (contains? terrain [x y])))]
             (new-prey {:x x :y y}))))

(defn initialize-world []
  (let [terrain (terrain/initialize)]
    {:food (initialize-food terrain)
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

(defn closest-food [world prey]
  (let [s (sight-box prey)
        seen-food (filter (fn [[_id food]]
                            (and (<= (:xmin s) (:x food) (:xmax s))
                                 (<= (:ymin s) (:y food) (:ymax s))))
                          (:food world))]
    (first (sort-by (fn [[_id food]] (distance prey food)) seen-food))))

(def initial-interactions {:created {}
                           :destroyed []})

(def interactions (atom initial-interactions))

(defn destroy! [type {:keys [id] :as _being}]
  (swap! interactions #(update % :destroyed conj {:type type :id id})))

(defn spawn-offspring! [type being] ;; TODO use both parents
  (let [n-children (inc (rand-int 3))
        children (into {} (repeatedly n-children (fn [] (merge (new-prey being)
                                                               ))))]
    (swap! interactions #(update-in % [:created type] merge children))))

(defn closest-mate [world prey]
  (let [s (sight-box prey)
        seen-mates (filter (fn [[_id other-prey]]
                             (and (<= (:xmin s) (:x other-prey) (:xmax s))
                                  (<= (:ymin s) (:y other-prey) (:ymax s))
                                  (not= (:gender prey) (:gender other-prey))))
                           (:preys world))]
    (first (sort-by (fn [[_id mate]] (distance prey mate)) seen-mates))))



(defn find-prey-food [state prey]
  (let [[_food-id food] (closest-food state prey)]
    (if food
      (if (pos? (distance prey food))
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

(defn update-prey [state prey]
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
            (let [new-prey (tick-prey (update-prey state prey))]
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

(defn viable-random-position [terrain grid-size]
  (let [x (rand-int grid-size)
        y (rand-int grid-size)]
    (if (contains? terrain [x y])
      (viable-random-position terrain grid-size)
      [x y])))

(defn replenish-food [state]
  (let [total-food-quantity (Math/round (double (* config/grid-size config/grid-size (get-in config/config [:food :density]))))
        food-to-spawn (- total-food-quantity (count (:food state)))
        food-coords (repeatedly food-to-spawn #(viable-random-position (:terrain state) config/grid-size))
        fresh-food (into {} (map (fn [[x y]] (new-food {:x x :y y})) food-coords))]
    (merge (:food state) fresh-food)))


(defn update-state [state]
  (let [new-preys (update-all-preys state)
        new-food (replenish-food state)]
    (-> state
        (assoc :preys new-preys)
        (assoc :food new-food)
        (resolve-interactions))))

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
  (let [s (sight-box being)]
    (q/fill 0 0 0 0)
    (q/stroke-weight 1)
    (q/rect (->size (:xmin s))
            (->size (:ymin s))
            (->size (:size s))
            (->size (:size s)))))

(defn draw-state [state]
  (q/background 137 125 123)
  (draw-terrain state)
  #_(when debug?
      (prn "---------")
      (doseq [prey (vals (:preys state))]
        (prn prey)))
  (when (zero? (mod (q/frame-count) 10))
    (print-stats state))
  (doseq [being (concat (vals (:food state))
                        (vals (:preys state)))]
    (let [xx (->size (:x being))
          yy (->size (:y being))]
      ; Set circle color.e
      (apply q/fill (color being))
      ; Calculate x and y coordinates of the circle.
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
