(ns prey.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import [java.util UUID]))

(def grid-size 50)
(def unit-size 10)
(def world-size (* grid-size unit-size))
(def sight-radius 4)
(defn random-color []
  [(rand-int 255) (rand-int 255) (rand-int 255)])

(defn new-id []
  (UUID/randomUUID))

(def colors
  {:rabbit [224 224 224]
   :grass [35 89 8]})

(def config
  {:rabbit {:size 10
            :initial-energy 50
            :nutrition-energy 10
            :speed 1}
   :grass {:max-amount 50
           :lifespan 10}})



(defn ->size [i] (* i unit-size))

(defn sight-box [being]
  {:xmin (- (:x being) sight-radius)
   :xmax (+ (:x being) sight-radius)
   :ymin (- (:y being) sight-radius)
   :ymax (+ (:y being) sight-radius)
   :size (inc (* 2 sight-radius))})

(defn distance [a b]
  (when (and a b)
    (Math/sqrt (+ (Math/pow (- (:x a) (:x b)) 2)
                  (Math/pow (- (:y a) (:y b)) 2)))))


(defn new-grass [_]
  (let [id (new-id)]
    [id {:x (rand-int grid-size)
         :id id
         :y (rand-int grid-size)
         :type :grass}]))

(defn seed-world []
  {:grass (into {} (map new-grass
                        (range (get-in config [:grass :max-amount]))))
   :rabbits (into {} (map (fn [_]
                            (let [id (new-id)]
                              [id
                               {:x (rand-int grid-size)
                                :y (rand-int grid-size)
                                :energy (get-in config [:rabbit :initial-energy])
                                :id id
                                :type :rabbit}]))
                          (range (get-in config [:rabbit :size]))))})


(defn setup []
  (q/frame-rate 20)
  (seed-world))

;;; update

(defn move-towards [source target]
  (let [step-length (get-in config [(:type source) :speed])
        valid-directions (cond-> []
                           (pos? (- (:x target) (:x source))) (conj :east)
                           (neg? (- (:x target) (:x source))) (conj :west)
                           (pos? (- (:y target) (:y source))) (conj :south)
                           (neg? (- (:y target) (:y source))) (conj :north))
        new-direction (rand-nth valid-directions)
        deltax (case new-direction
                 :north 0
                 :east step-length
                 :south 0
                 :west (- step-length))
        deltay (case new-direction
                 :north (- step-length)
                 :east 0
                 :south step-length
                 :west 0)]
    (-> source
        (update :x + deltax)
        (update :y + deltay)
        (update :energy dec))))

(defn move-randomly [being]
  (let [step-length (get-in config [(:type being) :speed])
        valid-directions (cond-> []
                           (< (:x being) (dec grid-size)) (conj :east)
                           (> (:x being) 0) (conj :west)
                           (> (:y being) 0) (conj :north)
                           (< (:y being) (dec grid-size)) (conj :south))
        new-direction (rand-nth valid-directions)
        deltax (case new-direction
                 :north 0
                 :east step-length
                 :south 0
                 :west (- step-length))
        deltay (case new-direction
                 :north (- step-length)
                 :east 0
                 :south step-length
                 :west 0)]
    (-> being
        (update :x + deltax)
        (update :y + deltay)
        (update :energy dec))))

(defn closest-grass [world rabbit]
  (let [s (sight-box rabbit)
        seen-grass (filter (fn [[_id grass]]
                             (and (<= (:xmin s) (:x grass) (:xmax s))
                                  (<= (:ymin s) (:y grass) (:ymax s))))
                           (:grass world))]
    (first (sort-by (fn [[_id grass]] (distance rabbit grass)) seen-grass))))

(def initial-interations {:created []
                          :destroyed []})

(def interactions (atom initial-interations))

(defn destroy! [type id]
  (swap! interactions #(update % :destroyed conj {:type type :id id})))


(defn update-rabbit [state rabbit]
  (if (zero? (:energy rabbit))
    (do (destroy! :rabbits (:id rabbit))
        rabbit)
    (let [[grass-id grass] (closest-grass state rabbit)]
      (if grass
        (if (pos? (distance rabbit grass))
          (move-towards rabbit grass)
          (do
            (destroy! :grass grass-id)
            (update rabbit :energy + (get-in config [:rabbit :nutrition-energy]))))
        (move-randomly rabbit)))))


(defn update-all-rabbits [state]
  (reduce (fn [acc [id rabbit]] (assoc acc id (update-rabbit state rabbit)))
          {}
          (:rabbits state)))

(defn resolve-destroyed [state]
  (reduce (fn [acc {:keys [type id] :as _destroyed}]
            (update-in acc [type] dissoc id))
          state
          (:destroyed @interactions)))

(defn resolve-interactions [state]
  (let [new-state (-> state
                      (resolve-destroyed))]
    #_(prn @interactions)
    (reset! interactions initial-interations)
    new-state))

(defn update-grass [state]
  (let [fresh-grass (into {} (map new-grass
                                  (range (- (get-in config [:grass :max-amount])
                                            (count (:grass state))))))]
    (merge (:grass state) fresh-grass)))


(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (let [new-rabbits (update-all-rabbits state)
        new-grass (update-grass state)]
    (-> state
        (assoc :rabbits new-rabbits)
        (assoc :grass new-grass)
        (resolve-interactions))))



;;; drawing


(defn draw-sight [being]
  (let [s (sight-box being)]
    (q/fill 0 0 0 0)
    (q/rect (->size (:xmin s))
            (->size (:ymin s))
            (->size (:size s))
            (->size (:size s)))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 137 125 123)
  (doseq [i (concat (vals (:rabbits state))
                    (vals (:grass state)))]
    (let [xx (->size (:x i))
          yy (->size (:y i))]
      ; Set circle color.
      (apply q/fill (get colors (:type i)))
      ; Calculate x and y coordinates of the circle.
      (q/rect xx yy unit-size unit-size)
      (when (= :rabbit (:type i))
        (draw-sight i)))))


(q/defsketch prey
  :title "Game of life"
  :size [world-size world-size]
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
