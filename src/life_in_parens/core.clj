(ns life-in-parens.core
  (:require
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [life-in-parens.actions :as actions]
    [life-in-parens.chart :as chart]
    [life-in-parens.config :as config]
    [life-in-parens.food :as food]
    [life-in-parens.predator :as predator]
    [life-in-parens.prey :as prey]
    [life-in-parens.terrain :as terrain]
    [life-in-parens.util :as util]
    [quil.core :as q]
    [quil.middleware :as m]))

(defonce running? (atom true))

(def initial-live-run {:data {:prey-population     []
                              :predator-population []
                              :generation          []
                              :energies            []
                              :last-tick           0}})
(def initial-last-run {})

(defonce last-run (atom initial-last-run))
(defonce live-stats (atom initial-live-run))

(defn triplet [quantity-seq]
  {:avg (util/mean quantity-seq)
   :min (apply min quantity-seq)
   :max (apply max quantity-seq)})

(defn average-dna [preys]
  (let [dnas (map :dna preys)
        total-dnas (reduce (fn [acc dna]
                             (reduce (fn [acc [gene value]]
                                       (cond
                                         (boolean? value) (update acc gene conj (if value 1.0 0.0))
                                         (coll? value) acc
                                         :else (update acc gene conj value)))
                                     acc
                                     dna))
                           {}
                           dnas)]
    (->> total-dnas
         (map (fn [[gene values]] [gene (double (util/mean values))]))
         (into {}))))

(defn stats [preys predators]
  (let [generations (map :generation preys)
        energies (map :energy preys)]
    {:generation          (when (seq generations) (triplet generations))
     :prey-population     (count preys)
     :predator-population (count predators)
     :energies            (when (seq energies) (triplet energies))}))

(defn save-stats [{:keys [preys predators] :as state}]
  (when-not config/debug?
    (swap! live-stats (fn [a]
                        (let [stats (stats (vals preys) (vals predators))]
                          (-> a
                              (assoc-in [:data :last-tick] (q/frame-count))
                              (update-in [:data :predator-population] (fnil conj []) (:predator-population stats))
                              (update-in [:data :prey-population] (fnil conj []) (:prey-population stats))
                              (update-in [:data :generation] (fnil conj []) (:generation stats))
                              (update-in [:data :energies] (fnil conj []) (:energies stats))))))
    (swap! last-run (fn [a] (-> a
                                (update :preys (fnil conj []) (vals preys))
                                (update :predators (fnil conj []) (vals predators))))))
  state)

(defn last-gen []
  (last (:preys @last-run)))

(defn plot-population []
  (chart/line-chart {:data [(map count (:preys @last-run))]} {:adapt? true :title (str/capitalize "Prey population")}))

(defn plot-average-dnas []
  (let [average-dnas (map average-dna (:preys @last-run))
        genes (keys (last average-dnas))]
    (doseq [gene genes]
      (chart/line-chart {:data [(map gene average-dnas)]} {:adapt? true :title (str/capitalize (name gene))}))))

(defn print-preys [state]
  (when-let [preys (:preys state)]
    (prn "------------")
    (prn "PREYS: ")
    (doseq [[_pid p] preys]
      (pprint/pprint p))))

(defn print-predators [state]
  (when-let [predators (seq (:predators state))]
    (prn "------------")
    (prn "PREDATORS: ")
    (doseq [[_pid p] predators]
      (pprint/pprint p))))

(defn print-actions [actions]
  (prn "------------")
  (prn (q/frame-count))
  (prn "ACTIONS: ")
  (doseq [a actions]
    (pprint/pprint a)))

(defn color [being]
  (cond
    (:dead? being) (get-in config/config [(:type being) :dead-color])
    (:pregnant? being) (get-in config/config [(:type being) :pregnant-color])
    (:gender being) (get-in config/config [(:type being) :color (:gender being)])
    :else (get-in config/config [(:type being) :color])))

(defn ->size [i] (* i config/unit-size))

(defn initialize-world []
  (let [terrain (terrain/initialize)]
    {:food      (food/initialize-food terrain)
     :terrain   terrain
     :preys     (prey/initialize terrain)
     :predators (predator/initialize terrain)}))

(defn debug-initialize-world []
  {:food      (food/debug-initialize-food)
   :terrain   (terrain/debug-initialize)
   :preys     (prey/debug-initialize)
   :predators (predator/debug-initialize)})


(defn setup []
  (q/frame-rate config/fps)
  (reset! last-run {})
  (if config/debug?
    (debug-initialize-world)
    (initialize-world)))

;;; update

(defn tick [being]
  (cond-> being
          :always (update :energy #(- % (get-in being [:dna :speed])))
          :always (update :age inc)
          (:dead? being) (update :dead-since inc)
          (not (:pregnant? being)) (update :desire inc)
          (:pregnant? being) (update :pregnancy (fnil inc 0))))

(defn tick-all-beings [beings]
  (reduce (fn [acc [being-id being]]
            (assoc acc being-id (tick being)))
          {}
          beings))

(defn tick-world [state]
  (-> state
      (update :preys tick-all-beings)
      (update :predators tick-all-beings)))


(defn update-state [state]
  (if @running?
    (let [map-fn map #_(when config/debug? map pmap)
          prey-actions (map-fn (fn [[_prey-id prey]] (prey/take-decision prey state))
                               (:preys state))
          predator-actions (map-fn (fn [[_predator-id predator]] (predator/take-decision predator state))
                                   (:predators state))
          food-actions (food/replenish-food-txs state)
          actions (apply concat (concat predator-actions prey-actions food-actions))]
      (when config/debug?
        (print-actions actions)
        (print-predators state)
        (print-preys state))
      (-> (reduce actions/resolve-action* state actions)
          (tick-world)
          (save-stats)))
    state))

;;; drawing

(defn draw-terrain [state]
  (doseq [terrain-pixel (:terrain state)]
    (q/fill 45 206 255)
    (q/no-stroke)
    (q/rect (->size (first terrain-pixel))
            (->size (second terrain-pixel))
            (->size 1)
            (->size 1))))

#_(defn draw-sight [being]
    (let [s (util/sight-box being)]
      (q/fill 0 0 0 0)
      (q/stroke 1)
      (q/stroke-weight 1)
      (q/rect (->size (:xmin s))
              (->size (:ymin s))
              (->size (:size s))
              (->size (:size s)))))

(defn draw-state [state]
  (apply q/background config/background-color)
  (draw-terrain state)
  (let [beings (concat (vals (:food state))
                       (vals (:preys state))
                       (vals (:predators state)))]
    (doseq [being beings]
      (let [xx (->size (:x being))
            yy (->size (:y being))]
        (apply q/fill (color being))
        (q/no-stroke)
        (q/rect xx yy config/unit-size config/unit-size)))))

(defn run []
  (reset! live-stats initial-live-run)
  (reset! last-run initial-last-run)
  (q/sketch
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
  (when-not config/debug?
    (async/thread
      (chart/live-lines-chart live-stats
                              [:prey-population :predator-population]
                              {:title       "Population"
                               :rounding-at 10})))
  #_(async/thread
      (chart/live-min-max-avg-chart live-stats :energies {:title       "Energies"
                                                          :rounding-at 1})))
