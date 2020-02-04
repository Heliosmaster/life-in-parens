(ns prey.core
  (:require
    [clojure.core.async :as async]
    [clojure.string :as str]
    [prey.actions :as actions]
    [prey.chart :as chart]
    [prey.config :as config]
    [prey.food :as food]
    [prey.predator :as predator]
    [prey.prey :as prey]
    [prey.terrain :as terrain]
    [prey.util :as util]
    [quil.core :as q]
    [quil.middleware :as m]))

(def initial-live-run {:data {:population-size []
                              :generation []
                              :energies []
                              :last-tick 0}})
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

(defn preys-stats [preys]
  (let [generations (map :generation preys)
        energies (map :energy preys)]
    {:generation (when (seq generations) (triplet generations))
     :population-size (count preys)
     :energies (when (seq energies) (triplet energies))}))

(defn save-stats [state]
  (swap! live-stats (fn [a] (if-let [preys (seq (:preys state))]
                              (let [stats (preys-stats (vals preys))]
                                (-> a
                                    (assoc-in [:data :last-tick] (q/frame-count))
                                    (update-in [:data :population-size] (fnil conj []) (:population-size stats))
                                    (update-in [:data :generation] (fnil conj []) (:generation stats))
                                    (update-in [:data :energies] (fnil conj []) (:energies stats))))
                              a)))
  (swap! last-run (fn [a] (if-let [preys (seq (:preys state))]
                            (update a :preys (fnil conj []) (vals preys))
                            a)))
  state)

(defn last-gen []
  (last (:preys @last-run)))

(defn plot-average-dnas []
  (let [average-dnas (map average-dna (:preys @last-run))
        genes (keys (last average-dnas))]
    (doseq [gene genes]
      (chart/line-chart {:data (map gene average-dnas)} {:adapt? true :title (str/capitalize (name gene))}))))


(defn analyze-last-run []
  (let [stats (->> (:preys @last-run)
                   (map preys-stats))]
    (chart/line-chart {:data (map :population-size stats)
                       :title "Population"}
                      {}))
  )

(defn print-preys [state]
  (prn "------------")
  (prn "PREYS: ")
  (doseq [[_pid p] (:preys state)]
    (clojure.pprint/pprint p)))

(defn print-predators [state]
  (prn "------------")
  (prn "PREDATORS: ")
  (doseq [[_pid p] (:predators state)]
    (clojure.pprint/pprint p)))

(defn print-actions [actions]
  (prn "------------")
  (prn (q/frame-count))
  (prn "ACTIONS: ")
  (doseq [a actions]
    (clojure.pprint/pprint a)))

(defn print-stats [state]
  (let [{:keys [nmales nfemales]} (preys-stats state)]
    (prn (format "%5d: Male: %5d - Female: %5d - Ratio: %2.2f" (q/frame-count) nmales nfemales (if (and (pos? nmales)
                                                                                                        (pos? nfemales))
                                                                                                 (double (/ nmales nfemales))
                                                                                                 0.0)))))
(defn color [being] ;; FIXME temporary pregnant just for debugging
  (if (:pregnant? being)
    (get-in config/config [(:type being) :pregnant-color])
    (if (:gender being)
      (get-in config/config [(:type being) :color (:gender being)])
      (get-in config/config [(:type being) :color]))))

(defn ->size [i] (* i config/unit-size))

(defn initialize-world []
  (let [terrain (terrain/initialize)]
    {:food (food/initialize-food terrain)
     :terrain terrain
     :preys (prey/initialize-preys terrain)
     :predators (predator/initialize terrain)}))

(defn debug-initialize-world []
  {:food (food/debug-initialize-food)
   :terrain (terrain/debug-initialize)
   :preys (prey/debug-initialize-preys)
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
  (let [prey-actions (map (fn [[_prey-id prey]] (prey/take-decision prey state))
                           (:preys state))
        predator-actions (map (fn [[_predator-id predator]] (predator/take-decision predator state))
                               (:predators state))
        food-actions (food/replenish-food-txs state)
        actions (concat predator-actions prey-actions food-actions)]
    #_(when config/debug?
      (print-actions actions)
      (print-predators state)
      (print-preys state))
    (-> (reduce actions/resolve-action state actions)
        (tick-world)
        (save-stats))))

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
  (apply q/background config/background-color)
  (draw-terrain state)
  (doseq [being (concat (vals (:food state))
                        (vals (:predators state))
                        (vals (:preys state)))]
    (let [xx (->size (:x being))
          yy (->size (:y being))]
      (apply q/fill (color being))
      (q/no-stroke)
      (q/rect xx yy config/unit-size config/unit-size)
      (when (and config/debug? (= :prey (:type being)))
        (draw-sight being)))))

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
  (async/thread
    (chart/live-line-chart live-stats :population-size {:title "Population"
                                                        :rounding-at 10}))
  #_(async/thread
      (chart/live-min-max-avg-chart live-stats :energies {:title "Energies"
                                                          :rounding-at 1})))
