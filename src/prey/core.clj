(ns prey.core
  (:require [quil.core :as q]
            [prey.food :as food]
            [prey.prey :as prey]
            [prey.chart :as chart]
            [prey.actions :as actions]
            [prey.terrain :as terrain]
            [prey.config :as config]
            [prey.util :as util]
            [quil.middleware :as m]
            [clojure.core.async :as async]))

(def initial-live-run {:data {:population-size []
                              :average-generation []}})
(def initial-last-run {})

(def last-run (atom initial-last-run))
(def live-stats (atom initial-live-run))

(defn preys-stats [preys]
  (let [#_#_ grouped-genders (group-by :gender preys)
        #_#_ males (:male grouped-genders)
        #_#_ females (:female grouped-genders)
        generations (map :generation preys)]
    {#_#_:nmales (count males)
     #_#_:nfemales (count females)
     :average-generation (when (seq generations) (util/mean generations))
     :population-size (count preys)
     #_#_:gender-ratio (when (pos? (count females))
                         (/ (count males)
                            (count females)))}))

(defn save-stats [state]
  (swap! live-stats (fn [a] (if-let [preys (seq (:preys state))]
                            (let [stats (preys-stats (vals preys))]
                              (-> a
                                  (update-in [:data :population-size] (fnil conj []) (:population-size stats))
                                  (update-in [:data :average-generation] (fnil conj []) (:average-generation stats))))
                            a)))
  (swap! last-run (fn [a] (if-let [preys (seq (:preys state))]
                            (update a :preys (fnil conj []) (vals preys))
                            a)))
  state)



(defn analyze-last-run []
  (let [stats (->> (:preys @last-run)
                   (map preys-stats))]
    (chart/line-chart {:data (map :population-size stats)
                       :title "Population"}))
  )

(defn print-preys [state]
  (doseq [[_pid p] (:preys state)]
    (clojure.pprint/pprint p)))

(defn print-actions [actions]
  (prn "------------")
  (prn (q/frame-count))
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
     :preys (prey/initialize-preys terrain)}))

(defn debug-initialize-world []
  {:food (food/debug-initialize-food)
   :terrain (terrain/debug-initialize)
   :preys (prey/debug-initialize-preys)})


(defn setup []
  (q/frame-rate config/fps)
  (reset! last-run {})
  (if config/debug?
    (debug-initialize-world)
    (initialize-world)))

;;; update

(defn tick-prey [prey]
  (cond-> prey
    :always (update :hunger inc)
    :always (update :age inc)
    (not (:pregnant? prey)) (update :desire inc)
    (:pregnant? prey) (update :pregnancy (fnil inc 0))))

(defn tick-world [state]
  (-> state
      (update :preys (fn [preys]
                       (reduce (fn [acc [prey-id prey]]
                                 (assoc acc prey-id (tick-prey prey))) {} preys)))))


(defn update-state [state]
  (let [prey-actions (pmap (fn [[_prey-id prey]] (prey/take-decision prey state))
                           (:preys state))
        food-actions (food/replenish-food-txs state)
        actions (concat prey-actions food-actions)]
    (when config/debug?
      (print-actions actions)
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
    (chart/live-line-chart live-stats :population-size "Population"))
  (async/thread
    (chart/live-line-chart live-stats :average-generation "Avg gen")))

#_(q/defsketch prey
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
