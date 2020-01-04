(ns prey.terrain
  (:require [taoensso.tufte :as t]
            [prey.config :as config]))

(defn all-neighbors [x y]
  (let [n (fn [z] (cond-> [z]
                    (pos? z) (conj (dec z))
                    (< z (dec config/grid-size)) (conj (inc z))))]
    (for [xx (n x)
          yy (n y)
          ] ;;   :when (or (not= x xx) (not= y yy))
      [xx yy])))

(defn refine [water-points]
  (set (remove nil? (for [x (range 0 config/grid-size)
                          y (range 0 config/grid-size)]
                      (let [neighbors (all-neighbors x y)
                            water-neighbors (filter #(contains? water-points %) neighbors)]
                        (when (<= (count water-neighbors) 3)
                          [x y]))))))

(defn generate []
  (into #{} (repeatedly (Math/round (double (* config/grid-size config/grid-size 0.45)))
                        (fn [] [(rand-int config/grid-size)
                                (rand-int config/grid-size)]))))


(defn initialize []
  (if config/terrain-disabled?
    #{}
    (nth (iterate refine (generate))
         20)))

(comment
  (t/defnp random-set [n]
    (let [s (t/p :s (set (t/p :rr (repeatedly n (fn [] [(rand-int 1000)
                                                        (rand-int 1000)])))))]
      (frequencies (repeatedly n (fn [] (let [x (t/p :x [(rand-int 1000)
                                                         (rand-int 1000)])]
                                          (t/p :contains? (contains? s x))))))))
  (t/defnp random-hashmap [n]
    (let [s (t/p :ss (reduce (fn [acc _] (assoc acc [(rand-int 1000)
                                                     (rand-int 1000)] true)) {} (range n)))]
      (frequencies (repeatedly n (fn [] (let [x (t/p :x [(rand-int 1000)
                                                         (rand-int 1000)])]
                                          (t/p :contains? (get s x)))))))))