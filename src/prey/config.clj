(ns prey.config)

(def debug? false)
(def grid-size (if debug? 10 40))
(def unit-size 10)
(def world-size (* grid-size unit-size))
(def sight-radius 5)
(def fps (if debug? 1 20))
(def background-color [137 125 123])

(def config
  {:prey     {:color                 {:male   [204 230 255]
                                      :female [255 179 230]}
              :pregnant-color        [255 0 0]
              :initial-density       0.025
              :initial-energy        40
              :gestation             5
              :litter-size           3
              :maturity-at           10
              :max-age               400
              :competition-threshold 3
              :crossover-probability 0.2
              :mutation-probability  0.1
              :energy-threshold      100
              :desire-threshold      25
              :direction-inertia     10
              :nutrition             20
              :speed                 1}
   :predator {:color             {:male   [57 73 249]
                                  :female [255 116 91]}
              :pregnant-color    [0 255 0]
              :direction-inertia 10
              :initial-density   0.001}
   :food     {:density  0.02
              :color    [35 89 8]
              :lifespan 10}})
