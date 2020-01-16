(ns prey.config)

(def debug? false #_ true)
(def grid-size 40)
(def unit-size 10)
(def world-size (* grid-size unit-size))
(def sight-radius 5)
(def fps 20)
(def background-color [137 125 123])

(def config
  {:prey {:color {:male [204 230 255]
                  :female [255 179 230]}
          :pregnant-color [255 0 0]
          :initial-density 0.015
          :pregnancy-duration 5
          :litter-size 2
          :max-age 400
          :starve-at 40
          :crossover-probability 0.2
          :mutation-probability 0.3
          :hunger-threshold 10
          :desire-threshold 25
          :direction-inertia 10
          :nutrition 10
          :speed 1}
   :food {:density 0.02
          :color [35 89 8]
          :lifespan 10}})
