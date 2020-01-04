(ns prey.config)

(def terrain-disabled? true)
(def debug? false)
(def grid-size 20)
(def unit-size 20)
(def world-size (* grid-size unit-size))
(def sight-radius 5)
(def fps 10)

(def config
  {:prey {:color {:male [204 230 255]
                  :female [255 179 230]}
          :initial-density 0.005
          :pregnancy-duration 5
          :starvation-hunger 40
          :desire-threshold 25
          :direction-inertia 10
          :nutrition 10
          :speed 1}
   :food {:density 0.01
          :color [35 89 8]
          :lifespan 10}})
