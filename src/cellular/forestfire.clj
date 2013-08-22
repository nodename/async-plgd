(ns cellular.forestfire
  (:require [cellular.cellular :refer [simulate]]
            [utils.helpers :refer :all]))

(def initial-values
  {:north-boundary :dead
   :south-boundary :dead
   :east-boundary :dead
   :west-boundary :dead
   :interior :alive})

(defn transition
  "If a live tree is next to a burning tree, it burns;
otherwise, it catches fire with probability p1.
A burning tree dies.
A dead tree has probability p2 of being replaced by a live tree."
  [subgrid i j]
  (let [uc ((subgrid i) j)
        un ((subgrid (dec i)) j)
        us ((subgrid (inc i)) j)
        ue ((subgrid i) (inc j))
        uw ((subgrid i) (dec j))]
    (let [p1 0.01
          p2 0.3]
      (condp = uc
        :alive (cond
                 (some #{:burning} [un us ue uw]) :burning
                 (<= (Math/random) p1) :burning
                 :else :alive)
        :burning :dead
        (cond
          (<= (Math/random) p2) :alive
          :else :dead)))))

(defn run
  [q m steps]
  (let [application {:initial-values initial-values
                     :transition transition}]
    (simulate q m steps application (pprinter))))