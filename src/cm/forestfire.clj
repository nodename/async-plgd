(ns cm.forestfire
  (:require [cm.cm :refer [simulate]]))

(def initial-values
  [:dead :dead :dead :dead :alive])

(defn transition
  "If a live tree is next to a burning tree, it burns;
otherwise, it catches fire with probability p1.
A burning tree dies.
A dead tree has probability p2 of being replaced by a live tree."
  [uc un us ue uw]
  (let [p1 0.01
        p2 0.3]
    (condp = uc
      :alive (cond
               (some #(= % :burning) [un us ue uw]) :burning
               (<= (Math/random) p1) :burning
               :else :alive)
      :burning :dead
      (cond
        (<= (Math/random) p2) :alive
        :else :dead))))

(def forest-fire {:initial-values initial-values
                  :transition transition})
