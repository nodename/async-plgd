(ns cm.laplace
  (:require [cm.cellular :refer [simulate]]))

(def initial-values
  {:north-boundary 0
   :south-boundary 100
   :east-boundary 100
   :west-boundary 0
   :interior 50})

(defn transitioner
  "In steady state, the temperature of every interior cell
is the average of the neighboring temperatures. This is the
discrete form of Laplace's equation. The residual is a measure
of how close the temperatures are to satisfying this equation.
The correction of a temperature is proportional to its residual.

For a large square grid relaxed in parity order, the relaxation factor
    fopt = 2 - 2*pi/n
ensures the fastest possible convergence towards stationary temperatures.
In numerical analysis, this method is called successive overrelaxation
with parity ordering."
  [n]
  (let [fopt (- 2 (/ (* 2 Math/PI) n))]
    (fn
      [subgrid i j]
      (let [uc ((subgrid i) j)
            un ((subgrid (dec i)) j)
            us ((subgrid (inc i)) j)
            ue ((subgrid i) (inc j))
            uw ((subgrid i) (dec j))]
        (let [residual (- (/ (+ un us ue uw) 4.0) uc)]
          (+ uc (* fopt residual)))))))

(defn run
  [q m steps]
  (let [application {:initial-values initial-values
                     :transition (transitioner (* q m))}]
    (simulate q m steps application)))