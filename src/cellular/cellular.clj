(ns cellular.cellular
  (:require [clojure.core.async :refer :all]
            [clojure.pprint :refer [pprint]]))

(defn pprinter []
  (let [in (chan)]
    (go (while true
          (pprint (<! in))))
    in))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))

(def p-printer (pprinter))
(def flow-printer (sink))
(def u-printer (sink))
(def val-printer (sink))

(defn initializer
  [n initial-values]
  (let [{:keys [north-boundary south-boundary east-boundary west-boundary interior]} initial-values]
    (fn [i j]
      (cond
        (zero? i) north-boundary
        (= (inc n) i) south-boundary
        (= (inc n) j) east-boundary
        (zero? j) west-boundary
        :else interior))))

(defn newgrid-row
  [m initialize i0 j0 i]
  (let [f (fn [row j]
            (conj row (initialize (+ i0 i) (+ j0 j))))]
    (reduce f [] (range (+ 2 m)))))
      
(defn newgrid
  [m initialize]
  (fn [qi qj]
    (let [i0 (* (dec qi) m)
          j0 (* (dec qj) m)
          f (fn [grid i]
              (conj grid (newgrid-row m initialize i0 j0 i)))]
      (reduce f [] (range (+ 2 m))))))

(defn phase1-step
  [q m qi qj channels u k]
  (let [{:keys [north south east west]} channels
        out (chan)]
    (go
      (when (< qi q) (>! south ((u m) k)))
      (when (< qj q) (>! east ((u k) m)))
      (go
        (let [u (if (> qi 1)
                  (assoc-in u [0 k] (<! north))
                  u)
              u (if (> qj 1)
                  (assoc-in u [k 0] (<! west))
                  u)]
          (>! out u))))
    out))

(defn exchange-phase1
  [q m qi qj b channels u]
  ;; qi row number, qj column number
  ;; qi, qj go from 1 to q inclusive
  (let [out (chan)
        last (- m b)]
    (go
      (let [new-u (loop [k (- 2 b)
                         u u]
                    (if (> k last)
                      u
                      (recur (+ 2 k) (<! (phase1-step q m qi qj channels u k)))))]
        (>! out new-u)))
    out))

(defn phase2-step
  [q m qi qj channels u k]
  (let [{:keys [north south east west]} channels
        out (chan)]
    (go
      (when (> qi 1) (>! north ((u 1) k)))
      (when (> qj 1) (>! west ((u k) 1)))
      (go
        (let [u (if (< qi q)
                  (assoc-in u [(inc m) k] (<! south))
                  u)
              u (if (< qj q)
                  (assoc-in u [k (inc m)] (<! east))
                  u)]
          (>! out u))))
    out))

(defn exchange-phase2
  [q m qi qj b channels u]
  (let [out (chan)
        last (dec (+ m b))]
    (go
      (let [new-u (loop [k (inc b)
                         u u]
                    (if (> k last)
                      u
                      (recur (+ 2 k) (<! (phase2-step q m qi qj channels u k)))))]
        (>! out new-u)))
    out))

(defn exchange
  [q m qi qj b channels u]
  (let [out (chan)]
    (go
      (let [u (<! (exchange-phase1 q m qi qj b channels u))
            u (<! (exchange-phase2 q m qi qj b channels u))]
        (>! out u)))
    out))

(defn relax-phase
  [transition q m qi qj channels]
  (fn [u b]
    (let [assoc-next-state-in (fn [u i j]
                                (assoc-in u [i j] (transition u i j)))
          assoc-row-of-next-states-in (fn [u i]
                                        (let [k (mod (+ i b) 2)
                                              last (- m k)
                                              f (fn [u j]
                                                  (assoc-next-state-in u i j))]
                                          (reduce f u (range (- 2 k) (inc last) 2))))
          assoc-next-states-in (fn [u]
                                 (reduce assoc-row-of-next-states-in u (range 1 (inc m))))]
      
      (let [out (chan)]
        (go
          (let [u (<! (exchange q m qi qj (- 1 b) channels u))
                u (assoc-next-states-in u)]
            (>! out u)))
        out))))
      
(defn relaxation-step
  [transition q m qi qj channels u]
  (let [out (chan)]
    (go
      (let [relaxation-phase (relax-phase transition q m qi qj channels)
            u (<! (relaxation-phase u 0))
            u (<! (relaxation-phase u 1))]
        (>! out u)))
    out))

(defn relaxation
  [q m steps transition]
  (fn [qi qj channels u]
    (let [out (chan)]
      (go
        (let [u (loop [step 0
                       u u]
                  (if (= step steps)
                    u
                    (recur (inc step) (<! (relaxation-step transition q m qi qj channels u)))))]
          (>! out u)))
      out)))

(defmacro copy
  [count in out]
  `(dotimes [_# ~count]
    (>! ~out (<! ~in))))

(defn outputter
  [q m]
  (fn [qi qj in out subgrid]
  (go
    (dotimes [i m]
      (let [ii (inc i)]
        (dotimes [j m]
          (let [jj (inc j)]
            (>! out ((subgrid ii) jj))))
        (copy (* (- q qj) m) in out)))
    (copy (* (- q qi) m m q) in out))))
  
(defn node
  [init relax output]
  (fn [qi qj channels]
  ;; qi row number; qj column number
    (let [{:keys [east west]} channels]
      (go
        (let [u (init qi qj)
              u (<! (relax qi qj channels u))]
          (output qi qj east west u))))))

(defmacro get-row
  [n in]
  `(loop [j# 0
         row# []]
    (if (= j# ~n)
      row#
      (recur (inc j#) (conj row# (<! ~in))))))

(defn master
  "Input the final grid of nXn values (states) from the processors, one element at a time.
The single input channel comes from the output function of channel h0q (the last channel of
the north boundary row, i.e. the channel that receives the output from the pipeline threaded
through the interior elements only."
  [n in start-time]
  (let [out (chan)]
    (go
      (let [grid (loop [i 0
                        grid []]
                   (if (= i n)
                     grid
                     (recur (inc i) (conj grid (get-row n in)))))
            elapsed-ms (long (/ (- (System/nanoTime) start-time) 1000000))]
        (>! out [elapsed-ms grid])))
    out))

(defn simulate
"Create a matrix of qXq processor nodes.
Every node is connected to its nearest neighbors (if any)
by four communication channels named north, south, east, and west.
Each processor node is responsible for a subgrid of mXm data cells
within the complete nXn grid, where n = q * m
(m must be even because we did not bother to code for the odd case.)
After initializing its subgrid, each node will update the subgrid
a fixed number of times (specified by the steps parameter)
before outputting the final values.
The nodes will update their subgrids simultaneously.
In numerical analysis, grid iteration is known as relaxation.

The nXn data grid is surrounded by a row of boundary cells on each side.
The application object must specify:
    a fixed value for the elements of each boundary
    and an initial value for the interior elements;
    and a transition function that returns the next value for a cell
    given the subgrid and the cell's position in the subgrid.
"
  [q m steps application]
  (let [{:keys [initial-values transition]} application
        chan-row #(vec (repeatedly (inc q) chan)) ;; we only use elements 1 through q of chan-row
        chan-matrix #(vec (repeatedly (inc q) chan-row))
        ew-channels (chan-matrix)
        ns-channels (chan-matrix)
        n (* q m)
        initialize (initializer n initial-values)
        init (newgrid m initialize)
        relax (relaxation q m steps transition)
        output (outputter q m)
        start-node (node init relax output)
        start-time (System/nanoTime)]
    
    (go (>! p-printer (<! (master n ((ew-channels 0) q) start-time))))
    
    ;; node coordinates range from 1 to q inclusive
    
    (doseq [i (range 1 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) 1)
                      :south ((ns-channels i) 1)
                      :east ((ew-channels i) 1)
                      :west ((ew-channels (dec i)) q)}]
        (start-node i 1 channels)))
    
    (doseq [i (range 1 (inc q))
            j (range 2 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) j)
                      :south ((ns-channels i) j)
                      :east ((ew-channels i) j)
                      :west ((ew-channels i) (dec j))}]
        (start-node i j channels)))))
    
