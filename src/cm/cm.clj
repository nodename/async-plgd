(ns cm.cm
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

;; grid is a qXq matrix of nodes (example: q = 3)
;; a node has coordinates qi, qj which range from 1 to q inclusive
;; each node has a mXm subgrid (example: m = 2) plus 4m boundary elements
;; NOTE m must be even
;; n = m * q

(defn initial
  [n [u1 u2 u3 u4 u5]]
  (fn [i j]
    (cond
      (zero? i) u1
      (= (inc n) i) u2
      (= (inc n) j) u3
      (zero? j) u4
      :else u5)))

(defn newgrid-row
  [m initialize i0 j0 i]
  (let [f (fn [row j]
            (conj row (initialize (+ i0 i) (+ j0 j))))]
  (reduce f [] (range (+ 2 m)))))
      
(defn newgrid
  [m initialize qi qj]
  (let [i0 (* (dec qi) m)
        j0 (* (dec qj) m)
        f (fn [grid i]
            (conj grid (newgrid-row m initialize i0 j0 i)))]
    (reduce f [] (range (+ 2 m)))))

(defn phase1-step
  [q m qi qj channels u k]
  (let [{:keys [north south east west]} channels
        out (chan)]
    (go
      (let [u (if (> qi 1)
                (assoc-in u [0 k] (<! north))
                u)
            _ (when (< qi q) (>! south ((u m) k)))
            _ (when (< qj q) (>! east ((u k) m)))
            u (if (> qj 1)
                (assoc-in u [k 0] (<! west))
                u)]
        (>! out u)))
    out))

(defn exchange-phase1
  [q m qi qj b channels u]
  ;; qi row number, qj column number
  ;; qi, qj go from 1 to q inclusive
  (let [in (chan)
        out (chan)
        last (- m b)]
    (go
      (<! in)
      (let [new-u (loop [k (- 2 b)
                         u u]
                    (if (> k last)
                      u
                      (recur (+ 2 k) (<! (phase1-step q m qi qj channels u k)))))]
        (>! out new-u)))
    {:in in :out out}))

(defn phase2-step
  [q m qi qj channels u k]
  (let [{:keys [north south east west]} channels
        out (chan)]
    (go
      (let [_ (when (> qi 1) (>! north ((u 1) k)))
            u (if (< qi q)
                (assoc-in u [(inc m) k] (<! south))
                u)
            u (if (< qj q)
                (assoc-in u [k (inc m)] (<! east))
                u)
            _ (when (> qj 1) (>! west ((u k) 1)))]
        (>! out u)))
    out))

(defn exchange-phase2
  [q m qi qj b channels u]
  (let [in (chan)
        out (chan)
        last (dec (+ m b))]
    (go
      (<! in)
      (let [new-u (loop [k (inc b)
                         u u]
                    (if (> k last)
                      u
                      (recur (+ 2 k) (<! (phase2-step q m qi qj channels u k)))))]
        (>! out new-u)))
    {:in in :out out}))

(defn exchange
  [q m qi qj b channels u]
  (let [in (chan)
        out (chan)]
    (go
      (<! in)
      (let [p1 (exchange-phase1 q m qi qj b channels u)
            _ (>! (p1 :in) :start)
            u (<! (p1 :out))
            p2 (exchange-phase2 q m qi qj b channels u)
            _ (>! (p2 :in) :start)
            u (<! (p2 :out))]
        (>! out u)))
    {:in in :out out}))

(defn relax-phase
  [transition q m qi qj channels u b]
  (let [assoc-next-state-in (fn [u i j]
                              (let [nextij (transition ((u i) j) ((u (dec i)) j) ((u (inc i)) j) ((u i) (inc j)) ((u i) (dec j)))]
                                (assoc-in u [i j] nextij)))
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
        (let [x (exchange q m qi qj (- 1 b) channels u)
              _ (>! (x :in) :start)
              u (<! (x :out))]
          (>! out (assoc-next-states-in u))))
      out)))
      
(defn relaxation-step
  [transition q m qi qj channels u]
  (let [out (chan)]
    (go
      (let [u (<! (relax-phase transition q m qi qj channels u 0))
            u (<! (relax-phase transition q m qi qj channels u 1))]
        (>! out u)))
    out))

(defn relaxation
  [steps transition]
  (fn [q m qi qj channels u]
    (let [out (chan)]
      (go
        (loop [step 0
               u u]
          (if (= step steps)
            (>! out u)
            (recur (inc step) (<! (relaxation-step transition q m qi qj channels u))))))
      out)))

(defmacro copy
  [count in out]
  `(dotimes [_# ~count]
    (>! ~out (<! ~in))))

(defn output
  [q m qi qj in out subgrid]
  (let [start (chan)]
    (go
      (<! start)
      (dotimes [i m]
        (let [ii (inc i)]
          (dotimes [j m]
            (let [jj (inc j)]
              (>! out ((subgrid ii) jj))))
          (copy (* (- q qj) m) in out)))
      (copy (* (- q qi) m m q) in out))
    start))
  
(defn node
  [initialize relax q m qi qj channels]
  ;; qi row number; qj column number
  (let [{:keys [east west]} channels]
    (go
      (let [u (newgrid m initialize qi qj)
            u (<! (relax q m qi qj channels u))
            output-process (output q m qi qj east west u)]
        (>! output-process :start)))))

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
  [n in]
  (let [out (chan)]
    (go
      (let [grid (loop [i 0
                        grid []]
                   (if (= i n)
                     grid
                     (recur (inc i) (conj grid (get-row n in)))))]
        (>! out grid)))
    out))

(defn simulate
  ;; m must be even!
  [q m steps application]
  (let [{:keys [initial-values transition]} application
        line (fn [] (vec (repeatedly (inc q) chan))) ;; we only use elements 1 through q of line
        matrix (fn [] (vec (repeatedly (inc q) line)))
        ew-channels (matrix)
        ns-channels (matrix)
        n (* q m)
        initialize (initial n initial-values)
        relax (relaxation steps transition)]
    
    (go (>! p-printer (<! (master n ((ew-channels 0) q)))))
    
    (doseq [i (range 1 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) 1)
                      :south ((ns-channels i) 1)
                      :east ((ew-channels i) 1)
                      :west ((ew-channels (dec i)) q)}]
        (node initialize relax q m i 1 channels)))
    
    (doseq [i (range 1 (inc q))
            j (range 2 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) j)
                      :south ((ns-channels i) j)
                      :east ((ew-channels i) j)
                      :west ((ew-channels i) (dec j))}]
        (node initialize relax q m i j channels)))))
    
