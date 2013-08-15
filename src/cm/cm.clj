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
  [m initial i0 j0 i]
  (let [f (fn [row j]
            (conj row (initial (+ i0 i) (+ j0 j))))]
  (reduce f [] (range (+ 2 m)))))
      
(defn newgrid
  [m initial qi qj]
  (let [i0 (* (dec qi) m)
        j0 (* (dec qj) m)
        f (fn [grid i]
            (conj grid (newgrid-row m initial i0 j0 i)))]
    (reduce f [] (range (+ 2 m)))))

(defn exchange-phase1
  [q m qi qj b [north south east west] u]
  ;; qi row number, qj column number
  ;; qi, qj go from 1 to q inclusive
  (let [in (chan)
        out (chan)
        last (- m b)]
    (go
      (<! in)
      (loop [k (- 2 b)
             u u]
        (if (> k last)
          (>! out u)
          (let [u (if (> qi 1)
                    (assoc-in u [0 k] (<! north))
                    u)
                _ (when (< qi q) (>! south ((u m) k)))
                _ (when (< qj q) (>! east ((u k) m)))
                u (if (> qj 1)
                    (assoc-in u [k 0] (<! west))
                    u)]
            (recur (+ 2 k) u)))))
    {:in in :out out}))

(defn exchange-phase2
  [q m qi qj b [north south east west] u]
  (let [in (chan)
        out (chan)
        last (dec (+ m b))]
    (go
      (<! in)
      (loop [k (inc b)
             u u]
        (if (> k last)
          (>! out u)
          (let [_ (when (> qi 1) (>! north ((u 1) k)))
                u (if (< qi q)
                    (assoc-in u [(inc m) k] (<! south))
                    u)
                u (if (< qj q)
                    (assoc-in u [k (inc m)] (<! east))
                    u)
                _ (when (> qj 1) (>! west ((u k) 1)))]
            (recur (+ 2 k) u)))))
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
  [next-state q m qi qj channels b]
  (let [assoc-next-state-in (fn [u i j]
                              (let [nextij (next-state ((u i) j) ((u (dec i)) j) ((u (inc i)) j) ((u i) (inc j)) ((u i) (dec j)))]
                                (assoc-in u [i j] nextij)))
        assoc-row-of-next-states-in (fn [u i]
                                      (let [k (mod (+ i b) 2)
                                            last (- m k)
                                            f (fn [u j]
                                                (assoc-next-state-in u i j))]
                                        (reduce f u (range (- 2 k) (inc last) 2))))
        assoc-next-states-in (fn [u]
                               (reduce assoc-row-of-next-states-in u (range 1 (inc m))))]
        
    (let [in (chan)
          out (chan)]
      (go
        (let [u (<! in)]
          (let [x (exchange q m qi qj (- 1 b) channels u)
                _ (>! (x :in) :start)
                u (<! (x :out))]
            (>! out (assoc-next-states-in u)))))
      {:in in :out out})))
      
(defn relax
  [next-state q m qi qj channels u]
  (let [out (chan)]
    (go
      (let [r0 (relax-phase next-state q m qi qj channels 0)
            _ (>! (r0 :in) u)
            u0 (<! (r0 :out))
            r1 (relax-phase next-state q m qi qj channels 1)
            _ (>! (r1 :in) u0)
            u1 (<! (r1 :out))]
        (>! out u1)))
    out))

(defn relaxation
  [next-state steps q m qi qj channels u]
  (let [out (chan)]
    (go
      (loop [step 0
             u u]
        (if (= step steps)
          (>! out u)
          (let [r (relax next-state q m qi qj channels u)
                u (<! r)]
            (recur (inc step) u)))))
    out))

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
  [q m initial next-state steps qi qj [north south east west :as channels]]
  ;; qi row number; qj column number
  (go
    (let [u (newgrid m initial qi qj)
          r (relaxation next-state steps q m qi qj channels u)
          u (<! r)
          output-process (output q m qi qj east west u)]
      (>! output-process :start))))

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
    (go (loop [i 0
               grid []]
          (if (= i n)
            (>! out grid)
            (recur (inc i) (conj grid (get-row n in))))))
    out))

(defn simulate
  ;; m must be even!
  [q m steps initial-values next-state]
  (let [line (fn [] (vec (repeatedly (inc q) chan))) ;; we only use elements 1 through q of line
        matrix (fn [] (vec (repeatedly (inc q) line)))
        h (matrix)
        v (matrix)
        initialize (initial (* q m) initial-values)]
    
    (go (>! p-printer (<! (master (* q m) ((h 0) q)))))
    
    (doseq [k (range 1 (inc q))]
      (let [[north south east west :as channels] [((v (dec k)) 1) ((v k) 1) ((h k) 1) ((h (dec k)) q)]]
        (node q m initialize next-state steps k 1 channels)))
    
    (doseq [i (range 1 (inc q))
            j (range 2 (inc q))]
      (let [[north south east west :as channels] [((v (dec i)) j) ((v i) j) ((h i) j) ((h i) (dec j))]]
        (node q m initialize next-state steps i j channels)))))
    
