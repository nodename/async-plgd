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

(comment
(defn outputter
  [n]
  (let [in (chan)]
    (let [row (fn [] (vec (take n (repeat nil))))
          matrix (vec (repeatedly n row))
          elements (* n n)]
      (loop [e elements
             m matrix]
        (let [[i j value] (<! in)]
          (if (zero? e)
            (>! u-printer ['FINAL matrix])
            (recur (dec e) (assoc-in matrix [i j] value))))))
    in))
          
(def output-chan (outputter (* q m)))
) ;; end comment


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

(def forest-fire
  [:dead :dead :dead :dead :alive])

(defn newgrid-row
  [m initial i0 j0 i]
  (loop [j 0
         row []]
    (if (= j (+ 2 m))
      row
      (recur (inc j) (conj row (initial (+ i0 i) (+ j0 j)))))))
      
(defn newgrid
  [m initial qi qj]
  (let [i0 (* (dec qi) m)
        j0 (* (dec qj) m)]
    (loop [i 0
           newgrid []]
      (if (= i (+ 2 m))
        newgrid
        (recur (inc i) (conj newgrid (newgrid-row m initial i0 j0 i)))))))
      
(defn next-state
  "If a live tree is next to a burning tree, it burns;
otherwise, it catches fire with probability p1.
A burning tree dies.
A dead tree has probability p2 of being replaced by a live tree."
  [uc un us ue uw]
  (let [p1 0.01
        p2 0.3]
    (cond
      (= uc :alive) (cond
                      (or (= un :burning) (= us :burning) (= ue :burning) (= uw :burning)) :burning
                      (<= (Math/random) p1) :burning
                      :else :alive)
      (= uc :burning) :dead
      :else (cond
              (<= (Math/random) p2) :alive
              :else :dead))))

(defn neighbor-element
  [test u i j channel]
  (let [c (chan)]
    (go
      (let [val (if test
                  (<! channel)
                  ((u i) j))]
        (>! c val)))
    c))
                         
(defn exchange-phase1
  [q m qi qj b north south east west u]
  ;; qi row number, qj column number
  ;; qi, qj go from 1 to q inclusive
  (let [in (chan)
        out (chan)
        last (- m b)]
    (go
      (<! in)
      (>! flow-printer ['exchange-phase1 qi qj 'start])
    (loop [k (- 2 b)
           u u]
      (if (> k last)
        (>! out u)
        (let [u0k (<! (neighbor-element (> qi 1) u 0 k north))
              _ (when (< qi q) (>! south ((u m) k)))
              _ (when (< qj q) (>! east ((u k) m)))
              uk0 (<! (neighbor-element (> qj 1) u k 0 west))
              u (assoc-in u [0 k] u0k)
              u (assoc-in u [k 0] uk0)]
          (recur (+ 2 k) u)))))
    {:in in :out out}))

(defn exchange-phase2
  [q m qi qj b north south east west u]
  (let [in (chan)
        out (chan)
        last (dec (+ m b))]
    (go (<! in)
    (loop [k (inc b)
           u u]
      (if (> k last)
        (>! out u)
        (let [_ (when (> qi 1) (>! north ((u 1) k)))
              um1k (<! (neighbor-element (< qi q) u (inc m) k south))
              ukm1 (<! (neighbor-element (< qj q) u k (inc m) east))
              _ (when (> qj 1) (>! west ((u k) 1)))
              u (assoc-in u [(inc m) k] um1k)
              u (assoc-in u [k (inc m)] ukm1)]
          (recur (+ 2 k) u)))))
    {:in in :out out}))


(defn exchange
  [q m qi qj b north south east west u]
  (let [in (chan)
        out (chan)]
    (go
      (<! in)
      (let [p1 (exchange-phase1 q m qi qj b north south east west u)
            _ (>! (p1 :in) :start)
            u (<! (p1 :out))
            p2 (exchange-phase2 q m qi qj b north south east west u)
            _ (>! (p2 :in) :start)
            u (<! (p2 :out))]
        (>! out u)))
    {:in in :out out}))

(defn relax-phase
  [q m qi qj north south east west b]
  (let [assoc-next-state-in (fn [u i j]
                              (let [nextij (next-state ((u i) j) ((u (dec i)) j) ((u (inc i)) j) ((u i) (inc j)) ((u i) (dec j)))]
                                (assoc-in u [i j] nextij)))
        assoc-row-of-next-states-in (fn [u i]
                                      (let [k (mod (+ i b) 2)
                                            last (- m k)]
                                        (loop [j (- 2 k)
                                               u u]
                                          (if (<= j last)
                                            (recur (+ j 2) (assoc-next-state-in u i j))
                                            u))))
        assoc-next-states-in (fn [u]
                               (loop [i 1
                                      u u]
                                 (if (> i m)
                                   u
                                   (recur (inc i) (assoc-row-of-next-states-in u i)))))]
    (let [in (chan)
          out (chan)]
      (go
        (let [u (<! in)]
          (let [x (exchange q m qi qj (- 1 b) north south east west u)
                _ (>! (x :in) :start)
                u (<! (x :out))]
            (>! out (assoc-next-states-in u)))))
      {:in in :out out})))
      
(defn relax
  [step q m qi qj north south east west u]
  (let [out (chan)]
    (go
      (when (and (= qi 1) (= qj 1)) (>! u-printer [['relax qi qj]]))
      (let [r0 (relax-phase q m qi qj north south east west 0)
            _ (>! (r0 :in) u)
            u0 (<! (r0 :out))
            _ (when (and (= qi 1) (= qj 1)) (>! u-printer [[qi qj 'After 'step step 'first 'relax-phase] u0]))
            r1 (relax-phase q m qi qj north south east west 1)
            _ (>! (r1 :in) u0)
            u1 (<! (r1 :out))]
        (when (and (= qi 1) (= qj 1)) (>! u-printer [[qi qj 'After 'step step 'second 'relax-phase] u1]))
        (>! out u1)))
      out))

(defn relaxation
  [steps q m qi qj north south east west u]
  (let [out (chan)]
    (go
      (>! flow-printer ['Relaxation qi qj steps 'steps])
      (loop [step 0
             u u]
        (if (= step steps)
          (do
            (when (and (= qi 1) (= qj 1)) (>! u-printer [[qi qj 'relaxation 'returning] u]))
            (>! out u))
          (do
            (when (and (= qi 1) (= qj 1)) (>! u-printer ['gonna 'relax qi qj 'step step]))
            (let [r (relax step q m qi qj north south east west u)
                  u (<! r)]
              (recur (inc step) u))))))
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
              (when (and (= qi 1) (= qj 1)) (>! u-printer [['output ii jj ((subgrid ii) jj)]]))
              (>! out ((subgrid ii) jj))))
          (copy (* (- q qj) m) in out)))
      (copy (* (- q qi) m m q) in out))
    start))
  
(defn node
  [q m initial steps qi qj north south east west]
  ;; qi row number; qj column number
  (go
    (>! flow-printer ['starting 'node qi qj])
    (let [u (newgrid m initial qi qj)]
      (when (and (= qi 1) (= qj 1)) (>! u-printer [[qi qj 'init] u]))
      (let [r (relaxation steps q m qi qj north south east west u)
            u (<! r)
            _ (when (and (= qi 1) (= qj 1)) (>! u-printer [['After 'relaxation qi qj] u]))
            output-process (output q m qi qj east west u)
            ]
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
    (go (loop [i 0
               grid []]
          (if (= i n)
            (>! out grid)
            (recur (inc i) (conj grid (get-row n in))))))
    out))

(defn simulate
  ;; m must be even!
  [q m steps initial-values]
  (let [line (fn [] (vec (repeatedly (inc q) chan))) ;; we only use elements 1 through q of line
        matrix (fn [] (vec (repeatedly (inc q) line)))
        h (matrix)
        v (matrix)
        initialize (initial (* q m) initial-values)]
    
    (go (>! p-printer (<! (master (* q m) ((h 0) q)))))
    
    (doseq [k (drop 1 (range (inc q)))]
      (node q m initialize steps k 1 ((v (dec k)) 1) ((v k) 1) ((h k) 1) ((h (dec k)) q)))
    
    (doseq [i (drop 1 (range (inc q)))
            j (drop 2 (range (inc q)))]
      (node q m initialize steps i j ((v (dec i)) j) ((v i) j) ((h i) j) ((h i) (dec j))))))
    
