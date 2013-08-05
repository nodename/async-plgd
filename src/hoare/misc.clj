(ns hoare.misc
  (:require [clojure.core.async :refer :all]))

;; 6.1 Prime Numbers: The Sieve of Eratosthenes

(defn sieve [printer]
  (let [in (chan)]
    (go
      ;; input a prime from my predecessor and print it:
      (let [prime (<! in)]
        (>! printer prime)
        (when (not= prime :done)
          (let [successor (sieve printer)]
            ;; input an ascending stream of numbers from my predecessor
            ;; and pass them on to my successor,
            ;; suppressing any that are multiples of my own prime:
            (loop [multiple prime]
              (let [m (<! in)]
                (when (not= m :done)
                  (let [multiple (if (> m multiple)
                                   (+ multiple prime)
                                   multiple)]
                    (when (< m multiple)
                      (>! successor m))
                    (recur multiple)))))))))
  in))

(defn printer []
  (let [in (chan)]
    (go (while true
          (println (<! in))))
    in))

(defn eratosthenes [n]
  "Print in ascending order all primes less than n"
  (let [printer (printer)
        sieve (sieve printer)]
    (>!! printer 2)
    (loop [m 3]
      (when (< m n)
        (do
          (>!! sieve m)
          (recur (+ m 2)))))
    (>!! sieve :done))
  nil)

;; 6.2 AN ITERATIVE ARRAY: MATRIX MULTIPLICATION

(defn constant-chan [val]
  (let [out (chan)]
    (go (while true
          (>! out val)))
    out))

(defn sink [channels]
  (go (while true
        (alts! channels))))

(defn center [A north west]
  (let [[south east] (repeatedly 2 chan)]
    (go (loop [x 0
               sum 0]
          (let [[value source] (alts! [north west])]
            (condp = source
              north (do
                      (>! south (+ (* A x) value))
                      (recur x value))
              west (do
                     (>! east value)
                     (recur value sum))))))
    {:south south :east east}))

(defn vec-printer [channels]
  (go (loop [output (vec (take (count channels) (repeat nil)))]
        (println output)
     ;   (<! (timeout 1000))
        (let [[value source] (alts! channels)]
          (recur (assoc output (.indexOf channels source) value))))))

(defn make-north []
  {:south (constant-chan 0)})

(defn make-west [c]
  {:east c})

(defn make-process-node [A north west]
  (center A (north :south) (west :east)))

(defn add-process [row A north]
  (let [current-node (make-process-node (first A) (first north) (last row))]
    (conj row current-node)))

(defn make-process-row [A north west]
  "A and north seqs; west a single channel"
  (loop [row [west]
         A A
         north north]
    (if (= (count A) 0)
      (rest row)
      (recur (add-process row A north)
             (rest A)
             (rest north)))))

(defn add-row [matrix A west]
  (let [current-row (make-process-row (first A) (last matrix) (first west))]
    (conj matrix current-row)))

(defn make-process-matrix [A north west]
  "A a square matrix, north and west seqs"
  (loop [matrix [north]
         A A
         west west]
    (if (= (count A) 0)
      (rest matrix)
      (recur (add-row matrix A west)
             (rest A)
             (rest west)))))

(defn multiplier [IN A]
  (let [west (map make-west IN)
        north (repeatedly (count IN) make-north)
        process-matrix (make-process-matrix A north west)]
    
    (sink (map #(% :east) (map last process-matrix)))
    (vec-printer (map #(% :south) (last process-matrix))))
  
  nil)
    
(defn test-multiplier []
  (multiplier [(constant-chan 3) (constant-chan 2) (constant-chan 1)] [[1 2 3] [3 2 5] [10 0 1]]))
                                          
           
      