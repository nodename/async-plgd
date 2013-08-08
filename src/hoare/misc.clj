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

(defn eratosthenes
  "Print in ascending order all primes less than n"
  [n]
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

;; Problem: A square matrix A of order n is given.
;; n streams are to be input, each stream representing
;; a column of an array IN.
;; n streams are to be output, each representing
;; a column of the product matrix IN x A.
;; After an initial delay, the results are to be produced
;; at the same rate as the input is consumed.
;; Consequently, a high degree of concurrency is required.

(defn constant-chan
  "A channel that constantly emits val"
  [val]
  (let [out (chan)]
    (go (while true
          (>! out val)))
    out))

(defn sink
  "Swallow output from channels"
  [channels]
  (go (while true
        (alts! channels))))

(defn vec-chan
  "A channel that always emits a vector of the most recent outputs of ins"
  [ins]
  (let [out (chan)]
  (go (loop [output (vec (take (count ins) (repeat nil)))]
        (>! out output)
        (let [[value source] (alts! ins)]
          (recur (assoc output (.indexOf ins source) value)))))
  out))

(defn vec-print
  "Print vec-chan of channels.
Note that this function runs in the main thread."
  [channels]
  (let [vec-out (vec-chan channels)
        printer-in (printer)]
    (while true
      (>!! printer-in (<!! vec-out)))))

;; A processor is assigned to each element in the input matrix.
;; Each of these nXn processors inputs a vector component
;; from the west and a partial sum from the north.
;; Each node outputs the vector component to its east,
;; and an updated partial sum to the south.

(defn processor [A north west]
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

(defn make-processor-node [A north west]
  (processor A (north :south) (west :east)))

(defn append-processor-node [processor-row A north]
  (let [processor-node (make-processor-node (first A) (first north) (last processor-row))]
    (conj processor-row processor-node)))

(defn make-processor-row [A north west]
  ;; A and north seqs; west a single channel
  (loop [row [west]
         A A
         north north]
    (if (zero? (count A))
      (rest row)
      (recur (append-processor-node row A north)
             (rest A)
             (rest north)))))

(defn append-processor-row [processor-matrix A west]
  (let [processor-row (make-processor-row (first A) (last processor-matrix) (first west))]
    (conj processor-matrix processor-row)))

(defn make-processor-matrix [A north west]
  ;; A a square matrix, north and west seqs
  (loop [matrix [north]
         A A
         west west]
    (if (zero? (count A))
      (rest matrix)
      (recur (append-processor-row matrix A west)
             (rest A)
             (rest west)))))

(defn matrix-multiplier [IN A]
  (let [;; The north border is a constant source of zeroes:
        north (repeatedly (count IN) (fn [] {:south (constant-chan 0)}))

        ;; The input data is produced by the west border nodes:
        west (map (fn [c] {:east c}) IN)

        processor-matrix (make-processor-matrix A north west)]

    ;; The east border is just a sink:
    (sink (map #(% :east) (map last processor-matrix)))

    ;; The desired results are consumed by south border nodes:
    (vec-print (map #(% :south) (last processor-matrix))))

  nil)

(defn test-multiplier
  "After a short delay of (how many?) outputs, repeatedly print [24 10 35 40 65]"
  []
  (matrix-multiplier
    (map constant-chan [3 2 1 0 5])
    [[1 2 3 4 5]
     [3 2 5 8 4]
     [10 0 1 2 2]
     [5 3 6 4 7]
     [1 0 3 2 8]]))



