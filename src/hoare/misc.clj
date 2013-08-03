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
           
      