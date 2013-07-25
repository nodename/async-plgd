(ns hoare.subroutines
  (:require [clojure.core.async :refer :all]))

;; 4.1 FUNCTION: DIVISION WITH REMAINDER

(defn divider []
  (let [in (chan)
        out (chan)]
    (go
      (while true
        (let [{:keys [x y]} (<! in)]
          (if (= y 0)
            (>! out {:error :divbyzero})
            (loop [r x
                   q 0]
              (if (>= r y)
                (recur (- r y) (inc q))
                (>! out {:quotient q :remainder r})))))))
    [in out]))

(defn test-divider []
  (let [[d-in d-out] (divider)]
    (go
      (dotimes [_ 20]
        (let [x (int (rand 100))
              y (int (rand 25))]
          (>! d-in {:x x :y y})
          (let [{:keys [quotient remainder error]} (<! d-out)]
            (println x "/" y ":" (if-not (nil? error)
                                   (str "Error " error)
                                   (str "quotient: " quotient " remainder: " remainder))))))))
  nil)

;; 4.2 RECURSION: FACTORIAL

(defn factorializer []
  "Compute a factorial by the recursive method.
...
Note: This unrealistic example introduces the technique of the 'iterative list of processes'
which will be used to a better effect in later examples."
  (let [in (chan)
        out (chan)]
    (go
      (while true
        (let [n (<! in)]
          (if (> n 1)
            (let [[child-in child-out] (factorializer)]
              (>! child-in (- n 1))
              (>! out (* n (<! child-out))))
            (>! out 1)))))
  [in out]))

(defn named-fact [name]
  "Wrap factorializer in a transformer that produces a string about the factorial"
  (let [in (chan)
        out (chan)
        [f-in f-out] (factorializer)]
    (go
      (while true
        (let [n (<! in)]
          (>! f-in n)
          (>! out (str name " says: factorial of " n " is " (<! f-out))))))
    [in out]))
    
(defn run-factorializer [name]
  "Print out 10 factorials"
  (let [[f-in f-out] (named-fact name)]
    (go
      (dotimes [i 10]
        (let [n (int (rand 20))]
          (>! f-in n)
          (println i (<! f-out))))))
  nil)

(defn test-factorializer []
  "Print out ten factorials for a and ten for b concurrently"
  (run-factorializer 'a)
  (run-factorializer 'b))

;; Well, that works, but you may have seen a and b stepping on each other a bit at the terminal.

(defn fan-in
  "Merge input channels into a single channel.
Whenever either input has something ready, send it on."
  ([ins] (fan-in (chan) ins))
  ([c ins]
    (go (while true
          (let [[x] (alts! ins)]
            (>! c x))))
    c))

(defn test-merged-factorial []
  (let [[a-in a-out] (named-fact 'a)
        [b-in b-out] (named-fact 'b)
        c (fan-in [a-out b-out])]
    
    (go (while true
          (println (<! c))))
    
    (go (dotimes [i 10]
          (>! a-in i)))
    
    (go (dotimes [i 10]
          (>! b-in i))))
  
  nil)

;; 4.3 DATA REPRESENTATION: SMALL SET OF INTEGERS
;; 4.4 SCANNING A SET

(defmacro found? [n content]
  `(loop [i# 0]
     (when (and (< i# (count ~content)) (not= (nth ~content i#) ~n))
       (recur (inc i#)))
     (< i# (count ~content))))

(defn myset []
  "Implement a set as a process using only array-like methods"
  (let [in (chan)
        out (chan)]
    (go
      (loop [content []]
        (let [[command n] (<! in)]
          (println command n)
          (condp = command
            ;; :has? n - reply true if n is in the set, false otherwise
            :has? (do
                    (>! out (found? n content))
                    (recur content))
            ;; :insert n - insert n into the set
            :insert (recur (if (found? n content)
                             content
                             (conj content n)))
            ;; :scan - reply :next with each member of the set in turn, and finally :noneleft
            :scan (do
                    (loop [i 0]
                      (if (< i (count content))
                        (do
                          (>! out [:next (nth content i)])
                          (recur (inc i)))
                        (>! out [:noneleft])))
                    (recur content))))))
    [in out]))

(defmacro scan-set [f set-in set-out]
  "Invoke f on each element of the set"
  `(do
     (>! ~set-in [:scan])
     (loop []
       (let [[response value] (<! ~set-out)]
         (when (= response :next)
           (do
             (~f value)
             (recur)))))))

(defn test-set []
  (let [[set-in set-out :as myset] (myset)]
    (go
      (>! set-in [:has? 4])
      (println (<! set-out))
      (>! set-in [:insert 4])
      (>! set-in [:insert 4])
      (>! set-in [:has? 4])
      (println (<! set-out))
      (scan-set println set-in set-out)
      (>! set-in [:insert 3])
      (>! set-in [:has? 3])
      (println (<! set-out))
      (scan-set println set-in set-out)))
  nil)

;; 4.5 RECURSIVE DATA REPRESENTATION: SMALL SET OF INTEGERS


            
                                     
