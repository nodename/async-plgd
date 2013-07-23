(ns hoare.subroutines
  (:require [clojure.core.async :refer :all]))

;; 4.1 FUNCTION: DIVISION WITH REMAINDER

(defn divider []
  (let [c (chan)]
    (go
      (while true
        (let [{:keys [x y]} (<! c)]
          (if (= y 0)
            (>! c {:error :divbyzero})
            (loop [r x
                   q 0]
              (if (>= r y)
                (recur (- r y) (inc q))
                (>! c {:quotient q :remainder r})))))))
    c))

(defn test-divider []
  (let [divider (divider)]
    (go
      (dotimes [_ 20]
        (let [x (int (rand 100))
              y (int (rand 25))]
          (>! divider {:x x :y y})
          (let [{:keys [quotient remainder error]} (<! divider)]
            (println x "/" y ":" (if-not (nil? error)
                                   (str "Error " error)
                                   (str "quotient: " quotient " remainder: " remainder)))))))))

;; 4.2 RECURSION: FACTORIAL

(defn factorializer []
  "Compute a factorial by the recursive method.
...
Note: This unrealistic example introduces the technique of the 'iterative list of processes'
which will be used to a better effect in later examples."
  (let [c (chan)]
    (go
      (while true
        (let [n (<! c)]
          (if (> n 1)
            (let [child (factorializer)]
              (>! child (- n 1))
              (>! c (* n (<! child))))
            (>! c 1)))))
  c))

;; Here's your decorator pattern for a channel:
(defn named-fact [name]
  "Wrap factorializer in a transformer that produces a string about the factorial"
  (let [c (chan)
        fact (factorializer)]
    (go
      (while true
        (let [n (<! c)]
          (>! fact n)
          (>! c (str name " says: factorial of " n " is " (<! fact))))))
    c))
    
(defn run-factorializer [name]
  "Print out 20 factorials"
  (let [factorializer (named-fact name)]
    (go
      (dotimes [_ 20]
        (let [n (int (rand 20))]
          (>! factorializer n)
          (println (<! factorializer))))))
  nil)

(defn test-factorializer []
  "Print out twenty factorials for a and twenty for b concurrently"
  (run-factorializer 'a)
  (run-factorializer 'b))

;; 4.3 DATA REPRESENTATION: SMALL SET OF INTEGERS
;; 4.4 SCANNING A SET

(defn myset []
  "Implement a set as a process using only array-like methods"
  (let [c (chan)]
    (go
      (loop [content []]
        (let [[command n] (<! c)]
          (println command n)
          (condp = command
            ;; :has? n - reply true if n is in the set, false otherwise
            :has? (do
                   (loop [i 0]
                     (if (and (< i (count content)) (not= (nth content i) n))
                       (recur (inc i))
                       (>! c (< i (count content)))))
                   (recur content))
            ;; :insert n - insert n into the set
            :insert (let [found (loop [i 0]
                                  (when (and (< i (count content)) (not= (nth content i) n))
                                    (recur (inc i)))
                                  (< i (count content)))]
                      (recur (if found content (conj content n))))
            ;; :scan - reply :next with each member of the set in turn, and finally :noneleft
            :scan (do
                    (loop [i 0]
                      (if (< i (count content))
                        (do
                          (>! c [:next (nth content i)])
                          (recur (inc i)))
                        (>! c [:noneleft])))
                    (recur content))))))
    c))

;; rather than making this a function with its own go block
;; and trying to sync it with the caller,
;; I've made it a macro so its code will just
;; drop into the caller verbatim
(defmacro scan-set [f myset]
  "Invoke f on each element of the set"
  `(do
     (>! ~myset [:scan])
     (loop []
       (let [[response value] (<! ~myset)]
         (when (= response :next)
           (do
             (~f value)
             (recur)))))))

(defn test-set []
  (let [myset (myset)]
    (go
      (>! myset [:has? 4])
      (println (<! myset))
      (>! myset [:insert 4])
      (>! myset [:insert 4])
      (>! myset [:has? 4])
      (println (<! myset))
      (scan-set println myset)
      (>! myset [:insert 3])
      (>! myset [:has? 3])
      (println (<! myset))
      (scan-set println myset)))
  nil)

;; 4.5 RECURSIVE DATA REPRESENTATION: SMALL SET OF INTEGERS


            
                                     
