(ns hoare.problem
  (:require [clojure.core.async :refer :all]))

(defn fan-in
  ([ins] (fan-in (chan) ins))
  ([c ins]
    (go (while true
          (let [[x] (alts! ins)]
            (>! c x))))
    c))

(defn service []
  (let [c (chan)]
    (go (while true
          (let [val (<! c)]
            (>! c (str val " done")))))
    c))

;; This function prints ten outputs from the service, as expected:
(defn test-service []
  (let [c (service)]
    (go (dotimes [i 10]
          (>! c i)
          (println (<! c)))))
  nil)

;; On the console I see messages coming from c
;; that have not been modified by the service;
;; furthermore they're coming out of order,
;; such as "b 9" before "b 8 done".
;; Don't know what's GO ing on here.
(defn test-fan-in []
  (let [a (service)
        b (service)
        c (fan-in [a b])]
    
    (go (while true
          (println "c:" (<! c))))
    
    (go (dotimes [i 10]
          (>! a (str "a " i))))
    
    (go (dotimes [i 10]
          (>! b (str "b " i)))))
  
  nil)

;; This version has the same problems as test-fan-in,
;; and additionally doesn't even print out twenty lines!
(defn test-fan-in-2 []
  (let [a (service)
        b (service)
        c (fan-in [a b])]
    
    (go (dotimes [i 10]
          (>! a (str "a " i))
          (println "c: " (<! c))))
    
    (go (dotimes [i 10]
          (>! b (str "b " i)))
          (println "c: " (<! c))))
  
  nil)

