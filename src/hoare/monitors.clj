(ns hoare.monitors
  (:require [clojure.core.async :refer :all]))

;; 5.1 BOUNDED BUFFER

(defn bounded-buffer
  [producer consumer-request consumer-in bufsize]
  (go (loop [buf []]
        (let [alts (condp = (count buf)
                     bufsize [consumer-request]
                     0 [producer]
                     [consumer-request producer])
              [value source] (alts! alts)]
          (condp = source
            consumer-request (do
                               (>! consumer-in (first buf))
                               (recur (vec (rest buf))))
            producer (recur (conj buf value))))))
                
    nil)

(defn test-buffer []
  (let [producer (chan)
        [consumer-in consumer-request] [(chan) (chan)]
        make-timeout (fn [] (timeout (.nextInt (java.util.Random.) 10000)))]
      ;  timeout (make-timeout)]
    
    (bounded-buffer producer consumer-request consumer-in 10)
        
    (go (while true
        ;  (<! timeout)
          (>! consumer-request :ready)
          (println (<! consumer-in))))
    
    (go (dotimes [i 20]
        ;  (<! timeout)
          (>! producer i)))
  
  nil))
    