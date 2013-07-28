(ns hoare.monitors
  (:require [clojure.core.async :refer :all]))

;; 5.1 BOUNDED BUFFER

(defn bounded-buffer
  [producer consumer-request consumer bufsize]
  (go (loop [buf []]
        (let [alts (condp = (count buf)
                     ;; buffer full: accept only from consumer-request:
                     bufsize [consumer-request]
                     ;; buffer empty: accept only from producer:
                     0 [producer]
                     ;; otherwise accept from either:
                     [consumer-request producer])
              [value source] (alts! alts)]
          (condp = source
            consumer-request (do
                               (>! consumer (first buf))
                               (recur (vec (rest buf))))
            producer (recur (conj buf value))))))
                
    nil)

(defn test-buffer []
  (let [producer (chan)
        [consumer consumer-request] [(chan) (chan)]
        rand-interval #(long (rand 2000))]
    
    (bounded-buffer producer consumer-request consumer 10)
        
    (go (while true
          (let [interval (rand-interval)]
            (<! (timeout interval))
            (>! consumer-request :ready)
            (println (<! consumer)))))
    
    (go (dotimes [i 20]
          (let [interval (rand-interval)]
            (<! (timeout interval))
            (>! producer i))))
  
  nil))

;; 5.2 INTEGER SEMAPHORE

;; All of our early examples of services (before we introduced the fan-in function)
;; would have worked with single channels instead of in/out channel pairs,
;; because the service and client sent and received in strict alternation.

;; Here we find a case where using just a single channel is fully justified:
;; the semaphore has no way to determine what the corresponding response channel
;; would be. We have strict alternation, and we avoid deadlock in the client
;; by introducing a second level of asynchrony.

(defn semaphore [users resources]
  (go (loop [available resources
             waiting []
             ;; requested, acquired, and released are purely for reporting purposes;
             ;; they aren't used by the algorithm:
             requested 0
             acquired 0
             released 0]
        (println "available" available "waiting" (count waiting)
                 "requested" requested "acquired" acquired "released" released)
        (let [[command user] (alts! users)]
              (condp = command
                ;; :V means a user is releasing a resource
                :V (if (> (count waiting) 0)
                    (do
                      (>! (first waiting) :ok)
                      (recur available (rest waiting)
                             requested (inc acquired) (inc released)))
                    (do
                      (recur (inc available) waiting
                             requested acquired (inc released))))
                ;; :P means a user is requesting a resource
                :P (if (= available 0)
                     (do
                       (recur available (conj waiting user)
                              (inc requested) acquired released))
                     (do
                       (>! user :ok)
                       (recur (dec available) waiting
                              (inc requested) (inc acquired) released))))))))

(defn test-semaphore []
  (let [users (repeatedly 100 chan)
        resources 10]
    (semaphore users resources)
    
    (doseq [channel users]
      (go
        (>! channel :P)
        ;; we may have to wait for an :ok for our :P request;
        ;; do it asynchronously by launching another goroutine:
        (go
          (<! channel)
          (>! channel :V)))))
  
  nil)
    