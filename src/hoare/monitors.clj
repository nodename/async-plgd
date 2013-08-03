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
;; would be, nor indeed does it use any knowledge of the identity of the requester.
;; We have strict alternation in the :P request and its :ok response,
;; and we avoid deadlock in the client by introducing a second level of asynchrony.

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
                ;; :P means a user is requesting access to a resource
                :P (if (= available 0)
                     (recur available (conj waiting user)
                            (inc requested) acquired released)
                     (do
                       (>! user :ok)
                       (recur (dec available) waiting
                              (inc requested) (inc acquired) released)))
                ;; :V means a user has released a resource
                :V (if (> (count waiting) 0)
                    (do
                      (>! (first waiting) :ok)
                      (recur available (rest waiting)
                             requested (inc acquired) (inc released)))
                    (recur (inc available) waiting
                           requested acquired (inc released))))))))

(defn test-semaphore []
  (let [users (repeatedly 100 chan)
        resources 10]
    (semaphore users resources)
    
    (doseq [user users]
      (go
        (>! user :P)
        ;; we may have to wait for an :ok for our :P request;
        ;; do it asynchronously by launching another goroutine:
        (go
          (<! user)
          (do '(some work that acquires, uses, and releases the resource))
          (>! user :V)))))
  
  nil)

;; 5.3 DIJKSTRA'S DINING PHILOSOPHERS

;; Five philosophers spend their lives thinking and eating.
;; The philosophers share a common dining room where
;; there is a circular table surrounded by five chairs,
;; each belonging to one philosopher.
;; In the center of the table there is a large bowl of spaghetti,
;; and the table is laid with five forks.
;; On feeling hungry, a philosopher enters the dining room, sits
;; in his own chair, and picks up the fork on the left of his place.
;; Unfortunately, the spaghetti is so tangled that he needs to
;; pick up and use the fork on his right as well.
;; When he has finished, he puts down both forks, and leaves the room.

(defn room [philosophers]
  (go (loop [occupants #{}]
        (println "in room:" (count occupants) "philosophers")
        (let [alts (cond
                     ;; do not allow all 5 philosophers in the room at once,
                     ;; to avoid the possibility that each one will
                     ;; pick up his left fork and starve to death
                     ;; because he cannot pick up his right fork:
                     (= (count occupants) 4) (vec occupants)
                     :else philosophers)
              [command philosopher] (alts! alts)]
          (condp = command
            :enter (recur (conj occupants philosopher))
            :exit (recur (disj occupants philosopher)))))))

(defn fork [left-philosopher right-philosopher]
  (go (loop [holder :none]
        (let [alts (condp = holder
                     :none [left-philosopher right-philosopher]
                     left-philosopher [left-philosopher]
                     right-philosopher [right-philosopher])
              [command philosopher] (alts! alts)]
          (condp = command
            :pickup (condp = holder
                      :none (recur philosopher)
                      (println "Fork error: already being held"))
            :putdown (condp = holder
                       philosopher (recur :none)
                       (println "Fork error: you can't put me down")))))))

(defn philosopher []
  (let [start (chan)
        room (chan)
        left-fork (chan)
        right-fork (chan)]
    (go
      (let [name (<! start)]
        (while true
          (<! (timeout (long (rand 6000)))) ; (THINK)
          (>! room :enter)
          (>! left-fork :pickup)
          (>! right-fork :pickup)
          (println name 'eating)
          (<! (timeout (long (rand 12000)))) ; (EAT)
          (println name 'done 'eating)
          (>! left-fork :putdown)
          (>! right-fork :putdown)
          (>! room :exit))))
    {:start start :room room :left-fork left-fork :right-fork right-fork}))

(defn dining-philosophers []
  (let [phils (vec (repeatedly 5 philosopher))
        phil0 (phils 0)
        phil1 (phils 1)
        phil2 (phils 2)
        phil3 (phils 3)
        phil4 (phils 4)]
    
    (fork (phil4 :right-fork) (phil0 :left-fork))
    (fork (phil0 :right-fork) (phil1 :left-fork))
    (fork (phil1 :right-fork) (phil2 :left-fork))
    (fork (phil2 :right-fork) (phil3 :left-fork))
    (fork (phil3 :right-fork) (phil4 :left-fork))
    
    (room (map :room phils))
    
    (>!! (phil0 :start) 0)
    (>!! (phil1 :start) 1)
    (>!! (phil2 :start) 2)
    (>!! (phil3 :start) 3)
    (>!! (phil4 :start) 4))
  
  nil)
    
                           
        
  
    