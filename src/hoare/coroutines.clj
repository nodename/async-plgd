(ns hoare.coroutines
  (:require [clojure.core.async :refer :all]))

;; Examples corresponding to those in section 3 of C.A.R. Hoare, "Communicating Sequential Processes" (CACM 21:8 August 1978),
;; the precursor to his book of the same title, which can be obtained at http://www.usingcsp.com/

(defmacro go-times [bindings & body]
  `(go (dotimes ~bindings
         ~@body)))

(defmacro go-loop [& body]
  `(go (while true
         ~@body)))

;; 3.1 COPY

(defn copy [west east]
  "Copy from channel west into channel east"
  (go-loop
    (>! east (<! west))))

(defn test-copy []
  (let [east (chan)
        west (chan)]
    (copy west east)
    
    (go-times [i 100]
              (>! west i))
    
    (go-loop
      (println (<! east))))
  
  nil)

;; This function demonstrates that it is possible to call close! inside a go block        
(defn foo []
  (let [c (chan)]
    (go (close! c))))

;; In this function, I get an exception "Can't put nil on channel"
(defn test-copy-and-close []
  (let [east (chan)
        west (chan)]
    (copy west east)
    
    (go
      (dotimes [i 10]
        (>! west i))
      (close! west))
    
    (go-loop
      (println (<! east))))
  
  nil)

;; 3.2 SQUASH

(defn squash [west east]
  "Copy from channel west into channel east but replace every pair of consecutive asterisks '**' by an upward arrow '^'.
Assume that the final character input is not an asterisk."
  (go-loop
    (let [value (<! west)]
      (if (not= value \*)
        (>! east value)
        (let [value (<! west)]
          (if (not= value \*)
            (do
              (>! east \*)
              (>! east value))
            (>! east \^)))))))

;; close! is not of much use since you can't call it within a go block or a thread.
;; So you need a control channel to signal end of input.
(defn squash-2
  "Copy from channel west into channel east but replace every pair of consecutive asterisks '**' by an upward arrow '^'.
Deal sensibly with input which ends with an odd number of asterisks."
  ([west east]
    (squash-2 west east nil))
  ([west east east-control]
    (let [control (chan)]
      (go-loop
        (let [value (<! west)]
          (if (not= value \*)
            (>! east value)
            (let [[value source] (alts! [west control])]
              (cond
                (= source control) (do
                                     (>! east \*)
                                     (when-not (nil? east-control)
                                       (>! east-control value)))
                (not= value \*) (do
                                  (>! east \*)
                                  (>! east value))
                :else (>! east \^) )))))
      ;; Return the control channel:
      control)))

(defn chars>!
  "Send all the chars of string to channel, then (if control is specified) signal completion by sending :exit to control"
  ([channel string]
    (chars>! channel string nil))
  ([channel string control]
    (go
      (dotimes [i (.length string)]
        (>! channel (.charAt string i)))
      (when-not (nil? control)
        (>! control :exit)))))

(defn print-chars<! [channel]
  "Dump everything from channel to stdout"
  (go-loop
    (.print System/out (<! channel))))
      ; This is different from (print (<! channel)), which doesn't flush everything until the user hits return!

(defn test-squasher [squasher string]
  (let [east (chan)
        west (chan)
        control (squasher west east)]
    
    (chars>! west string control)
    
    (print-chars<! east)
    
    nil))

(defn test-squash []
  (test-squasher squash "here****there*p"))

(defn test-squash-2 []
  (test-squasher squash-2 "here****there*p***"))

;; 3.3 DISASSEMBLE

(defn disassemble [cardfile X]
  "Read cards from cardfile and output to X the stream of characters they contain.
An extra space should be inserted at the end of each card."
  (go-loop
    (let [cardimage (<! cardfile)]
      (dotimes [i 80]
        (>! X (.charAt cardimage i)))
      (>! X \space))))

(defn test-disassemble []
  (let [cardfile (chan)
        X (chan)]
    (disassemble cardfile X)
    
    (let [cardimage "01234567891123456789212345678931234567894123456789512345678961234567897123456789"]
      (go-times [i 10]
        (>! cardfile cardimage)))
    
    (print-chars<! X)
    
    nil))
    
;; 3.4 ASSEMBLE

(defn assemble
  "Read a stream of characters from X and print them in lines of 125 characters on lineprinter.
The last line should be completed with spaces (or pad-char, if specified) if necessary."
  ([X lineprinter]
    (assemble X lineprinter \space))
  ([X lineprinter pad-char]
    (let [control (chan)]
      (go-loop
        (loop [lineimage ""]
          (let [i (.length lineimage)
                [char source] (alts! [X control])]
         ;   (println "ass: got " char)
            (cond
              (= source control) (do
                                ;   (println "ass: sending " (apply str lineimage (repeat (- 125 i) pad-char)))
                                   (>! lineprinter (apply str lineimage (repeat (- 125 i) pad-char)))
                                ;   (println "ass: sending \n")
                                   (>! lineprinter \newline))
              (< i 125) (recur (str lineimage char))
              :else (do
                  ;    (println "ass: sending " lineimage)
                      (>! lineprinter lineimage)
                   ;   (println "ass: sending \n")
                      (>! lineprinter \newline)
                      (recur ""))))))
      control)))

(defn test-assemble []
  (let [X (chan)
        lineprinter (chan)
        control (assemble X lineprinter \X)]
    (let [cardimage "01234567891123456789212345678931234567894123456789512345678961234567897123456789"]
      (chars>! X (apply str (repeat 10 cardimage)) control))
    (print-chars<! lineprinter)
    nil))
    
;; 3.5 REFORMAT

(defn reformat
  "Read a sequence of cards of 80 characters each, and print the characters on a lineprinter at 125 characters per line.
Every card should be followed by an extra space, and the last line should be completed with spaces if necessary.
...
This elementary problem is difficult to solve elegantly without coroutines."
  ([cardfile lineprinter]
    (reformat cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)
          _ (disassemble cardfile X)
          control (assemble X lineprinter pad-char)]
      control)))

(defn reformat-2
  "Same as reformat but with a copy coroutine in the middle"
   ([cardfile lineprinter]
    (reformat-2 cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)
          Y (chan)
          _ (disassemble cardfile X)
          _ (copy X Y)
          control (assemble Y lineprinter pad-char)]
      control))) 

(defn test-reformatter [reformatter]
  (let [cardfile (chan)
        lineprinter (chan)
        control (reformatter cardfile lineprinter \X)]
    
    (let [cardimage "1234567891123456789212345678931234567894123456789512345678961234567897123456789"]
      (go
        (dotimes [i 10]
          (>! cardfile (str i cardimage)))
        (>! control :exit)))
    
    (print-chars<! lineprinter)
    
    nil))

(defn test-reformat []
  (test-reformatter reformat))

(defn test-reformat-2 []
  (test-reformatter reformat-2))

;; 3.6 CONWAY'S PROBLEM

(defn conway
  "Adapt the reformat program to replace every pair of consecutive asterisks by an upward arrow."
   ([cardfile lineprinter]
    (conway cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)
          Y (chan)
          _ (disassemble cardfile X)
          _ (squash X Y)
          control (assemble Y lineprinter pad-char)]
      control))) 

(defn test-conway []
  (let [cardfile (chan)
        lineprinter (chan)
        control (conway cardfile lineprinter \X)]
    
    (let [cardimage "01234567891123456789212345678931234567894123456789512345678961234567897123456789"]
      (go
          (>! cardfile cardimage)
          (>! control :exit)))
    
    (print-chars<! lineprinter)
    
    nil))
    
