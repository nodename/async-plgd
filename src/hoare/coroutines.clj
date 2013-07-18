(ns hoare.coroutines
  (:require [clojure.core.async :refer :all]))

;; Examples corresponding to those in section 3 of C.A.R. Hoare,
;; "Communicating Sequential Processes" (CACM 21:8 August 1978).
;; This paper is the precursor to his book of the same title,
;; which can be obtained at http://www.usingcsp.com/

(defmacro go-times [bindings & body]
  `(go (dotimes ~bindings
         ~@body)))

;; 3.1 COPY

(defn copy [west east]
  "Copy from channel west into channel east"
  (go
    (loop []
      (let [value (<! west)]
        ;; this value will be nil if close! has been called on the channel.
        ;; we cannot copy nil to east because explicitly putting a nil is not allowed.
        (if (nil? value)
          (close! east)
          (do
            (>! east value)
            (recur)))))))
          
(defn test-copy []
  "Print out all the numbers from 0 to 9,
then two seconds later print out the numbers from 10 to 19"
  (let [east (chan)
        west (chan)
        ;; a channel that will close after 2000 ms:
        timeout (timeout 2000)]
    
    ;; this process will remain ready to copy
    (copy west east)
    
    (go
      (dotimes [i 10]
        (>! west i))
      (<! timeout)
      (dotimes [i 10]
        (>! west (+ 10 i))))
    
    ;; this process will remain ready to print
    (go
      (loop []
        (println (<! east))
        (recur))))
  
  nil)

(defn test-copy-and-close []
  "Print out all the numbers from 0 to 9,
then fail to print out the numbers from 10 to 19"
  (let [east (chan)
        west (chan)]
    (copy west east)
    
    (go
      (dotimes [i 10]
        (>! west i))
      (close! west)
      (dotimes [i 10]
        (>! west (+ 10 i))))
    
    ;; since west has closed, copy will close east;
    ;; once east has closed, we will forever get nils from it;
    ;; we never see the values put to west after west has closed;
    ;; they never even made it onto west
    (go-times [i 20]
      (println (<! east))))
  
  nil)

;; 3.2 SQUASH

(defn squash
  "Copy from channel west into channel east but replace every pair of consecutive asterisks '**' by an upward arrow '^'.
Deal sensibly with input which ends with an odd number of asterisks."
  [west east]
      (go
        (loop []
          (let [value (<! west)]
            (cond
              (nil? value) (close! east)
              (not= value \*) (do
                                (>! east value)
                                (recur))
              :else (let [value (<! west)]
                      (cond
                        (nil? value) (do
                                       (>! east \*)
                                       (close! east))
                        (not= value \*) (do
                                          (>! east \*)
                                          (>! east value)
                                          (recur))
                        :else (do
                                (>! east \^)
                                (recur)))))))))

(defn chars>!
  "Send all the chars of string to channel, then (if close is specified) signal completion by closing channel"
  ([channel string]
    (chars>! channel string nil))
  ([channel string close]
    (go
      (dotimes [i (.length string)]
        (>! channel (.charAt string i)))
      (when-not (nil? close)
        (close! channel)))))

(defn print-chars<! [channel]
  "Dump everything from channel to stdout until channel closes, then print a newline"
  (go
    (loop []
      (let [value (<! channel)]
        (if (nil? value)
          (println "")
          (do
            (.print System/out value)
            ; This is apparently different from (print value), which doesn't flush everything until the user hits return!
            (recur)))))))

(defn test-squasher
  [string]
    (let [east (chan)
          west (chan)]
      (squash west east)
      
      (chars>! west string :close)
      
      (print-chars<! east))
    
    nil)

(defn test-squash []
  "Print out 'here^^there*p^*'"
  (test-squasher "here****there*p***"))

;; 3.3 DISASSEMBLE

(def cardimage-without-first-char "1234567891123456789212345678931234567894123456789512345678961234567897123456789")
(defn cardimage [digit]
  (str digit cardimage-without-first-char))

(defn disassemble [cardfile X]
  "Read cards from cardfile and output to X the stream of characters they contain.
An extra space should be inserted at the end of each card."
  (go
    (loop []
      (let [cardimage (<! cardfile)]
        (if (nil? cardimage)
          (close! X)
          (do
            (dotimes [i 80]
              (>! X (.charAt cardimage i)))
            (>! X \space)
            (recur)))))))

(defn test-disassemble []
  "Dump cardimages 0 through 9 to stdout with a space after each"
  (let [cardfile (chan)
        X (chan)]
    (disassemble cardfile X)
    
    (go-times [i 10]
              (>! cardfile (cardimage i)))
    
    (print-chars<! X))
  
  nil)
    
;; 3.4 ASSEMBLE

(defn assemble
  "Read a stream of characters from X and print them in lines of 125 characters on lineprinter.
The last line should be completed with spaces (or pad-char, if specified) if necessary."
  ([X lineprinter]
    (assemble X lineprinter \space))
  ([X lineprinter pad-char]
    (go
      (loop []
        (loop [lineimage ""]
          (let [i (.length lineimage)
                char (<! X)]
            (cond
              (nil? char) (do
                            (>! lineprinter (apply str lineimage (repeat (- 125 i) pad-char)))
                            (>! lineprinter \newline)
                            (close! lineprinter))
              (< i 125) (recur (str lineimage char))
              :else (do
                      (>! lineprinter lineimage)
                      (>! lineprinter \newline)
                      (recur "")))))))))

(defn test-assemble []
  "Dump cardimages 0 through 9 to stdout in lines of 125 characters.
Pad the last line with Xs to 125 characters."
  (let [X (chan)
        lineprinter (chan)]
    (assemble X lineprinter \X)
    (chars>! X (apply str (for [i (range 10)] (cardimage i))) :close)
    (print-chars<! lineprinter))
  nil)
    
;; 3.5 REFORMAT

(defn reformat
  "Read a sequence of cards of 80 characters each, and print the characters on a lineprinter at 125 characters per line.
Every card should be followed by an extra space, and the last line should be completed with spaces if necessary.
...
This elementary problem is difficult to solve elegantly without coroutines."
  ([cardfile lineprinter]
    (reformat cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)]
      (disassemble cardfile X)
      (assemble X lineprinter pad-char))))

(defn reformat-2
  "Same as reformat but with a copy coroutine in the middle"
   ([cardfile lineprinter]
    (reformat-2 cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)
          Y (chan)]
      (disassemble cardfile X)
      (copy X Y)
      (assemble Y lineprinter pad-char))))

(defn test-reformatter [reformatter]
  (let [cardfile (chan)
        lineprinter (chan)]
    (reformatter cardfile lineprinter \X)
    
    (go
      (dotimes [i 10]
        (>! cardfile (cardimage i)))
      (close! cardfile))
    
    (print-chars<! lineprinter))
  
  nil)

(defn test-reformat []
  (test-reformatter reformat))

(defn test-reformat-2 []
  (test-reformatter reformat-2))

;; 3.6 CONWAY'S PROBLEM
;; Not that Conway!

(defn conway
  "Adapt the reformat program to replace every pair of consecutive asterisks by an upward arrow."
   ([cardfile lineprinter]
    (conway cardfile lineprinter \space))
  ([cardfile lineprinter pad-char]
    (let [X (chan)
          Y (chan)]
      (disassemble cardfile X)
      (squash X Y)
      (assemble Y lineprinter pad-char)))
  nil)

(defn cardimage-with-stars [i]
  "Replace one random character in cardimage i with a pair of asterisks"
  (let [cardimage (cardimage i)
        index (int (+ 1 (rand 79)))
        front (.substring cardimage 0 (- index 1))
        back (.substring cardimage index)]
    (str front "**" back)))

(defn test-conway []
  (let [cardfile (chan)
        lineprinter (chan)]
    (conway cardfile lineprinter \X)
    
    (go
      (dotimes [i 10]
        (>! cardfile (cardimage-with-stars i)))
      (close! cardfile))
    
    (print-chars<! lineprinter)))
