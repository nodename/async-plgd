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

;; The first common pattern of the Go language is
;; GENERATOR: a function that returns a channel.
;; The channel is running the go block defined in the function;
;; thus the channel is a handle on a service.
;; We use this pattern throughout the examples.

(defn copier [source]
  "A process that copies values from the source channel"
  (let [c (chan)]
    (go
      (loop []
        (let [value (<! source)]
          ;; this value will be nil if close! has been called on the channel.
          ;; we cannot copy nil to c because explicitly putting a nil is not allowed.
          (if (nil? value)
            (close! c)
            (do
              (>! c value)
              (recur))))))
    c))

(defn test-copy []
  "Print out all the numbers from 0 to 9,
then after two seconds print out the numbers from 10 to 19"
  (let [west (chan)
        ;; this process will remain ready to copy...
        east (copier west)
        ;; a channel that will close after 2000 ms:
        timeout (timeout 2000)]
    
    (go
      (dotimes [i 10]
        (>! west i))
      ;; the only value that will come from the timeout is the nil when it closes:
      (<! timeout)
      (dotimes [i 10]
        (>! west (+ 10 i))))
    
    ;; this process will remain ready to print...
    (go
      (loop []
        (println (<! east))
        (recur))))
  
  ;; until all the processes go away when they go out of scope:
  nil)

(defn test-copy-and-close []
  "Print out all the numbers from 0 to 9,
then fail to print out the numbers from 10 to 19"
  (let [west (chan)
        east (copier west)]
    (go
      (dotimes [i 10]
        (>! west i))
      (close! west)
      (dotimes [i 10]
        (>! west (+ 10 i))))
    
    ;; since west has closed, copier will close east;
    ;; once east has closed, we will forever get nils from it;
    ;; we never see the values put to west after west has closed;
    ;; they never even made it onto west
    (go-times [i 20]
      (println (<! east))))
  
  nil)

;; 3.2 SQUASH

(defn squasher [source]
  "Copy from source channel but replace every pair of consecutive asterisks '**' by an upward arrow '^'.
Deal sensibly with input which ends with an odd number of asterisks."
  (let [c (chan)]
      (go
        (loop []
          (let [value (<! source)]
            (cond
              (nil? value) (close! c)
              (not= value \*) (do
                                (>! c value)
                                (recur))
              :else (let [value (<! source)]
                      (cond
                        (nil? value) (do
                                       (>! c \*)
                                       (close! c))
                        (not= value \*) (do
                                          (>! c \*)
                                          (>! c value)
                                          (recur))
                        :else (do
                                (>! c \^)
                                (recur))))))))
      c))

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

(defn test-squasher-with
  [string]
    (let [west (chan)
          east (squasher west)]
      (chars>! west string :close)
      
      (print-chars<! east))
    
    nil)

(defn test-squash []
  "Print out 'here^^there*p^*'"
  (test-squasher-with "here****there*p***"))

;; 3.3 DISASSEMBLE

(def cardimage-without-first-char "1234567891123456789212345678931234567894123456789512345678961234567897123456789")
(defn cardimage [digit]
  (str digit cardimage-without-first-char))

(defn disassembler [cardfile]
  "Read cards from cardfile and output the stream of characters they contain.
An extra space should be inserted at the end of each card."
  (let [c (chan)]
    (go
      (loop []
        (let [cardimage (<! cardfile)]
          (if (nil? cardimage)
            (close! c)
            (do
              (dotimes [i 80]
                (>! c (.charAt cardimage i)))
              (>! c \space)
              (recur))))))
  c))

(defn test-disassemble []
  "Dump cardimages 0 through 9 to stdout with a space after each"
  (let [cardfile (chan)
        disassembler (disassembler cardfile)]
    (go-times [i 10]
              (>! cardfile (cardimage i)))
    
    (print-chars<! disassembler))
  
  nil)
    
;; 3.4 ASSEMBLE

(defn assembler
  "Read a stream of characters from source channel and print them in lines of 125 characters, as on a lineprinter.
The last line should be completed with spaces (or pad-char, if specified) if necessary."
  ([source]
    (assembler \space source))
  ([pad-char source]
    (let [lineprinter (chan)]
      (go
        (loop []
          (loop [lineimage ""]
            (let [i (.length lineimage)
                  char (<! source)]
              (cond
                (nil? char) (do
                              (>! lineprinter (apply str lineimage (repeat (- 125 i) pad-char)))
                              (>! lineprinter \newline)
                              (close! lineprinter))
                (< i 125) (recur (str lineimage char))
                :else (do
                        (>! lineprinter lineimage)
                        (>! lineprinter \newline)
                        (recur "")))))))
      lineprinter)))

(defn test-assemble []
  "Dump cardimages 0 through 9 to stdout in lines of 125 characters.
Pad the last line with Xs to 125 characters."
  (let [cardfile (chan)
        lineprinter (assembler \X cardfile)]
    (chars>! cardfile (apply str (for [i (range 10)] (cardimage i))) :close)
    (print-chars<! lineprinter))
  nil)
    
;; 3.5 REFORMAT

;; That GENERATOR pattern totally simplifies the composition of processes!
;; We just use a threading macro!

(defn reformatter
  "Read a sequence of cards of 80 characters each, and print the characters on a lineprinter at 125 characters per line.
Every card should be followed by an extra space, and the last line should be completed with spaces if necessary.
...
This elementary problem is difficult to solve elegantly without coroutines."
  ([cardfile]
    (reformatter \space cardfile))
  ([pad-char cardfile]
    (->> cardfile
      disassembler
      (assembler pad-char))))

(defn reformatter-2
  "Same as reformatter but with a copier coroutine in the middle"
   ([cardfile]
    (reformatter-2 \space cardfile))
  ([pad-char cardfile]
    (->> cardfile
      disassembler
      copier
      (assembler pad-char))))

(defn test-reformatter [reformatter]
  (let [cardfile (chan)
        reformatter (reformatter \X cardfile)]
    
    (go
      (dotimes [i 10]
        (>! cardfile (cardimage i)))
      (close! cardfile))
    
    (print-chars<! reformatter))
  
  nil)

(defn test-reformat []
  (test-reformatter reformatter))

(defn test-reformat-2 []
  (test-reformatter reformatter-2))

;; 3.6 CONWAY'S PROBLEM
;; Not that Conway!

(defn conway
  "Adapt the reformatter to replace every pair of consecutive asterisks by an upward arrow."
   ([cardfile]
    (conway \space cardfile))
  ([pad-char cardfile]
    (->> cardfile
      disassembler
      squasher
      (assembler pad-char))))

(defn cardimage-with-stars [i]
  "Replace one random character in cardimage i with a pair of asterisks"
  (let [cardimage (cardimage i)
        index (int (+ 1 (rand 79)))
        front (.substring cardimage 0 (- index 1))
        back (.substring cardimage index)]
    (str front "**" back)))

(defn test-conway []
  (let [cardfile (chan)
        conway (conway \X cardfile)]
    
    (go
      (dotimes [i 10]
        (>! cardfile (cardimage-with-stars i)))
      (close! cardfile))
    
    (print-chars<! conway))
  
  nil)
