(ns utils.helpers
  (:require [clojure.core.async :refer [<! go]]
             [fipp.edn :refer [pprint]]))

(defn pprinter []
  (let [in (chan)]
    (go (while true
          (let [value (<! in)]
            (if (nil? value)
              (println "")
              (do
                (pprint value))))))
    in))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))

(defn print-all<!
  "Dump everything from channel to stdout until channel closes, then print a newline"
  [channel]
  (go
    (while true
      (let [value (<! channel)]
        (if (nil? value)
          (println "")
          (do
            (.print System/out (str value \newline \newline)) ; This is apparently different from (print value), which doesn't flush everything until the user hits return!
            (println "")))))))