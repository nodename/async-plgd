(ns utils.helpers
  (:require [clojure.core.async :refer :all]
            [clojure.pprint :refer [pprint]]))

(defn pprinter []
  (let [in (chan)]
    (go (while true
          (pprint (<! in))))
    in))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))