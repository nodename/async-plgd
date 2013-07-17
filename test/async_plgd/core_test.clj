(ns async-plgd.core-test
  (:require [clojure.test :refer :all]
            [async-plgd.core :refer :all])
  (:use clojure.test
        async-plgd.core))

(deftest copy-test
  (testing "copy"
    (let [east (chan)
          west (chan)]
      
