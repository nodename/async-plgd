(ns cellular.forestfire-test
  (:require [clojure.test :refer :all]
            [cellular.forestfire :refer :all]
            [clojure.core.async :refer :all])
  (:use clojure.test
        cellular.forestfire))

(defn get-positions
  "Find all positions in grid that have one of these values"
  [grid & values]
  (for [i (range (count grid))
        j (range (count grid)) ;; assume grid is square
        :when ((set values) (get-in grid [i j]))]
    [i j]))

(defn test-positions
  "Does every one of these positions in grid have one of these values?"
  [grid positions & values]
  (let [test (fn [[i j]] ((set values) (get-in grid [i j])))]
    (every? test positions)))

(defn all-burning-trees-die
  [first-grid next-grid]
  (let [positions (get-positions first-grid :burning)]
    (test-positions next-grid positions :dead)))

(defn all-dead-trees-were-dead-or-burning
  [first-grid next-grid]
  (let [positions (get-positions next-grid :dead)]
    (test-positions first-grid positions :burning :dead)))

(defn get-grids
  "Return a seq of the grids from a forest-fire simulation,
including the initial grid and the n following grids"
  [n]
  (let [fire-channel (simulate-forestfire 10 8)]
    (for [i (range (inc n))]
      ((<!! fire-channel) :grid))))

(def steps 100)

(def grids (vec (get-grids steps)))

(defmacro are-range
  [argv expr n]
  `(are ~argv ~expr ~@(range (eval n))))

(deftest test-all-die
  []
  (are-range [i] (all-burning-trees-die (get grids i) (get grids (inc i))) steps))

(deftest test-all-were-burning
  []
  (are-range [i] (all-dead-trees-were-dead-or-burning (get grids i) (get grids (inc i))) steps))

(run-tests)