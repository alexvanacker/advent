(ns advent.y2022.day03
  (:require [advent.utils :as utils]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]))

(defn input->compartments
  [input]
  (let [half-size (/ (count input) 2)]
    [(subs input 0 half-size)
     (subs input half-size)]))

(defn find-duplicates
  [[comp1 comp2 :as _list]]
  (set/intersection (set comp1) (set comp2)))

(defn char-range
  "Function to return a range of characters from `start` to `end` (including)."
  [start end]
  (map char (range (int start) (inc (int end)))))

(def a-z (char-range \a \z))
(def A-Z (char-range \A \Z))

(def prios
  (->> (concat a-z A-Z)
       (map-indexed (fn [idx item] [(inc idx) item]))
       (into {})
       (set/map-invert)))

(defn input->prio [input]
  (->> input
       input->compartments
       find-duplicates
       (map prios)
       (reduce +)))

(defn sol1-compute [lines]
  (->> lines
       (map input->prio)
       (reduce +)))

(defn sol1 []
  (->> (utils/read-file "2022_3.txt")
       (sol1-compute)))

(defn get-group-badge [group]
  ;; Group is supposed to be a seq of size 3
  {:pre [(= 3 (count group))]}
  (->> group
       (map set)
       (apply set/intersection)))

(defn sol2-compute [lines]
  (->> lines
       (partition 3)
       (map get-group-badge)
       (map first)
       (map prios)
       (reduce +)))

(defn sol2 []
  (->> (utils/read-file "2022_3.txt")
       sol2-compute))

(def test-input ["vJrwpWtwJgWrhcsFMMfFFhFp"
                 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                 "PmmdzqPrVvPwwTWBwg"
                 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                 "ttgJtRGJQctTZtZT"
                 "CrZsJsPPZsGzwwsLwLmpwMDw"])

(deftest test-1 []
  (is (= 157 (sol1-compute test-input))))

(deftest test-2 []
  (is (= 70
         (sol2-compute test-input))))
