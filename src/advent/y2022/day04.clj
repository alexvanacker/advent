(ns advent.y2022.day04
  (:require [advent.utils :as utils]
            [clojure.string :as str]
            [clojure.test :refer [is deftest]]))

(defn one-contains?
  {:test
   (fn []
     (is (one-contains? '(2 2 2 4))))}
  [[start1 end1 start2 end2 :as _input]]
  (or
   (and (>= start1 start2) (<= end1 end2))
   (and (<= start1 start2) (>= end1 end2))))

(defn overlap?
  [[start1 end1 start2 end2 :as _input]]
  (or (and (>= start2 start1) (<= start2 end1))
      (and (>= start1 start2) (<= start1 end2))))

(defn line-matches?
   {:test (fn []
           (is (line-matches? "2-3,2-2")))}
  [line]
  (->> (str/split line #",")
       (map #(str/split % #"-"))
       (apply concat)
       (map read-string)
       (one-contains?)))

(defn sol1 []
  (->> (utils/read-file "2022_4.txt")
       (filter line-matches?)
       (count)))

(defn line-overlaps?
  [line]
  (->> (str/split line #",")
       (map #(str/split % #"-"))
       (apply concat)
       (map read-string)
       (overlap?)))

(defn sol2 []
  (->> (utils/read-file "2022_4.txt")
       (filter line-overlaps?)
       (count)))

(deftest overlap []
  (is (overlap? '(2 3 3 4)))
  (is (overlap? '(4 10 2 7)))
  (is (not (overlap? '(2 3 4 5))))
  (is (not (overlap? '(4 5 2 3)))))
