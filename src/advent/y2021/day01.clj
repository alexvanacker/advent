(ns advent.y2021.day01
  (:require [advent.utils :as utils]))


(defn count-increase
  [nbrs]
  (->> nbrs
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(def input (utils/read-file-int "2021_01.txt"))


(defn sol1 []
  (count-increase input))

(defn sol2 []
  (->>  input
        (partition 3 1)
        (map (partial apply +))
        (count-increase)))
