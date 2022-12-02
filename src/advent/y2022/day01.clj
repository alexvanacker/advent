(ns advent.y2022.day01
  (:require [advent.utils :as utils]
            [clojure.string :as str]))

(defn get-lists
  [input]
  (filter #(not (= '("") %))
          (partition-by str/blank? input)))

(defn to-int-list
  [list]
  (map read-string list))

(defn sum-list
  [list]
  (reduce + list))

(defn sol []
  (->> (utils/read-file "2022_1.txt")
       (get-lists)
       (map to-int-list)
       (map sum-list)
       #(apply max %)))

(defn sol2 [])
(->> (utils/read-file "2022_1.txt")
     (get-lists)
     (map to-int-list)
     (map sum-list)
     (sort >)
     (take 3)
     (reduce +))
