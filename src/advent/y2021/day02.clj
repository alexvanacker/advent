(ns advent.y2021.day02
  (:require [advent.utils :as utils]
            [clojure.string :as string]))

(def input (utils/read-file "2021_02.txt"))

(defn depth? [x]
  (or (string/starts-with? x "up")
      (string/starts-with? x "down")))

(defn calc-meas [input]
  (let [split (string/split input #" ")
        dir (first split)
        measure (Integer/parseInt (last split))]
    (case dir
      "down" measure
      "forward" measure
      (- measure))))

(defn sol1 []
  (let [depth (apply + (map calc-meas (filter depth? input)))
        hor (apply + (map calc-meas (remove depth? input)))]
    (* depth hor)))

(defn compute-all [cur input]
  (let [measure (calc-meas input)]
    (if (depth? input)
      (update cur :aim + measure)
      (-> cur
          (update :hor + measure)
          (update :depth + (* measure (:aim cur)))))))

(defn sol2 []
  (let [end-result (reduce compute-all {:hor 0 :aim 0 :depth 0} input)]
    (* (:depth end-result) (:hor end-result))))
