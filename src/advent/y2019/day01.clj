(ns advent.y2019.day01
  (:require [advent.utils :as utils]))


(defn fuel [mass]
  (- (quot mass 3)
     2))

(defn sol1 []
  (reduce +
          (map (comp fuel read-string)
               (utils/read-file "2019_1.txt"))))

(defn mod_fuel_requirements
  ([mass]
   (mod_fuel_requirements (fuel mass) (list (fuel mass))))
  ([mass reqs]
   (let [req (fuel mass)]
     (if (>= 0 req)
       (reduce + reqs)
       (recur req (conj reqs req))))))

(defn sol2 []
  (reduce +
          (map (comp mod_fuel_requirements read-string)
               (utils/read-file "2019_1.txt"))))
