(ns advent.y2019.day02
  (:require [clojure.string :as  str]
           [advent.utils :as utils]))

(def ops {1 (fn [x y] (+ x y))
          2 (fn [x y] (* x y))})

;; Isn't there better? (other than naming)
(defn get_vec_square [vec pos]
  (vec (vec pos)))

(defn program_alarm
  ([vect]
   (program_alarm vect 0))
  ([vect pos]
   ;;(println  (str "input: " vect " " pos))
   (let [op (vect pos)]
     (if (= op 99)
       vect
       (recur (assoc vect
                     (vect (+ pos 3))
                     ((get ops op)
                      (get_vec_square vect (+ pos 1))
                      (get_vec_square vect (+ pos 2))))
              (+ 4 pos))))))

(defn program_alarm_input [x y]
  (let [input (vec (map read-string (utils/read-file-int-line "2019_2.txt")))]
    (program_alarm (assoc (assoc input 1 x) 2 y))))
