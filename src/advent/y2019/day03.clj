(ns advent.y2019.day03
  (:require [advent.utils :as utils]
            [clojure.test :refer :all]))

(defn move_up [x y]
  [x (inc y)])

(defn move_down [x y]
  [x (dec y)])

(defn move_left [x y]
  [(dec x) y])

(defn move_right [x y]
  [(inc x) y])

(def opmap {"U" move_up
            "L" move_left
            "R" move_right
            "D" move_down})

(defn move [[x y] move_str]
  (let [letter (str (first move_str))
        nbr (Integer/parseInt (subs move_str 1) 10)]
    (loop [coords [x y]
           points []]
;;      (println (str "Starting at " coords))
      (let [new_coords (apply (opmap letter) coords)
            new_points (conj points new_coords)]
        (if (= nbr (count new_points))
          new_points
          (recur new_coords new_points))))))

(defn move_reduce [points move_str]
  ;;(println (str "Processing " move_str))
  (let [new_points (move (last points) move_str)]
    (concat points new_points)))

(defn get_wire_points [moves]
  (remove #(= [0 0] %) (reduce move_reduce [[0 0]] moves)))

(defn abs [n]
  (max n (- n)))

(defn manhattan_distance [[x y]]
  (+ (abs x)
     (abs y)))

(defn closest_intersect [[wire1 wire2]]
  (let [intersect (clojure.set/intersection wire1 wire2)]
    (first (into (sorted-map) (zipmap (map manhattan_distance intersect) intersect)))))

(defn string-to-wire [string]
  (get_wire_points (#(clojure.string/split % #",") string)))

(defn get-closest-intersect [array-of-string]
  (let [wires (map (comp set string-to-wire) array-of-string)]
    (first (closest_intersect wires))))

(defn get-cost-to-point [wire [x y]]
  (let [full-wire (conj wire [0 0])]
    (.indexOf full-wire [x y])))

(defn get-intersect-costs [[wire1 wire2]]
  (let [intersect (clojure.set/intersection (set wire1) (set wire2))]
    (into (sorted-map) (zipmap (map #(+ (get-cost-to-point wire1 %)
                                        (get-cost-to-point wire2 %)) intersect) intersect))))

(defn get-lowest-intersect-cost [array-of-string]
  (let [wires (map string-to-wire array-of-string)]
    (first (first (get-intersect-costs wires)))))

(def input (utils/read-file "2019_3.txt"))

(defn sol2 []
  (get-lowest-intersect-cost input))

(defn sol1 []
  (get-closest-intersect input))

(deftest cost-intersect-1
  (is (= 30
         (get-lowest-intersect-cost ["R8,U5,L5,D3" "U7,R6,D4,L4"]))))

(deftest cost-test-zero
  (is (= 0
         (get-cost-to-point (string-to-wire "R8,U5")
                            [0 0]))))

(deftest cost-test
  (is (= 13
         (get-cost-to-point (string-to-wire "R8,U5,L5,D3")
                            [8 5]))))

(deftest simple-case
  (is (= 6
         (get-closest-intersect ["R8,U5,L5,D3" "U7,R6,D4,L4"]))))

(deftest test2
  (is (= 159
         (get-closest-intersect ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                 "U62,R66,U55,R34,D71,R55,D58,R83"]))))
