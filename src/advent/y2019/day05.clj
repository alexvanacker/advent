(ns advent.y2019.day05
  (:require [advent.utils :as utils]
            [clojure.test :refer :all]))


;; TODO Refactor to feed the map of vect/output/input/pos 
(defn get-value [input mode vect]
  (if (= mode 1) input (vect input)))

(defn get-nth-next-value
  [vect pos param-modes n]
   (get-value (vect (+ pos n))
              (param-modes (dec n))
              vect))

(def ops {1 {:arity 2
             :function (fn [param-modes vect pos input output]
                         {:vect
                          (assoc vect
                                 (vect (+ pos 3))
                                 (+ (get-nth-next-value vect pos param-modes 1)
                                    (get-nth-next-value vect pos param-modes 2)))
                          :output output
                          :input input
                          :pos (+ pos 4)})}
          2 {:arity 2
             :function (fn [param-modes vect pos input output]
                         {:vect
                          (assoc vect
                                 (vect (+ pos 3))
                                 (* (get-nth-next-value vect pos param-modes 1)
                                    (get-nth-next-value vect pos param-modes 2)))
                          :output output
                          :input input
                          :pos (+ pos 4)})}
          3 {:arity 1
             :function (fn [param-modes vect pos input output]
                         {:vect (assoc vect (vect (+ pos 1)) input)
                          :output output
                          :input input
                          :pos (+ pos 2)})}
          4 {:arity 1
             :function (fn [param-modes vect pos input output]
                         {:vect vect
                          :output (conj output
                                        (get-nth-next-value vect pos param-modes 1))
                          :input input
                          :pos (+ pos 2)})}
          5 {:arity 2
             :function (fn [param-modes vect pos input output]
                         (if (zero? (get-nth-next-value vect pos param-modes 1))
                           {:vect vect :output output :input input :pos (+ pos 3)}
                           {:vect vect
                            :output output
                            :input input
                            :pos (get-nth-next-value vect pos param-modes 2)}))}
          6 {:arity 2
             :function (fn [param-modes vect pos input output]
                         (if (not= 0 (get-nth-next-value vect pos param-modes 1))
                           {:vect vect :output output :input input :pos (+ pos 3)}
                           {:vect vect
                            :output output
                            :input input
                            :pos (get-nth-next-value vect pos param-modes 2)}))}
          7 {:arity 3
             :function (fn [param-modes vect pos input output]
                         (let [store-pos (vect (+ pos 3))
                               first (get-nth-next-value vect pos param-modes 1)
                               second (get-nth-next-value vect pos param-modes 2)]
                           (if (< first second)
                             {:vect (assoc vect store-pos 1)
                              :output output
                              :input input
                              :pos (+ pos 4)}
                             {:vect (assoc vect store-pos 0)
                              :output output
                              :input input
                              :pos (+ pos 4)})))}
          8 {:arity 3
             :function (fn [param-modes vect pos input output]
                         (let [store-pos (vect (+ pos 3))
                               first (get-nth-next-value vect pos param-modes 1)
                               second (get-nth-next-value vect pos param-modes 2)]
                           (if (= first second)
                             {:vect (assoc vect store-pos 1)
                              :output output
                              :input input
                              :pos (+ pos 4)}
                             {:vect (assoc vect store-pos 0)
                              :output output
                              :input input
                              :pos (+ pos 4)})))}})

(defn get-op [string]
  (ops (Integer/parseInt
        (clojure.string/join
         (take-last 2 string)))))

(defn get-modes [string arity]
  """ Returns the vector of parameter modes to apply. """
  ;; Note: not proud of this implem...
  (let [init (map #(Character/digit % 10) (drop-last 2 string))
        missing-zeroes (repeat (- arity (count init)) 0)]
    (vec
     (reverse
     (flatten
      (conj init missing-zeroes))))))

(defn compute [vect pos input output]
  """ Given a string such as 0002, extracts the operation
      and its parameters mode, then applies the operation, return vect, input and output."""
  (let [op-str (str (vect pos))
        op (get-op op-str)
        param_modes (get-modes op-str (:arity op))]
    ((:function op) param_modes vect pos input output)))

(defn full-compute
  ([vect input output]
  (full-compute {:vect vect
                 :input input
                 :output output
                 :pos 0}))
  ([{vect :vect input :input output :output pos :pos}]
  (if (= 99 (vect pos))
     {:vect vect :input input :output output}
     (recur (compute vect pos input output)))))


(defn full-compute-str [vec-str input]
  (full-compute (vec (map read-string vec-str)) input []))

(defn sol1 []
  (full-compute-str
   (utils/read-file-int-line "2019_5.txt")
   1))

(defn sol2 []
  (full-compute-str
   (utils/read-file-int-line "2019_5.txt")
   5))



(deftest equals-8
  (is (= [1]
         (:output (full-compute [3,9,8,9,10,9,4,9,99,-1,8] 8 [])))))

(deftest not-equals-8
  (is  (= [0]
          (:output (full-compute [3,9,8,9,10,9,4,9,99,-1,8] 9 [])))))

(deftest equals-zero
  (is (= [0]
         (:output (full-compute [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 [])))))

(deftest op2
  (is (= [1102 2 2 4]
         (:vect (compute [1102 2 2 3] 0 [] [])))))

(deftest simple-output
  (is (= [4]
         (:output (full-compute [4 0 99] 0 [])))))

(deftest input-output
  (is (= [1]
       (:output (full-compute [3 0 4 0 99] 1 [])))))

(deftest multiple-ops-vect
  (is (= [2 1 1 0 1002 0 2 4 4 7 99]
         (:vect (full-compute [1 1 1 0 1002 0 2 7 4 7 99] [] [])))))


(deftest e2e
  (is (= [2 1 1 0 1002 0 2 4 4 7 99]
         (:vect (full-compute-str ["1" "1" "1" "0" "1002" "0" "2" "7" "4" "7" "99"] [])))))

(deftest get-modes-complete
  (is (= [1 0]
         (get-modes "102" 2))))

(deftest get-modes-one-zero
  (is (= [0 1]
         (get-modes "1001" 2))))

(deftest op1
  (is (= [2 1 1 0]
         (:vect (compute ["1" 1 1 0] 0 [] [])))))


