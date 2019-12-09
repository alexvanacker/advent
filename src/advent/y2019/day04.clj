(ns advent.y2019.day04
  (:require [clojure.test :refer :all]))

(defn has-cons-double-digits-regexp? [nbr]
  (some? (re-find #"(\d)\1" nbr)))

(defn has-exactly-two-same-digits-conseq? [nbr]
  (some? (seq (filter #(= 2
                          (count (get % 0)))
                      (re-seq #"(\d)\1+" nbr)))))
  ; Leaving this here as I'd like to understand why it didn't work in the end
  ;(if-let [doubles (re-seq #"(\d)\1" nbr)]
  ;  (if-let [more (re-seq #"(\d)\1{2,}" nbr)]
  ;    (< (count more) (count doubles))
  ;    true)
  ;  false))

(defn is-increasing-only? [nbrs]
  (apply <= nbrs))

(defn to-digits [nbr]
  (map #(Character/digit % 10) (str nbr)))

(defn matches [x]
  (and (is-increasing-only? (to-digits x))
       (has-cons-double-digits-regexp? (str x))))

(defn matches-all-criteria? [x]
  (and (is-increasing-only? (to-digits x))
       (has-exactly-two-same-digits-conseq? (str x))))

(defn sol2 []
  (let [input (range 357253 892943)]
    (count (filter matches-all-criteria? input))))

(defn sol1 []
  (let [input (range 357253 892943)]
    (count (filter matches input))))

(deftest sol2-1
  (is (= true
         (matches-all-criteria? 11111122))))

(deftest sol2-2
  (is (= false (matches-all-criteria? 123444))))

(deftest no-large-group-end
  (is (= false
         (has-exactly-two-same-digits-conseq? "123444"))))

(deftest exactyl-two-and-three
  (is (= true
         (has-exactly-two-same-digits-conseq? "1223222"))))

(deftest double-digits-end
  (is (= true (has-cons-double-digits-regexp? "3577"))))

(deftest double-digits-1-digit
  (is (= false (has-cons-double-digits-regexp? ""))))

(deftest double-digits-false
  (is (= false (has-cons-double-digits-regexp? "212"))))

(deftest has-double-digits
  (is (= true
         (has-cons-double-digits-regexp? "22"))))
