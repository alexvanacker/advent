(ns advent.y2022.day06
  (:require [advent.utils :as utils]
            [clojure.test :refer [deftest is]]))

(def input (first (utils/read-file "2022_6.txt")))

(defn are-all-diff?
  ;; Returns true when all the characters in chars are different
  [chars]
  (= (count chars)
     (count (set chars))))

(defn indexed-matches?
  ;; Returns idx if all charactes in item are different
  [idx item]
  (when (are-all-diff? item)
    idx))

(defn get-marker-pos
  ;; Given a signal and a size, returns the index if the first character for which all the
  ;; characters in signal of size size-marker are different
  [signal size-marker]
  (+ size-marker
     (->> (partition size-marker 1 signal)
          (keep-indexed indexed-matches?)
          (first))))

(defn get-first-marker-pos
  [signal]
  (get-marker-pos signal 4))

(defn get-start-of-message
  [signal]
  (get-marker-pos signal 14))

(defn sol1
  []
  (get-first-marker-pos input))

(defn sol2
  []
  (get-start-of-message input))

(def signal-test "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(deftest marker-test
  []
  (is (= 7
         (get-first-marker-pos signal-test)))
  (is (= 5
         (get-first-marker-pos "bvwbjplbgvbhsrlpgdmjqwftvncz"))))
