(ns advent.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent.utils :as utils]))

(defn sum_reduce [[freq_set prev] x]
  (let [res (+ prev x)]
    (if (contains? freq_set res) (reduced [res])
        [(conj freq_set res) res])))

(defn find_first_dup [freqs start input]
  (reduce sum_reduce [freqs start] input))

(defn sol [input]
  (loop [freqs #{0} start 0]
    (let [res (find_first_dup freqs start input)]
      ;; Ugly, maybe return a map that has the 'first_dup' key
      (if (== (count res) 1)
        res
        (recur (set/union freqs (get res 0)) (get res 1))))))

(defn contains-exact-n? [elts n]
  (contains? (set (vals (frequencies elts)))
             n))

(defn sol [elts]
  (reduce
   (fn [{:keys [two three]} x]
     (cond
       (and
        (contains-exact-n? x 2)
        (contains-exact-n? x 3)) {:two (inc two), :three (inc three)}
       (contains-exact-n? x 2) {:two (inc two), :three three}
       (contains-exact-n? x 3) {:two two, :three (inc three)}
       :else {:two two, :three three})) {:two 0, :three 0} elts))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (sol args))

