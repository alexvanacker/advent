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

(defn read-file [f]
  (-> (slurp f)
      (clojure.string/split-lines)))


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
       :else {:two two, :three three}
       )) {:two 0, :three 0} elts))


(defn sol20191 [mass]
  (- (quot mass 3)
     2))

(defn sol20191_complete []
  (reduce +
          (map (comp sol20191 read-string)
               (utils/read-file "2019_1.txt"))))

(defn mod_fuel_requirements
  ([mass]
   (mod_fuel_requirements (sol20191 mass) (list (sol20191 mass))))
   ([mass reqs]
    (let [req (sol20191 mass)]
      (println (str "computin mass " mass))
      (if (>= 0 req)
        (reduce + reqs)
        (recur req (conj reqs req))))))

(defn sol2019_2_complete []
  (reduce +
          (map (comp mod_fuel_requirements read-string)
               (utils/read-file "2019_1.txt"))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (sol args))

