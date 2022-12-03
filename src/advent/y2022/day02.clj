(ns advent.y2022.day02
  (:require [advent.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def symbol-points {:rock 1 :paper 2 :scissors 3})
(def result-points {:win 6 :draw 3 :lose 0})
(def wanted-result {"X" :lose "Y" :draw  "Z" :win})
(def game-matrix {:rock :scissors, :scissors :paper, :paper :rock})
(def letter-to-symbol {"X" :rock, "Y" :paper, "Z" :scissors
                       "A" :rock, "B" :paper, "C" :scissors})

(def elve-to-symbol {"A" :rock "B" :paper "C" :scissors})
(def letter->symbol (merge wanted-result elve-to-symbol))

(defn result
  [elve me]
  (if (= elve me)
    :draw
    (if (= elve (me game-matrix))
      :win
      :lose)))

(defn compute-points
  [[elve me :as _list]]
  (+ (me symbol-points)
     (-> (result elve me)
         (result-points))))

(defn get-needed-play
  [elve wanted_result]
  (case wanted_result
    :draw elve
    :lose (elve game-matrix)
    :win (elve (set/map-invert game-matrix))))

(defn compute-points-2
  ;; Compute the points knowing what elve played and what result we want
  [[elve wanted_result :as _list]]
  (let [my_play (get-needed-play elve wanted_result)]
    (compute-points [elve my_play])))

(defn letter-to-symbol-list [list]
  (map letter-to-symbol list))

(defn letter-list->symbol [list]
  (map letter->symbol list))

(defn sol1
  []
  (let [input (utils/read-file "2022_2.txt")
        parsed (map #(str/split % #" ") input)
        with-symbols (map letter-to-symbol-list parsed)]
    (->> with-symbols
         (map compute-points)
         (reduce +))))

(defn sol2
  []
  (->> (utils/read-file "2022_2.txt")
       (map #(str/split % #" "))
       (map letter-list->symbol)
       (map compute-points-2)
       (reduce +)))
