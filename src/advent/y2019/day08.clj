(ns advent.y2019.day08
  (:require [advent.utils :as utils]
             [clojure.test :refer :all]))

(defn create-layers
 [width height input]
 (map #(partition width %)
      (partition (* width height) input)))

(defn get-least-zeroes-layer [w h input]
  (->> (create-layers w h input)
       (map flatten)
       (map frequencies)
       (apply min-key #(get % 0))))

(defn apply-pixel [top-p bottom-p]
    (if (= 2 top-p)
      bottom-p
      top-p))

(defn merge-layers [top-layer bottom-layer]
  (map #(map apply-pixel %1 %2)
       top-layer
       bottom-layer))

(defn render-image [w h input]
  (->> (create-layers w h input)
       (reduce merge-layers)))

(def d8input
  (->> (get (utils/read-file "2019_8.txt") 0)
       (map #(Character/digit % 10))))

(defn sol1 []
  (let [freqs (get-least-zeroes-layer 25 6 d8input)]
    (* (get freqs 1)
       (get freqs 2))))

(defn sol2 []
  (render-image 25 6 d8input))

(deftest test1
  (is (= '((0 1) (1 0))
         (render-image 2 2 (map #(Character/digit % 10) "0222112222120000")))))
