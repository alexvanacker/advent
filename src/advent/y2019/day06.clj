(ns advent.y2019.day06
  (:require [advent.utils :as utils]
            [clojure.test :refer :all]))

(defn dfs
  ([graph node]
   (disj (dfs graph node #{})
         node))
  ([graph node discovered]
   (loop [discovered-updated (conj discovered node)
          to-check (filter #(not (contains? discovered-updated %)) (graph node))]
       (if (empty? to-check)
         discovered-updated
         (let [res (dfs graph (first to-check) discovered-updated)]
           (recur res
                  (rest to-check)))))))

(def dfs-mem (memoize dfs))

(defn is-in-list? [coll v]
  (some #(= % v) coll))

(defn graph-bsf [graph start]
  "Returns the map of child -> parent which make up all the paths from start in graph."
  (loop [queue (conj '() start)
         visited []
         parents {}]
    (if (empty? queue)
      parents
      (let [current_node (peek queue)
            neighbors (get graph current_node)
            not-visited (filter (complement #(is-in-list? visited %)) neighbors)
            new-queue (apply conj (pop queue) not-visited)
            new-parents (into parents (map #(hash-map % current_node) not-visited))]
        (if (is-in-list? visited current_node)
          (recur new-queue visited parents)
          (recur new-queue (conj visited current_node) new-parents))))))

(defn shortest-path [graph start end]
  "Using BFS, returns the shortest path from start to end in graph."
  (let [paths (graph-bsf graph start)]
    (loop [current end
           path []]
      (if (= current start)
        (conj path start)
        (recur (get paths current) (conj path current))))))


(defn count-all-links [graph]
  (->> (keys graph)
       (map #(dfs-mem graph %))
       (map count)
       (reduce +)))

(defn add-node [graph k]
  "Returns the graph with the added node which key is k."
  (if (get graph k)
    graph
    (assoc graph k [])))

(defn add-edge [graph [k v]]
  (let [ensure-graph (add-node graph k)]
    (assoc ensure-graph
           k
           (conj (get ensure-graph k) v))))


(defn build-undirected-graph [edges]
  " Given a vector of edges, builds the undirected graph represented
with adjacency lists"
  (->> (concat edges (map reverse edges))
     (reduce add-edge {})))

(defn input->graph [f]
  (->> (utils/read-file f)
       (map #(clojure.string/split % #"\)"))
       (map (fn [[k v]] [v k]))
       (reduce add-edge {})))

(defn input->undirected-graph [f]
  (->> (utils/read-file f)
       (map #(clojure.string/split % #"\)"))
       (build-undirected-graph)))

(defn s1 []
  (count-all-links (input->graph "2019_6.txt")))

(defn s2 []
  (- (count (shortest-path (input->undirected-graph "2019_6.txt") "YOU" "SAN"))
     3))

(def test-aoc (input->graph "2019_6_test.txt"))

(def test-aoc-trans (input->undirected-graph "2019_6_test_2.txt"))


(deftest build-undirected-graph-simple
  (is (= {"A" ["B"] "B" ["C" "A"] "C" ["B"]}
         (build-undirected-graph [["A" "B"] ["B" "C"]]))))

(deftest dfs-simple
  (is (= 2
         (count (dfs {"A" [] "B" ["C", "A"] "C" []} "B")))))

(def multiple-graph {"A" [] "B" ["A"] "C" ["B"] "D" ["C"]})

(deftest dfs-multiple
  (is (= 3
         (count (dfs multiple-graph "D")))))

(deftest memoize-version
  (is (= 3
         (count (dfs-mem multiple-graph "D")))))

(deftest count-all-links-test
  (is (= 6
         (count-all-links multiple-graph))))

(deftest dfs-aoc-test
  (is (= 7
         (count (dfs-mem test-aoc "L")))))

(deftest count-all-aoc-test
  (is (= 42
         (count-all-links test-aoc))))

(deftest orbital-transfers
  (is (= 4
         (- (count (shortest-path test-aoc-trans "YOU" "SAN"))
            3))))
