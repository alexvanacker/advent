(ns advent.y2022.day07
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [advent.utils :as utils]))

;; My mistake here was to rely on nested maps which are hard to filter/manipulate
;; next time I will use another structure.
(def test-input (str/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))

(defn is-ls? [input] (str/starts-with? input "$ ls"))
(defn is-cd? [input] (str/starts-with? input "$ cd"))

(defn cd [dir path structure]
  ;; Returns the structure and
  (if (= ".." dir)
    [(pop path) structure]
    [(conj path dir) structure]))

(defn parse-file-line [line]
  (let [split-result (str/split line #" ")]
    {(get split-result 1) (read-string (first split-result))}))

(defn parse-ls-line [line]
  (if (str/starts-with? line "dir")
    {(get (str/split line #" ") 1) {}}
    (parse-file-line line)))

(defn parse-line
  [[path structure] line]
  (if (is-cd? line)
    (cd (last (str/split line #" ")) path structure)
    (if (is-ls? line)
      [path (assoc-in structure path {})]
      [path (assoc-in structure path (merge (get-in structure path) (parse-ls-line line)))])))

(defn input->structure
  ;; input is an array of instructions and their resutls
  [input]
  (get (reduce parse-line [[] {}] input) 1))

;; compute path size
;; If it's a file, return its size
;; otherwise sums all the files and folders
(defn get-size
  [structure path]
  (let [dir (get-in structure path)]
    (if-not (map? dir)
      dir
      (reduce (fn [acc k] (+ acc (get-size structure (conj path k)))) 0 (keys dir)))))

(defn get-sizes [structure path]
  {(peek path) (get-size structure path)})

(defn keep-nested-maps [m]
  (filter (fn [k] (when (map? (get m k)) k)) (keys m)))

(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk. Only keeps keys with a map (not leaves)."
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (when (map? v)
                (map (fn [x] (conj node x)) (keep-nested-maps v)))))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn get-all-sizes [structure]
  (map #(get-sizes structure %) (keys-in structure)))

(defn sol1
  []
  (let [structure (input->structure (utils/read-file "2022_7.txt"))
        sizes (get-all-sizes structure)]
    (reduce + (map second (filter (fn [[_ v]] (< v 100000)) (map flatten (map vec sizes)))))))

(defn get-folder-to-delete
  [sizes]
  (let [vec-sizes (map flatten (map vec sizes))
        root-size (second (first vec-sizes))
        to-free (- 30000000 (- 70000000 root-size))]
    (println to-free)
    (->> vec-sizes
         (filter (fn [[_ v]] (>= v to-free)))
         (sort-by second)
         first
         second)))

(defn sol2
  []
  (let [sizes (get-all-sizes (input->structure (utils/read-file "2022_7.txt")))]
    (get-folder-to-delete sizes)))

(def test-input-ls (str/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d"))

(def expected-ls {"a" {}
                  "d" {}
                  "b.txt" 14848514
                  "c.dat" 8504156})

(deftest parse-ls-test
  []
  (is (= {"b.txt" 14848514}
         (parse-ls-line "14848514 b.txt")))
  (is (= {"a" {}}
         (parse-ls-line "dir a"))))

(deftest cd-test []
  (is (= [["a"] {}]
         (cd ".." ["a" "b"] {})))
  (is (= [["/"] {}]
         (cd "/" [] {}))))

(deftest parse-line-test []
  (is (= {"/" expected-ls}
         (input->structure test-input-ls))))

(deftest sizes []
  (is (= 1 (get-size {"/" {"a" {"b" 1}}} ["/" "a"])))
  (is (= 2 (get-size {"/" {"a" 1 "b" {"c" 1}}} ["/"]))))
