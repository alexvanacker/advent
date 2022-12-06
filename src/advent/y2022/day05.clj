(ns advent.y2022.day05
  (:require [advent.utils :as utils]
            [clojure.test :refer [is deftest]]
            [clojure.string :as str]))

(def input-file (utils/read-file "2022_5.txt"))

(defn move-one
  [state column-from column-to]
  (let [to-move (peek (state column-from))]
    (update (update state column-from pop)
            column-to #(conj % to-move))))

(defn move
  [state column-from column-to nbr-move]
  (last (take (inc nbr-move)
              (iterate #(move-one % column-from column-to) state))))

(defn move-all
  [state column-from column-to nbr-move]
  (let [to-move (take nbr-move (state column-from))]
    (update (update state
                    column-from
                    #(drop nbr-move %))
            column-to
            #(concat to-move %))))

(defn input->head
  [input]
  (first (partition-by str/blank? input)))

(defn input->moves
  [input]
  (last (partition-by str/blank? input)))

(defn remove-brackets-and-whitespaces [string]
  (-> string
      (str/replace "[" "")
      (str/replace "]" "")
      (str/replace "   " "")))

(defn empty-blank-string-list
  [list]
  (filter #(not (str/blank? %)) list))

(defn parse-head-line
  [line]
  (into {} (map-indexed (fn [idx item] {(inc idx) item})
                        (map empty-blank-string-list
                             (map list
                                  (str/split (remove-brackets-and-whitespaces line)
                                             #" "))))))

(defn parse-head
  [input-head]
  (update-vals
   (reduce (fn [coll coll1] (merge-with into coll coll1))
           (->> input-head
                reverse
                (drop 1)
                (map parse-head-line)))

   (fn [coll] (reverse (into () coll)))))

(defn parse-move-line
  ;; Parses line that is Move x from f to t
  [line]
  (let [[nbr from to] (map read-string (re-seq #"\d+" line))]
    {:from from
     :to to
     :nbr nbr}))

(defn move-reduce
  [state
   {:keys [from to nbr]}]
  (move-all state from to nbr))

(defn run-instructions
  [input-end state]
  (reduce move-reduce
          state
          (map parse-move-line
               input-end)))

(defn sol
  []
  (let [head-input (input->head input-file)
        end-input (input->moves input-file)
        state (parse-head head-input)]
    (str/join (map first (vals (into (sorted-map) (run-instructions end-input state)))))))

(def test-input {1 (seq '("A" "B")) 2 '()})

(def test-head-input ["    [A] [B]" "[C] [D] [E]" " 1   2   3 "])

(deftest test-move-one
  []
  (is (= {1 '("B") 2 '("A")}
         (move-one test-input 1 2))))

(deftest parse-line-test
  []
  (is (= (parse-head-line "    [A] [B]")
         {1 '() 2 '("A") 3 '("B")})))

(deftest move-from-parse
  []
  (is (= {1 '() 2 '("C" "A" "D") 3 '("B" "E")}
         (move-one (parse-head test-head-input) 1 2))))

(deftest head-parse-test
  []
  (is (= {1 '("C") 2 '("A" "D") 3 '("B" "E")}
         (parse-head test-head-input))))

(deftest parse-move-line-test
  []
  (is (= {:from 4 :to 7 :nbr 5}
         (parse-move-line "move 5 from 4 to 7"))))


(def move-all-input {1 '("A" "B") 2 '("C")})

(deftest move-all-test
  []
  (is (= {1 '() 2 '("A" "B" "C")}
         (move-all move-all-input 1 2 2))))
