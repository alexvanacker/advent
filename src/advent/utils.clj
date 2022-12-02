(ns advent.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file
  " Function that returns the lines as an array found in the file
  located in resources/`filename`"
  [filename]
  (-> (slurp (io/resource filename))
      (str/split-lines)))

(defn read-file-ints
  [filename]
  (-> (read-file filename)
      (map read-string)))

(defn read-file-int-line
  "Function that reads a file that represents an array of integers in one line and returns that array."
  [filename]
  (-> (slurp (io/resource filename))
      (str/trim)
      (#(str/split % #","))))


(defn split-comma [string]
  (str/split string #","))

(defn read-file-lines-split
  "Reads a file, and splits each line with `,`."
  [filename]
  (as-> (slurp (io/resource filename)) v
      (str/split-lines v)
      (map split-comma v)))
 
