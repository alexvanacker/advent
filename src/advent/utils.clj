(ns advent.utils
  (:require [clojure.java.io :as io]))

(defn read-file
  " Function that returns the lines as an array found in the file
  located in resources/`filename`"
  [filename]
  (-> (slurp (io/resource filename))
      (clojure.string/split-lines)))

(defn read-file-ints
  [filename]
  (-> (read-file filename)
      (map read-string)))

(defn read-file-int-line
  "Function that reads a file that represents an array of integers in one line and returns that array."
  [filename]
  (-> (slurp (io/resource filename))
      (clojure.string/trim)
      (#(clojure.string/split % #","))))


(defn split-comma [string]
  (clojure.string/split string #","))

(defn read-file-lines-split
  "Reads a file, and splits each line with `,`."
  [filename]
  (as-> (slurp (io/resource filename)) v
      (clojure.string/split-lines v)
      (map split-comma v)))
 
