(ns advent.utils
  (:require [clojure.java.io :as io]))

(defn read-file
  " Function that returns the lines as an array found in the file
  located in resources/`filename`"
  [filename]
  (-> (slurp (io/resource filename))
      (clojure.string/split-lines)))
