(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as comb]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;; We solve the puzzle by applying a simple observation to narrow the search.
;; The sum of values is 27.0; thus the average value of a row/column/diagonal
;; is 9.0. For all the rows, columns, and diagonals to sum to the same value,
;; they must all sum to 9.0.

;; functions needed for testing (will be used for the test as well,
;; so we just collect them here, as the test requires this namespace)

(defn sum-rows [m]
  (map #(reduce + %) m))

(defn sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

;; function to generate all the solutions (there are 8, which is
;; the number of symmetries of the square, 4 reflectional symmetries
;; and 4 rotational symmetries)

(defn gen-squares [values]
  (->> (comb/permutations values)
       (map #(partition 3 %)) ; partition each permutation into 3s
       (map #(vec (map vec %))) ; convert to vector of vectors
       (filter #(= [9.0 9.0 9.0] (sum-rows %)))
       (filter #(= [9.0 9.0 9.0] (sum-cols %)))
       (filter #(= [9.0 9.0] (sum-diagonals %)))
       ))

(defn magic-square [values]
  (first (gen-squares values)))
