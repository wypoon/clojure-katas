(ns sudoku.solver-test
  (:require [clojure.test :refer :all]
            [sudoku.solver :refer :all]))

(def board
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]]
)

(def solved-board
  [[5 3 4 6 7 8 9 1 2]
   [6 7 2 1 9 5 3 4 8]
   [1 9 8 3 4 2 5 6 7]
   [8 5 9 7 6 1 4 2 3]
   [4 2 6 8 5 3 7 9 1]
   [7 1 3 9 2 4 8 5 6]
   [9 6 1 5 3 7 2 8 4]
   [2 8 7 4 1 9 6 3 5]
   [3 4 5 2 8 6 1 7 9]]
)

(def solved-columns
  [[5 6 1 8 4 7 9 2 3]
   [3 7 9 5 2 1 6 8 4]
   [4 2 8 9 6 3 1 7 5]
   [6 1 3 7 8 9 5 4 2]
   [7 9 4 6 5 2 3 1 8]
   [8 5 2 1 3 4 7 9 6]
   [9 3 5 4 7 8 2 6 1]
   [1 4 6 2 9 5 8 3 7]
   [2 8 7 3 1 6 4 5 9]]
)

(def solved-boxes
  [[5 3 4 6 7 2 1 9 8]
   [6 7 8 1 9 5 3 4 2]
   [9 1 2 3 4 8 5 6 7]
   [8 5 9 4 2 6 7 1 3]
   [7 6 1 8 5 3 9 2 4]
   [4 2 3 7 9 1 8 5 6]
   [9 6 1 2 8 7 3 4 5]
   [5 3 7 4 1 9 2 8 6]
   [2 8 4 6 3 5 1 7 9]]
)

(deftest test-values-for-coord
  (testing "row values for coord"
    (let [coord [1 1]
          values #{6 1 9 5}]
      (is (= values (row-values board coord)))))
  (testing "column values for coord"
    (let [coord [1 1]
          values #{3 9 6}]
      (is (= values (col-values board coord)))))
  (testing "box values for coord"
    (let [coord [1 1]
          values #{5 3 6 9 8}]
      (is (= values (box-values board coord)))))
  (testing "valid values for coord"
    (let [coord [1 1]
          values #{2 4 7}]
      (is (= values (valid-values-for board coord)))))
)

(deftest test-structures
  (testing "rows are correct"
    (is (= solved-board (rows solved-board))))
  (testing "columns are correct"
    (is (= solved-columns (cols solved-board))))
  (testing "boxes are correct"
    (is (= solved-boxes (boxes solved-board))))
)

(deftest test-solve-puzzle
  (testing "can solve a simple puzzle"
    (is (= solved-board (simple-solve board))))
  (testing "can solve another easy puzzle"
    (let [puzzle (from-string easy)]
      (is (valid-solution? (simple-solve puzzle)))))
  (testing "can solve a gentle puzzle"
    (let [puzzle (from-string gentle)]
      (is (not (valid-solution? (simple-solve puzzle))))
      (is (valid-solution? (solve puzzle)))))
  (testing "can solve a diabolical puzzle"
    (let [puzzle (from-string diabolical)]
      (is (not (valid-solution? (simple-solve puzzle))))
      (is (valid-solution? (solve puzzle)))))
  (testing "can solve a backtracker puzzle"
    (let [puzzle (from-string backtracker)]
      (is (not (valid-solution? (simple-solve puzzle))))
      (is (valid-solution? (solve puzzle)))))
  (testing "can solve a minimal puzzle"
    (let [puzzle (from-string minimal)]
      (is (valid-solution? (solve puzzle)))))
)
