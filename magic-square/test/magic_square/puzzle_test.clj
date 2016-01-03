(ns magic-square.puzzle-test
  (:require [clojure.test :refer :all]
            [magic-square.puzzle :refer :all]))

(deftest test-magic-square
  (testing "all the rows, columns, and diagonals add to the same number"
    (is (= (set (sum-rows (magic-square values)))
           (set (sum-cols (magic-square values)))
           (set (sum-diagonals (magic-square values)))))

    (is (= 1
           (count (set (sum-rows (magic-square values))))
           (count (set (sum-cols (magic-square values))))
           (count (set (sum-diagonals (magic-square values))))))))
