(ns sudoku.solver
  (:require [clojure.set :as set]))

;; Example puzzles

(def easy
  ["2....1.38",
   "........5",
   ".7...6...",
   ".......13",
   ".981..257",
   "31....8..",
   "9..8...2.",
   ".5..69784",
   "4..25...."]
)

(def gentle
  [".1.42...5",
   "..2.71.39",
   ".......4.",
   "2.71....6",
   "....4....",
   "6....74.3",
   ".7.......",
   "12.73.5..",
   "3...82.7."]
)

(def diabolical
  [".9.7..86.",
   ".31..5.2.",
   "8.6......",
   "..7.5...6",
   "...3.7...",
   "5...1.7..",
   "......1.9",
   ".2.6..35.",
   ".54..8.7."]
)

(def backtracker
  ["1..9.7..3",
   ".8.....7.",
   "..9...6..",
   "..72.94..",
   "41.....95",
   "..85.43..",
   "..3...7..",
   ".5.....4.",
   "2..8.6..9"]
)

(def minimal
  [".98......",
   "....7....",
   "....15...",
   "1........",
   "...2....9",
   "...9.6.82",
   ".......3.",
   "5.1......",
   "...4...2."]
)

;; Convert vector of strings to a matrix (vector of vector of ints),
;; with 0 denoting an empty square

(def all-values #{1 2 3 4 5 6 7 8 9})

(def empty-value 0)

(defn from-string [lines]
  (mapv #(vec (map (fn [c] (if (= \. c) 0 (- (int c) (int \0)))) %)) lines))

;; Coordinate-based programming (not the most functional approach)

;; For each coordinate, figure out the set of filled values for each of the
;; row, column and box the square is in, and thus obtain the set of values
;; that may be used for filling it, if it is not yet filled.

(def coords (for [i (range 9) j (range 9)] [i j]))

(defn transpose [board]
  (vec (apply map vector board)))

(defn non-empty? [value]
  (not= empty-value value))

(defn row-values [board coord]
  "The set of values in the row containing coord."
  (let [[r _] coord]
    (set (filter non-empty? (get board r)))))

(defn col-values [board coord]
  "The set of values in the column containing coord."
  (let [[r c] coord]
    (row-values (transpose board) [c r])))

(defn top-left [coord]
  (vec (map #(* % 3) (map #(quot % 3) coord))))

(defn box [coord]
  (let [[r c] (top-left coord)]
    (for [i (range r (+ r 3))
          j (range c (+ c 3))]
      [i j])))

(defn box-values [board coord]
  "The set of values in the box containing coord."
  (set (filter non-empty? (map #(get-in board %) (box coord)))))

(defn valid-values-for [board coord]
  "The set of values that may be used to fill coord. If the coord is already
   filled, this is the singleton set containing the filled value."
  (if (non-empty? (get-in board coord))
    #{(get-in board coord)}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (box-values board coord)))))

;; A more functional approach

(def rows identity)

(def cols transpose)

(defn group [board]
  (partition 3 (map #(partition 3 %) board)))

(defn flatvec [s] (vec (flatten s)))

(defn ungroup [m]
  (mapv flatvec m))

(defn boxes [board]
  "A matrix (vector of vectors) where the vectors are the boxes."
  (->> board
       group
       (mapcat cols)
       ungroup))

(defn valid-rows? [board]
  "Are all the rows filled with 1 to 9?"
  (->> board
       rows
       (map set)
       (every? #(= all-values %))))

(defn valid-cols? [board]
  "Are all the columns filled with 1 to 9?"
  (->> board
       cols
       (map set)
       (every? #(= all-values %))))

(defn valid-boxes? [board]
  "Are all the boxes filled with 1 to 9?"
  (->> board
       boxes
       (map set)
       (every? #(= all-values %))))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-boxes? board)))

;; Solving the puzzle

;; For each square, we consider the valid values for it.
;; If it is already filled, there is only one valid value.
;; If it is not filled, and there is only one valid value, we fill it with
;; that value.
;; prune returns a new board with values filled in where possible.
;; Repeated pruning may fill more values. The simplest puzzles may be solved
;; using this technique. When pruning does not fill the board further, i.e.,
;; we have a fixed point, and the board is not solved yet, we need additional
;; techniques.
;; It would be nice to implement the prune function using a more functional
;; approach than the coordinate-based approach below.
(defn prune [board]
  (let [f (fn [board coord]
            (let [v (valid-values-for board coord)]
              (if (= 1 (count v))
                (assoc-in board coord (first (seq v)))
                board)))]
    (reduce f board coords)))

(defn simple-solve [board]
  "Attempt to solve the board using repeated pruning until a fixed point
   is reached. Return the resulting board."
  (if (valid-solution? board)
    board
    (let [board2 (prune board)]
      (if (= board board2)
        board
        (recur board2)))))

;; If repeated pruning does not solve the puzzle, we first look for an
;; unfilled square with the minimal number of valid values. This number
;; will be at least 2 (otherwise we could continue pruning).
;; We then fill the square with each valid value and do a backtracking
;; search. In this search, we stop when a solution is found, but we also
;; terminate a search path when the board cannot be solved using the filled
;; values. We call such a board blocked.

(defn blocked? [board]
  "A board is blocked if there is some square for which there are no valid
   values."
  (some #(= 0 (count (valid-values-for board %))) coords))

(defn get-candidate [board]
  (or
   (first (filter #(= 2 (count (valid-values-for board %))) coords))
   (first (filter #(= 3 (count (valid-values-for board %))) coords))
   (first (filter #(= 4 (count (valid-values-for board %))) coords))
   (first (filter #(= 5 (count (valid-values-for board %))) coords))
   (first (filter #(= 6 (count (valid-values-for board %))) coords))
   (first (filter #(= 7 (count (valid-values-for board %))) coords))
   (first (filter #(= 8 (count (valid-values-for board %))) coords))
   ; I do not think the last condition should occur, but I guess if
   ; the board is completely unfilled to begin with?
   (first (filter #(= 9 (count (valid-values-for board %))) coords))))

(defn solve- [board]
  (let [b (simple-solve board)]
    (if (valid-solution? b)
      [b]
      (if (blocked? b)
        []
        (let [c (get-candidate b)]
          (for [v (valid-values-for b c)
                solution (solve- (assoc-in b c v))]
            solution))))))

(defn solve [board]
  (first (solve- board)))
