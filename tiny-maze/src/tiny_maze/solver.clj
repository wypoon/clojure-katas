(ns tiny-maze.solver)

;; It is not clearly stated in the instructions, but we assume that in a maze,
;; one can only move up, down, left, and right.
;; Also,  mazes are assumed to be rectangular, although not necessily square.

(defn nrows [maze] (count maze))
(defn ncols [maze] (count (first maze)))

; Attach coordinates to each position
(defn- index [maze]
  (for [i (range (ncols maze))
        j (range (nrows maze))]
    [(get-in maze [i j]) [i j]]))

(defn start [maze]
  "Return coordinates of the start position"
  (first (for [[v pos] (index maze) :when (= :S v)] pos)))

(defn end [maze]
  "Return coordinates of the end position"
  (first (for [[v pos] (index maze) :when (= :E v)] pos)))

(defn adj [maze pos]
  "Return the positions one can move to from pos;
   pos is assumed to be an open position (not a wall)"
  (let [deltas [[0 -1] [0 1] [-1 0] [1 0]]]
    (->> (map #(mapv + pos %) deltas)
         (filter #(< -1 (first %) (ncols maze))) ; within horizontal bounds
         (filter #(< -1 (last %) (nrows maze))) ; within vertical bounds
         (filter #(not= 1 (get-in maze %))) ; not a wall
         )))

; depth-first search of the maze
(defn find-paths [maze]
  (let [s (start maze)
        e (end maze)]
    ((fn search
       [path visited]
       (let [current (peek path)]
         (if (= e current)
           [path]
           (->> (adj maze current)
                (remove visited)
                (mapcat #(search (conj path %) (conj visited %)))))))
     [s] #{}) ; in the beginning, path consists of s, and visited is empty
    ))

;; Now, we need to write a matrix showing a path
(defn write [maze path]
  (reduce #(assoc-in %1 %2 :x) maze path))

(defn solve-maze [maze]
  (let [path (first (find-paths maze))]
    (write maze path)))
