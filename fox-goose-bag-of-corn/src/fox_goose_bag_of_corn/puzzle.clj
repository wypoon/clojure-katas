(ns fox-goose-bag-of-corn.puzzle
  (:require clojure.set))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

;; It would be more natural to use sets instead of vectors to represent who/what
;; are on the banks and the boat.

;; Logically possible moves:
;; 1. left bank to boat - if you are on the left bank - then the boat is empty -
;; one of #{:fox :goose :corn} plus :you move from bank to boat.
;; 2. right bank to boat - if you are on the right bank - then the boat is empty
;; - zero or one of #{:fox :goose :corn} plus :you move from bank to boat.
;; 3. boat to either bank - if boat is not empty, who/whatever is on boat move
;; to bank.
;; It is easier, however, to think of the sequence of moves that go together:
;; bank-to-boat-to-other-bank. The sequence consists of two of the above moves.
;; We then consider the states to be the states at the end of the two-move
;; sequences, and consider what states we can transition to from the current
;; state.
;; Then the states have :you on one bank, with one or more of #{:fox :goose :corn}
;; with :you, and the rest of them on the other bank, and nothing on the boat.
;; The next state should always have you on the other bank. You can take one
;; of #{:fox :goose :corn}, which is on the bank with you, to the other bank;
;; subject to the rules that :fox and :goose cannot be left alone together, and
;; :goose and :corn cannot be left alone together. (The rules imply that only
;; :fox and :corn can be left alone together.)

(def start-state [#{:fox :goose :corn :you} #{}])
(def goal-state [#{} #{:fox :goose :corn :you}])

(defn gen-right-transitions [state]
  ; state is a vector of 2 sets, with :you in the first set (left bank)
  ; return the states that this state can transition to: :you and one
  ; other from the left bank move to the right bank (second set)
  (for [x (first state) :when (not= x :you)]
    [(disj (first state) x :you) (conj (second state) x :you)]))

(defn gen-left-transitions [state]
  ; state is a vector of 2 sets, with :you in the second set (right bank)
  ; return the states that this state can transition to: :you and possibly
  ; one other from the right bank move to the left bank (first set)
  (cons [(conj (first state) :you) (disj (second state) :you)]
    (for [x (second state) :when (not= x :you)]
      [(conj (first state) x :you) (disj (second state) x :you)])))

(defn valid? [state]
  (and 
    (not-any? #(= % #{:fox :goose}) state)
    (not-any? #(= % #{:goose :corn}) state)))

(defn valid-right-transitions [state]
  (filter valid? (gen-right-transitions state)))

(defn valid-left-transitions [state]
  (filter valid? (gen-left-transitions state)))

(defn generate-transitions [state]
  (let [left-bank (first state)
        right-bank (second state)]
    (if (:you left-bank)
      (valid-right-transitions state)
      (valid-left-transitions state))))

(defn dfs [start goal]
  ((fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> (generate-transitions current)
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %)))))))
    [start] #{}))

;; translate from our format to the format expected by Carin

(defn- add-boat [state]
  [(first state) #{:boat} (last state)])

; we now consider states to be vectors of 3 sets, with the middle set containing
; :boat

; we need to generate the intermediate move that we omitted in our two-move
; state transitions
(defn- intermediate [state1 state2 right]
  ; right is a boolean to indicate if state1-to-state2 is a right transition
  ; return a vector of 3 sets that would represent the boat in transit from
  ; state1 to state2
  (if right
    [(first state2)
     (clojure.set/union #{:boat} (clojure.set/difference (first state1) (first state2)))
     (last state1)]
    [(first state1)
     (clojure.set/union #{:boat} (clojure.set/difference (last state1) (last state2)))
     (last state2)]))

(defn- translate [path]
  (loop [[s1 & s] path
         acc [s1]]
    (if (seq s)
      (let [s2 (first s)
            right (odd? (count s))]
        (recur s (conj acc (intermediate s1 s2 right) s2)))
      acc)))

(defn- vectorize [state]
  (map vec state))

(defn plan-a []
  (->> (dfs start-state goal-state)
       first
       (map add-boat)
       translate
       (map vectorize)))

(defn plan-b []
  (->> (dfs start-state goal-state)
       second
       (map add-boat)
       translate
       (map vectorize)))

(defn river-crossing-plan []
  ;; [[[:fox :goose :corn :you] [:boat] []]
  ;;  [[:fox :corn] [:boat :goose :you] []]
  ;;  [[:fox :corn] [:boat] [:goose :you]]
  ;;  [[:fox :corn] [:boat :you] [:goose]]
  ;;  [[:fox :corn :you] [:boat] [:goose]]
  ;;  [[:fox] [:boat :corn :you] [:goose]]
  ;;  [[:fox] [:boat] [:goose :corn :you]]
  ;;  [[:fox] [:boat :goose :you] [:corn]]
  ;;  [[:fox :goose :you] [:boat] [:corn]]
  ;;  [[:goose] [:boat :fox :you] [:corn]]
  ;;  [[:goose] [:boat] [:corn :fox :you]]
  ;;  [[:goose] [:boat :you] [:corn :fox]]
  ;;  [[:goose :you] [:boat] [:corn :fox]]
  ;;  [[] [:boat :goose :you] [:corn :fox]]
  ;;  [[] [:boat] [:corn :fox  :goose :you]]
  ;; ]
  (plan-b)
)
