(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn is-word? [s] 
  (some #{s} words))

; for a given word, generate a sequence of strings of the same length
; that can be obtained from it by changing a single letter
; (the strings are not necessarily words in the dictionary)
(defn generate-candidates [word]
  (if (empty? word)
    []
    ; first part: vary the first letter and keep the rest of the word
    ; second part: keep the first letter, and vary one of the other letters,
    ; i.e., append candidates found for the rest of the word
    (into [] (concat (for [x alphabet :when (not= x (first word))] (apply str (conj (rest word) x)))
                     (map #(str (first word) %) (generate-candidates (rest word)))))))

; for a given word, return the sequence of words that can be reached in
; one step
(defn one-step [word]
  (filter is-word? (generate-candidates word)))

;; breadth-first traversal of a tree

; A traversal function normally takes (some representation of) a graph and a start vertex.
; Here, the representation of the graph is given by an adjacency function.
; If traversing a graph, we'd keep a set, visited, to keep track of visited vertices/nodes;
; but here, the adj function is understood to return only the child nodes.
(defn bft [adj v]
  ((fn bft- [acc queue]
    (lazy-seq
      (if (empty? queue)
        acc
        (let [current (peek queue)]
          (bft- 
            (cons current acc)
            (into (pop queue) (adj current)))))))
    [] (conj clojure.lang.PersistentQueue/EMPTY v)))

;; breadth-first search

; takes a goal predicate in addition to the adjacency function and the start vertex
(defn bfs [adj v goal]
  (filter goal (bft adj v)))

;; definition of the adjacency function for our use case

; Our tree is the state space, where vertices are paths (vectors of words)
; of length 1, length 2, etc., without cycles (but not necessarily shortest
; paths).
(defn adj [p]
  (let [w (last p)]
    (for [x (one-step w) :when (not (some #{x} p))]
      (conj p x))))

(defn at-goal? [goal path]
  (= (last path) goal))

;; main function

(defn doublets [word1 word2]
  (let [results (bfs adj [word1] (partial at-goal? word2))]
    (if (seq results)
      (first results)
      ; test wants empty vector for the case of no chain
      [])))
