(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

;; helper functions

(defn rotate [n s]
  (let [l (count s)
        k (mod n l)]
    (concat (drop k s) (take k s))))

(defn encode-single [key letter]
  (let [i (- (int letter) (int \a))
        j (- (int key) (int \a))]
    ; look up i-th row, j-th column, where indices are 0-based
    (nth (rotate i alphabet) j)))

(defn decode-single [key letter]
  ; given the key (j-th column), go down each row until we find the letter
  ; the first letter of that row is the decoded letter
  (let [j (- (int key) (int \a))]
    (loop [i 0]
      (if (= letter (nth (rotate i alphabet) j))
        (first (rotate i alphabet))
        (recur (inc i))))))

(defn decipher-single [code plain]
  ; look at the i-th row (unencoded letter (plain)),
  ; find the index of the encoded letter (code), i.e., the column,
  ; and that gives us the corresponding letter for the cipher key
  (let [i (- (int plain) (int \a))
        ; build a map of letters to indices so we can use the letter to look up the index
        m (into {} (map-indexed (fn [idx itm] [itm idx]) (rotate i alphabet)))]
    (nth alphabet (m code))))

; function to find the repeating subsequence
(defn repeating-subseq [s]
  ; keep shifting the sequence left, and compare with the original truncated
  (loop [n 1]
    (let [l (count s)
          s1 (take (- l n) s)
          s2 (drop n s)]
      (if (= s1 s2)
        (take n s)
        (recur (inc n))))))

;; main functions

(defn encode [keyword message]
  (let [l (count message)
        keys (take l (cycle keyword))]
    (apply str (map encode-single keys message))))

(defn decode [keyword message]
  (let [l (count message)
        keys (take l (cycle keyword))]
    (apply str (map decode-single keys message))))

(defn decipher [cipher message]
  (apply str 
    (repeating-subseq
      (map decipher-single cipher message))))

