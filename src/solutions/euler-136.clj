;; (x + 2y)^2 - (x + y)^2 - x^2 = n
;; x^2 + 4xy +4y^2 - x^2 - 2xy - y^2 - x^2 = n
;; -x^2 + 2xy + 3y^2 = n
;;
;; (3y - x)(y + x) = n
;; i * j = n
;; i = 3y -x
;; j = y + x
;;
;; x = 3y - i
;; x = j - y
;; 3y - i = j - y
;; y = (j + i) / 4
;;
;; y = j - x
;; y = (i + x)/3
;; j - x = (i+x)/3
;; 3j - 3x = i + x
;; x = (3j - i) / 4
;;
;; Conditions
;; i * j = n
;; (i + j) mod 4 == 0
;; (3j - i) mod 4 == 0
;;
;; i * j = 2^u*V
;;
;; possible solutions when V is prime:
;; i = 1, j = V
;; i = 2, j = 2 * V
;; i = 4, j = 4 * V

;; using prime files 1-4 downloaded from http://primes.utm.edu/lists/small/millions/
(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(def en 50000000)
(defn meets? [i j n]
  (if (and
        (< j n)
        (>= n (* i j))
        (> (* 3 j) i)
        (= (mod (+ i j) 4) 0)
        (= (mod (- (* 3 j) i) 4) 0)
        )
    1
    0)
  )

(defn process-prime [x n]
  (reduce + [(meets? 1 x n) (meets? 2 (* 2 x) n) (meets? 4 (* 4 x) n)] )
)

(defn parse-lines [acc tst]
  (let [tst-list (map #(Integer/parseInt %) (vec (split tst #"\s")))]
    (+ acc (reduce + (map (fn [x] (process-prime x en)) tst-list)))
    )
)

(defn process-file [file-name]
  (with-open [rdr (reader file-name)]
    (reduce parse-lines 0 (line-seq rdr)))
  )

(defn euler-136 [n]
  (+ 1
     (process-file "src/solutions/primes1.txt")
     (process-file "src/solutions/primes2.txt")
     (process-file "src/solutions/primes3.txt")
     (process-file "src/solutions/primes4.txt")
  )
)

(euler-136 en)

;; Solution: 2544559
;; Time: 8.7 seconds
