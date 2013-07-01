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
;; i > j
;; (i + j) mod 4 == 0
;; (3j - i) mod 4 == 0

(use '[clojure.contrib.generic.math-functions])

(defn meets? [i j]
  (if (and
        (> (* 3 j) i)
        (= (mod (+ i j) 4) 0)
        (= (mod (- (* 3 j) i) 4) 0)
        )
    true
    false)
  )

(defn count-solutions [n]
  (reduce +
          (map
            (fn [x]
              (if (= 0 (mod n x))
                (reduce + [(if (meets? x (/ n x)) 1 0) (if (and (not (= (/ n x) x)) (meets? (/ n x) x)) 1 0)])
                0
              )
            )
            (range 1 (+ 1 (sqrt n)))
          )
  )
)

(defn euler-135 []
  (reduce +
          (map
            (fn [x]
              (if (= 10 (count-solutions x))
                1
                0))
            (range 1 1000000))))


;; Solution: 4989
;; Time: 228 seconds
