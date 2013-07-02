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

(defn euler-135 [n]
  (loop
    [i 1
     values (vec (replicate (+ 1 n) 0))
     ]
    (if (> i n)
      (reduce (fn [x y] (if (= y 10) (+ 1 x) x)) 0 values)
      (recur
        (inc i)
        (loop
            [j 1
             acc values]
              (if (> i (/ n j))
                acc
                (let [q (* i j)
                      new-acc (if (meets? i j)
                                  (assoc acc q (+ (nth acc q) 1))
                                  acc)]
                  (recur (inc j) new-acc)))))))
)


;; Solution: 4989
;; Time: 13.4 seconds
