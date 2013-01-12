(load-file "src/solutions/common-functions.clj")

(def consecutive-primes (for [x (range 5 1000004) :when (prime? x)] x)
  )

;; Extended Euclidean Algorithm
;; http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
(defn ext-gcd [a0 b0]
  (loop [x 0
         y 1
         lastx 1
         lasty 0
         q (int (/ a0 b0))
         a b0
         b (mod a0 b0)]
    (if (= b 0)
      x
      (recur (- lastx (* q x)) (- lasty (* q y)) x y (int (/ a b)) b (mod a b))
      )
    )
  )

(defn S [x y]
  (let [p10 (int (pow 10 (int (+ 1 (log10 x)))))]
    (+ x (* p10 (mod (* (- y x) (ext-gcd p10 y)) y)))
    )
  )
(defn euler-134 []
  (reduce + (for [x (range 1 (count consecutive-primes))]
    (S (nth consecutive-primes (- x 1)) (nth consecutive-primes x)))
          )
  )

;; Solution: 18613426663617118
;; Time: 128 seconds
