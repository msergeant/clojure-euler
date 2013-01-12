(load-file "src/solutions/common-functions.clj")

(def consecutive-primes (for [x (range 6 1000000) :when (prime? x)] x)
  )
(defn S [x y]
  (let [p10 (int (pow 10 (int (+ 1 (log10 x)))))]
    (reduce (fn [a b] (if (= x (mod (* y b) p10)) (* a (* y b)) (* a 1))) 1 (range 1 p10)
            )
    )
  )
(println (S 19 23))
(defn euler-134 []
  )
