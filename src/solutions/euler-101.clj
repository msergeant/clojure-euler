(load-file "src/solutions/common-functions.clj")

(defn base-func
  [x]
  (loop
    [iterator 0
     sum 0]
    (if (> iterator 10)
      sum
      (recur (inc iterator) (+ sum (* (exp -1 iterator) (exp x iterator))))
      )
  )
  )

(defn op [order n]
  "Optimum Polynomial using Lagrange interpolation"
  (let [x (vec (range 1 (+ 1 order)))
        y (map base-func x)]
    (loop
      [iterator 0
       sum 0]
      (if (= iterator order)
        sum
        (recur (inc iterator) (+ sum (* (nth y iterator)
                                         (/ (reduce * (map (fn [z] (- n z)) (remove-index x iterator)))
                                            (reduce * (map (fn [z] (- (nth x iterator) z)) (remove-index x iterator))))
                                        )
                                 )
               )
        )
      )
    )
  )

(defn euler-101 []
  (loop
    [order 10
     sum 0]
    (if (= order 0)
      sum
      (recur (dec order) (+ sum (op order (+ 1 order))))
      )
    )
  )

;; Solution: 37076114526
;; Time: 40 ms
