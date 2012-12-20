(defn exp [x n]
  (reduce * (repeat n x))
  )

(defn base-func
  [x]
  (exp x 3)
  )

(defn remove-index [coll n] (vec (concat (subvec coll 0 n) (subvec coll (+ 1 n)))))

(defn op [order n]
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

(defn euler-101
  )
