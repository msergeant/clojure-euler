(defn euler-109 []
  (let [unos (conj (range 1 21) 25)
        dos (map #(* 2 %) (conj (range 1 21) 25))
        tres (map #(* 3 %) (range 1 21))
        all-spots (concat unos dos tres)]
    (+
      ;;count 1 dart checkouts
      (count dos)
      ;;count 2 dart checkouts
      (reduce + (for [i all-spots
                      j dos
                      :when (< (+ i j) 100)]
        1))
      ;;count 3 dart checkouts
      (reduce + (for [i (range 0 (count all-spots))
                      j (range 0 (count all-spots))
                      k dos
                       :when (and (< (+ i j k) 100) (<= i j))]
                     1))
      )
    )
  )
