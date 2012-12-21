(defn exp [x n]
  (reduce * (repeat n x))
  )

(defn remove-index [coll n] (vec (concat (subvec coll 0 n) (subvec coll (+ 1 n)))))

