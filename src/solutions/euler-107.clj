(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(defn process-file [file-name line-func line-acc]
  (with-open [rdr (reader file-name)]
    (reduce line-func line-acc (line-seq rdr)))
  )

(defn process-line [acc line]
  (let [new-vec (map #(if (not= "-" %) 
                        (Integer/parseInt %)
                        nil) (vec (split line #",")))]
    (assoc acc (count acc) new-vec)
    )
  )

(defn zero-if-nil [x] (if (nil? x) 0 x))
(defn add-nils [x, y] (+ (zero-if-nil x) (zero-if-nil y)))
(defn shortest-next [covered not-covered graph]
  (let [edges (for [begin covered
                    end not-covered
                    :when (not (nil? (nth (nth graph end) begin)))]
                (vector [begin end] (nth (nth graph end) begin)))]
        (loop [minimum (last (first edges))
               min-edge (first (last edges))
               index 0]
          (if (>= index (count edges))
            min-edge
            (if (< minimum (last (nth edges index)))
              (recur minimum min-edge (inc index))
              (recur (last (nth edges index)) (first (nth edges index)) (inc index))
              )
            )
          )
    )
  )

(defn euler-107 
  "Solves euler projet problem 107 using Prim's algorithm"
  []
  (let [the-matrix (process-file "src/solutions/network.txt" process-line [])
        original-weight (/ (reduce + (map #(reduce add-nils %) the-matrix)) 2)
        width (count (nth the-matrix 0))]
    (- original-weight
      (loop [covered #{0}
             not-covered (set (range 1 width))
             total 0 
             ]
        (if (empty? not-covered)
          total
          (let [next-edge (shortest-next covered not-covered the-matrix)
                now-covered (last next-edge)]
            (recur (conj covered now-covered) 
                   (disj not-covered now-covered) 
                   (+ total (nth (nth the-matrix (last next-edge)) (first next-edge))))
            )
          )
        ))
    )
  )

;; Solution: 259679
;; Time: 148 msec
