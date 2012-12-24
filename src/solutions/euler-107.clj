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

(defn euler-107 []
  (let [the-matrix (process-file "src/solutions/network_example.txt" process-line [])
        original-weight (/ (reduce + (map #(reduce add-nils %) the-matrix)) 2)
        width (count (nth the-matrix 0))]
    (println original-weight)
    (println width)
    )
  )
