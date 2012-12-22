(load-file "src/solutions/subset-functions.clj")
(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(defn process-file [file-name line-func line-acc]
  (with-open [rdr (reader file-name)]
    (reduce line-func line-acc (line-seq rdr)))
  )

(defn process-line [acc line]
  (let [tst-list (map #(Integer/parseInt %) (vec (split line #",")))]
   (if (and (test-cond-i tst-list) (test-cond-ii tst-list))
     (+ acc (reduce + tst-list))
     acc
    )
    )
  )

(defn euler-105 []
  (process-file "src/solutions/sets.txt" process-line 0)
  )

;; Solution: 185484
;; Time: 69 seconds
