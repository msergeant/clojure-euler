(load-file "src/solutions/subset-functions.clj")

;;{11, 17, 20, 22, 23, 24}
;;{20, 31, 38, 39, 40, 42, 45}
(defn euler-103 []
  (let [guess [20, 31, 38, 39, 40, 42, 45]
        candidates (for
    [a (range 18 22)
     b (range 29 33)
     c (range 36 40)
     d (range 37 41)
     e (range 38 42)
     f (range 40 44)
     g (range 43 48)
     :when (and (< a b) (< b c) (< c d) (< d e) (< e f) (< f g))]
    (let [x [a b c d e f g]]
    (if (and (test-cond-i x) (test-cond-ii x))
      x
      nil))
          )]
    (loop
      [iterator 0
       min-sum 999999
       sum-string ""]
      (if (>= iterator (count candidates))
        sum-string
        (let [cand-sum (reduce + (nth candidates iterator))]
          (if (or (nil? (nth candidates iterator)) (< min-sum cand-sum))
            (recur (inc iterator) min-sum sum-string)
            (recur (inc iterator) cand-sum (reduce str (nth candidates iterator))))
          )
        )
    )
  )
)


;; Solution: 20313839404245
;; Time: 53 seconds
