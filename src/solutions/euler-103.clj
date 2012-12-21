(load-file "src/solutions/common-functions.clj")

(defn sum-subset [coll active-bits]
  (reduce + (map-indexed (fn [idx itm] (if (= 0 (bit-and (exp 2 idx) active-bits))
                                         0
                                         itm)) coll))
  )

(defn bit-count
  ([x]
   (bit-count x 0))
  ([x c]
    (if (zero? x)
      c
     (recur (bit-and x (dec x)) (inc c))))
 )

(defn test-cond-i [coll]
  (reduce (fn [x y] (and x y)) 
          (for [ x (range 1 (exp 2 (count coll)))
                 y (range 1 (exp 2 (count coll)))
                 :when (and (> y x) (= 0 (bit-and x y)))]
                 (not= (sum-subset coll x) (sum-subset coll y))
            )
    )
  )

(defn test-cond-ii [coll]
  (reduce (fn [x y] (and x y)) 
          (for [ x (range 1 (exp 2 (count coll)))
                 y (range 1 (exp 2 (count coll)))
                 :when (and (> y x) (= 0 (bit-and x y)) (not= (bit-count x) (bit-count y)))]
            (if (> (bit-count x) (bit-count y))
              (> (sum-subset coll x) (sum-subset coll y))
              (< (sum-subset coll x) (sum-subset coll y))
            )
    )
  )
)

;;{11, 17, 20, 22, 23, 24}
;;{22, 33, 39, 42, 44, 45, 46}
(defn euler-103 []
  (let [candidates (for
    [a (range 20 24)
     b (range 31 35)
     c (range 37 41)
     d (range 40 44)
     e (range 42 46)
     f (range 43 47)
     g (range 44 48)
     :when (and (< a b) (< b c) (< c d) (< d e) (< e f) (< f g))]
    (let [x [a b c d e f]]
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
