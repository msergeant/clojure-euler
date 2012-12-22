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
