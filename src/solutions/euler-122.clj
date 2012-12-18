(defn euler-122 [limit]
  (let [multis (ref (vec (cons 0 (take (+ 1 200) (repeat 99)))))]
    ((fn node-calculation
      [path]
      (if (or (< limit (last path)) (< (nth @multis (last path)) (dec (count path))))
              nil
              ((fn [] 
                 (dosync (ref-set multis (assoc @multis (last path) (dec (count path)))))
                 (loop
                   [iterator (count path)
                    loop-path path]
                   (if (>= 0 iterator)
                     nil
                     (do
                       (node-calculation
                         (conj loop-path (+ (last loop-path) (nth loop-path (dec iterator)))))
                       (recur
                         (dec iterator) loop-path)
                         )
                       )
                     )
                   ))
                )
        ) [1])
    (reduce + (take (+ 1 limit) @multis))
    )
  )

;; Solution: 1582
;; Time: 1700 ms
