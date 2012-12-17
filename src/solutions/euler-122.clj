(defn euler-122 [limit]
  (let [multis (ref (zipmap (range 1 (+ 1 limit)) (repeat 99)))]
    ;;Actual calculation will go in here
    (reduce + (vals @multis))
  )
)
