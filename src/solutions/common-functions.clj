(use '[clojure.contrib.generic.math-functions])

(defn log10 [n] (/ (clojure.contrib.generic.math-functions/log n) (clojure.contrib.generic.math-functions/log 10)))

(defn remove-index [coll n] (vec (concat (subvec coll 0 n) (subvec coll (+ 1 n)))))

(defn square [n] (* n n))

(defn divides? [a b]
          (= (mod b a) 0))
(defn find-divisor [n test-divisor]
          (cond (> (square test-divisor) n) n
                (divides? test-divisor n) test-divisor
                :else (find-divisor n (+ test-divisor 1))))
(defn smallest-divisor [n]
          (find-divisor n 2))
(defn prime? [n]
          (= n (smallest-divisor n)))
