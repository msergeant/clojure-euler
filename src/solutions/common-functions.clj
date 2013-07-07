(use '[clojure.contrib.generic.math-functions])
(use '[clojure.math.numeric-tower :only [expt]])

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

(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
    []
    (let [reinsert (fn [table x prime]
                    (update-in table [(+ prime x)] conj prime))]
          (defn primes-step [table d]
             (if-let [factors (get table d)]
                (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                       (inc d))
                (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                                                                                                                  (inc d))))))
          (primes-step {} 2)))
