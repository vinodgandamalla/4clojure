(ns four-clojure.core
  (:require  [clojure.string :as cstr]))

4

"HELLO WORLD"

:a :b :c

'(1 2 3 4)

:a :b :c

[1 2 3 4]

#{:a :b :c :d}

2

20

[:b 2]

3

'(20 30 40)


8

#(* % 2)

(fn multiply-by-2 [x] (* 2 x))

[6 7 8]

'( 6 7)

;; last element
(comp first reverse)

;;penultimate element
(comp second reverse)

;;nth element
(defn yenth [collection index]
  (if (zero? index)
    (first collection)
    (recur (rest collection) (- index 1))))

(defn yenthelement [collection index] (first (drop index collection) ))

;; count a sequence
(defn count-seq [input]
  (loop [c (seq input)
         ans 0]
    (if (empty? c)
      ans
      (recur (rest c) (inc ans)))))


;;reverse a sequnce
(defn rev [seq]
  (into '() seq))

(defn rv [coll]
  (loop [c  coll
         ans '()]
    (if (empty? c)
      ans
      (recur (rest c) (conj ans (first c))))))


;;sum it all up

(defn sum [coll]
  (loop [c coll
         ans 0]
    (if (empty? c)
      ans
      (recur (next c)(+ ans (first c))))))



;;sum it all up

reduce +


;;palindrome detector

(defn pal [x]
  (if (coll? x)
    (= x (reverse x))
    (= x ( cstr/reverse x))))

;;flatten a sequence
(fn flt [coll]
  (let [l (first coll) r (next coll)]
    (concat
     (if (sequential? l)
       (flt l)
       [l])
     (when (sequential? r)
       (flt r)))))

;;get the caps
(defn only-higher [s] (apply str (re-seq #"[A-Z]+" s)))

;;compress a sequence

(defn compress [coll] (map first (partition-by identity coll)))


;;factorial
(defn factorial [n] (loop [n n
                           ans 1]
                      (if (= n 0)
                        ans
                        (recur (dec n) (* ans n)))))


;;duplicate a sequence
mapcat (fn [x] (vector x x))

;;Replicate a sequence
(defn replicateseq [s n] (mapcat (fn [i] (repeatedly n #(identity i)))s))

;;range
(defn myrange [x y]
  (take (- y x) (iterate inc x) ))

7

;;[x 7 y 3 z 1]


"ABC"

;;number of c's in string
(defn numoftimes [coll]
  (count (re-seq #"C|c" coll)))

;;maximum value

(defn max2 [x y]
  (if (> x y)
    x
    y))
(defn maxn [& z]
  (reduce max2 z))

;;interleave
(defn inter [seq1 seq2]
  (if(or (empty? seq1) (empty? seq2))
    []
    (concat [(first seq1) (first seq2)]
            (inter (rest seq1) (rest seq2)))))

;; interpose a sequence
(defn interp [x coll] (rest (mapcat #(list x %) coll)))


;;drop every nth term
#(apply concat (partition-all (dec %2) %2 %1))


;;reverse interleave
#(apply map list (partition %2 %1))



;;rotate sequence
(defn rotate [dir s]
  (let [n (mod dir (count s))]
    (concat (drop n s) (take n s))))


;;filter
(defn myfilter [f coll]
  (into []
        (apply concat
               (map
                (fn [x]
                  (if (f x) [x] []))
                coll))))

;;fibonacci
(defn fibonacci [x]
  (take x
        (( fn fib [a b]
          (cons a (lazy-seq( fib b (+ a b)))))
         1 1)))


;;range
(defn myrange [x y] (take (- y x) (iterate inc x)))

;;flatten a sequence
(defn flat [c]
  (loop [coll c
         ans []]
    (if (empty? coll)
      ans
      (recur (next coll) (if (coll? (first coll))
                           (into ans (flat (first coll) ))
                           (conj ans (first coll)))))))


;;split by type

#(vals (group-by type %))


;;split a sequence

(defn split [n s] (vector (take n s) (drop n s)))


;;flipping out

(defn flip [f] (fn [arg1 arg2 & args] (apply f arg2 arg1 args)))


;;odd numbers

filter (fn [x] (odd? x))

;;prime numbers

(defn prime? [n]
  (if (even? n) false
      (let [root (num (int (Math/sqrt n)))]
        (loop [i 3]
          (if (> i root) true
              (if (zero? (mod n i)) false
                  (recur (+ i 2))))))))

(defn n-primes [n]
  (loop [num 2 p []]
    (if (>= (count p) n) p
        (recur (inc num) (if (prime? num) (concat p [num]) p)))))

;;perfect numbers

(defn divisors [n]
  (filter (fn [x]  (= (mod n x) 0))
          (range 1 (inc (/ n 2)))))

(defn isperfect? [n] (= n (reduce +  (filter (fn [x]  (= (mod n x) 0))
                                             (range 1 (inc (/ n 2)))) )))

(defn perfectnumbers [c] (loop [c c
                                ans []]
                           (if (< c 2)
                             ans
                             (recur (dec c) (if (isperfect? c)
                                              (conj ans c) ans)))))

;;gcd
(defn gcd [x y]
  (let [rem (mod x y)]
    (if (= 0 rem)
      y
      (gcd y rem))))

;;cartesian product
(defn cartesianproduct [s1 s2]
  (loop [coll1 s1
         res #{}]
    (if (empty? coll1)
      res
      (recur(rest coll1) (into res (map #(vector (first coll1) %) s2))))))


;;LCM

(defn lcmof2numbers [a b] (/ (* a b) (gcd a b)))


(defn lcmofnnumbers [y] (reduce lcmof2numbers y))

(defn lcmofnratios [z] (let [lcd (lcmofnnumbers (map denominator z))]
                         (/ (reduce #(* %1 %2) (map #(* lcd %) z)) lcd)))

(defn lcm[x]
  (let [structure ( group-by type  x)
        ratios (get structure clojure.lang.Ratio)
        numbers (get structure java.lang.Long)]
    (lcmof2numbers (lcmofnnumbers numbers) (lcmofnratios ratios))))


;;Symmetric Difference

#(clojure.set/difference (clojure.set/union %1 %2) (clojure.set/intersection %1 %2))
