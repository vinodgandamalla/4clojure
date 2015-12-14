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
                           (into ans (flat (first coll)))
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

(defn is-prime? [num]
  (if (< num 2)
    false
    (empty? (filter (fn [x]
                      (if (= 0 (mod num x)) true false )) (range 2 num)))))

(defn prime-seq [n]
  "This function returns the given size of prime numbers"
  (vec (take n (filter is-prime? (iterate inc 0)))))


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


;;partition a sequence

(defn partseq [x y]
  (loop [ans2 []
         ans1 y]
    (if (empty? ans1)
      ans2
      (recur (if (= x (count (take x ans1)))
               (conj ans2 (take x ans1)) ans2) (drop x ans1)))))


;;frequencies

(defn freq [a]
  (into {} (map (fn [[x y]] [x (count y)]) (group-by identity a))))


;;function composition

(defn mycomp [& fs] (reduce (fn [f g] #(f (apply g %&))) fs))

;;juxtaposition

(defn juxtaposition [& fs]
  (fn [& as] (map #(apply % as) fs)))


;;map construction

#(apply hash-map (interleave %1 %2))


;;unique elements

(defn uniq [x]
  (vals (loop [coll x
               newmap {}]
          (if (empty? coll)
            newmap
            (recur (next coll) (if (contains? newmap (first coll))
                                 newmap
                                 (assoc newmap (first coll) (first coll))))))))


;;sorting a collection using selection sort algorithm

(defn min-two [x y]
  (if (< x y) x y))
(defn min-idx [coll]
  (.indexOf coll (reduce min-two coll)))


(defn swaping [ele c]
  (let [coll (vec c)
        idx (min-idx coll)]
    (concat (subvec coll 0 idx ) [ele] (subvec coll (inc idx)))))

(defn selection-sort1 [coll]
  (loop [result []
         c coll]
    (if (empty? c)
      result
      (recur (conj result (nth c (min-idx c)))
             (next (swaping (first c) c))))))


;; find an element
(defn myfind [coll ele]

  (loop [c (seq coll)
         v false]
    (if(or (= v true) (empty? c))
      v
      (recur (next c) (if (= (first c) ele)
                        true
                        false)))))

(defn myfind1 [ele coll]
  (loop [c coll]
    (if (empty? c)
      false
      (if (= (first c) ele)
        true
        (recur (next c) )))))


;; intersection of collections



(defn myintersection [coll1 coll2]
  (loop [c1 (seq coll1)
         res []]
    (if(empty? c1)
      res
      (recur (next c1) (if(myfind coll2 (first c1))
                         (conj res (first c1))
                         res)))))


;;uniq1

(defn uniq1 [coll]
  (loop [coll coll
         res []]
    (if(empty? coll)
      res
      (recur (next coll) (if(= (first coll) (last res))
                           res
                           (conj res (first coll)))))))



;;partition1

(defn partition1 [coll]
  (split-with (partial = (first coll))coll))


;;group1

(defn group1 [coll]
  (loop [coll coll
         res []]
    (if(empty? coll)
      res
      (recur (second (split-with (partial = (first coll)) coll)) (conj res (first (split-with (partial = (first coll)) coll)))))))



;;compress1

(defn compress1 [coll1]
  (map list (uniq1 coll1) (map count (group1 coll1))))


;;map using recursion
(defn mymap[func coll]
  (if (nil? (next coll))
    (cons (func (first coll)) '())
    (cons (func (first coll)) (mymap func (rest coll)))))



;; filter using recursion
(defn filter11 [pred coll]
  (if(nil? (next coll))
    (if (pred (first coll))
      (cons (first coll) '())
      '())
    (if(pred (first coll))
      (cons (first coll) (filter11 pred (rest coll)))
      (filter11 pred (rest coll)))))



;; every? using recursion


(defn myevery [pred coll]
  (if(nil? (next coll))
    (pred (first coll))
    (and (pred (first coll)) (myevery pred (next coll)))))


;;any using recursion

(defn myany [pred coll]
  (if(nil? (next coll))
    (pred (first coll))
    (or (pred (first coll)) (myany pred (next coll)))))



;; duplicate the sequence

(defn duplica [coll]
  (if (nil? (next coll))
    (cons (first coll) (cons (first coll) '()))
    (cons (first coll) (cons (first coll) (duplica (rest coll))))))


;;return last element of a collection


(defn returnlastelement [coll]
  (if(nil? (next coll))
    (first coll)
    (returnlastelement (next coll))))



;;return penultimate element of a collection

(defn return_penultimate_element [coll]
  (if(nil? (next (next coll)))
    (first coll)
    (return_penultimate_element (next coll))))



;; reverse of a collection using recursion

(defn reverc [coll]
  (let [c (seq coll)]
    (if c
      (conj (reverc (rest c)) (first c))
      [])))





;;partitionz

(defn partitionz [pred coll]
  (list (filter pred coll) (remove pred coll)))



;;group the sorted collection

(defn grouppp [coll]
  (if (empty? coll)
    '()
    (cons (first (partitionz (fn [x] (= x (first coll))) coll)) (grouppp (second (partitionz (fn [x] (= x (first coll))) coll))))))


;;compress the sorted collection

(defn compresz [coll]
  (map (fn [x] (list (first x) (count x))) (grouppp coll)))



;;split-at

(defn mysplit-at [k coll]
  (loop [acc []
         n k
         left coll]
    (if (zero? n)
      [acc left]
      (recur (conj acc (first left))
             (dec n)
             (rest left)))))
