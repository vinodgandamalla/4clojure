(ns four-clojure.core)

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

(comp first reverse)

(comp second reverse)

(fn [collection index]
  (if (zero? index)
    (first collection)
    (recur (rest collection) (- index 1))))


  (defn count-seq [input]
    (let [coll (seq input)]
      (loop [c coll
             ans 0]
        (if (empty? c)
          ans
          (recur (next c) (inc ans))))))


  (defn rev [seq]
    (into '() seq))

  (fn [coll]
    (loop [c coll
           ans 0]
      (if (empty? c)
        ans
        (recur (next c)(+ ans (first c))))))



  ;;(defn fib [n] ((loop [s 0
  ;;res [0 1]]
  ;;(if (> s (- n 2))
  ;;res
  ;;(recur (inc s) (conj res (+ (nth res s) (nth res (+ s 1) ))))
  ;;)
 ;; )))


(defn palindrome [coll] (if (coll? coll)
                          (= coll (reverse coll))
                          (= coll (apply str (reverse coll)))
                          ))

(fn flt [coll]
  (let [l (first coll) r (next coll)]
    (concat
     (if (sequential? l)
       (flt l)
       [l])
     (when (sequential? r)
       (flt r)))))

(fn only-lower [s] (apply str (re-seq #"[A-Z]+" s)))


(fn [coll] (map first (partition-by identity coll)))



(defn factorial [n] (loop [n n
                           ans 1]
                      (if (= n 0)
                        ans
                        (recur (dec n) (* ans n) )
                        )
                      ))


mapcat (fn [x] (vector x x))


(defn replicateseq [s n] (mapcat (fn [i] (repeatedly n #(identity i)))s))

(fn [x y]
  (take (- y x) (iterate inc x) ))


7

;;[x 7 y 3 z 1]


"ABC"

(defn numoftimes [coll]
  (count (re-seq #"C|c" coll)))


(defn maximumvalue [& x] (reduce #(if (> %1 %2) %1 %2) x))


(defn inter [seq1 seq2]
  (if(or (empty? seq1) (empty? seq2))
    []
    (concat [(first seq1) (first seq2)]
            (inter (rest seq1) (rest seq2)))))
