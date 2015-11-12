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


(#(str "Hello, " % "!"))


[6 7 8]

'( 6 7)

(comp first reverse)

(comp second reverse)

 (fn [collection index]
 (if (zero? index)
   (first collection)
   (recur (rest collection) (- index 1)))


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



(defn fib [n] ((loop [s 0
                      res [0 1]]
                 (if (> s (- n 2))
                   res
                   (recur (inc s) (conj res (+ (nth res s) (nth res (+ s 1) ))))
                   )
                 )))
