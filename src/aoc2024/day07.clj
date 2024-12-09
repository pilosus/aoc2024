(ns aoc2024.day07
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]
   [clojure.math.combinatorics :as c]))

(defn ||
  [a b]
  (parse-long (str a b)))

(def OPS1 [+ *])
(def OPS2 [|| + *])

(defn get-data
  "Get input data and parse it"
  [test]
  (let [lines (->> (t/input-path test)
                   t/path->lines)]
    (reduce
     (fn [acc v]
       (let [[r nums] (s/split v #": ")
             item [(t/str->long r) (t/str->longs nums)]]
         (conj acc item)))
     []
     lines)))

(defn eval-expr
  "Given a list of numbers and a list operators evaluate an expression
  as if precedence rule do not exist"
  [numbers operators]
  (reduce
   (fn [acc [n op]]
     (op acc n))
   0
   (map vector numbers operators)))

(defn get-ops
  "Get a list of possible operator permutations.
  Each permutations is prepended with the + operator
  to be applied to the starting value inside reduce function"
  [operators numbers]
  (map
   ;; prepend with + to be applied to the first iteration
   (fn [v] (conj v +))
   (c/selections operators (dec (count numbers)))))

(defn match?
  "Return true if given numbers can be arranged
  with possible operators combinations into expressions
  that evaluate to expected value"
  [expected numbers operators]
  (loop [ops (get-ops operators numbers)
         found? false]
    (if (empty? ops)
      found?
      (if (= expected (eval-expr numbers (first ops)))
        (recur nil true)
        (recur (rest ops) found?)))))

(defn count-matches
  [data operators]
  (reduce
   (fn [acc [expected numbers]]
     (if (match? expected numbers operators)
       (+ acc expected)
       acc))
   0
   data))

(defn part1
  "Part 1: (part1)
  => 2501605301465 Elapsed time: 277.039375 msecs
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [data (get-data test?)]
    (count-matches data OPS1)))

(defn part2
  "Part 2: (part2)
  => 44841372855953 Elapsed time: 20141.038375 msecs
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [data (get-data test?)]
    (count-matches data OPS2)))
