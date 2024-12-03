(ns aoc2024.day03
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(def mul-regexp #"mul\((?<left>\d+)\,(?<right>\d+)\)")

(def mul-do-regexp #"mul\((?<left>\d+)\,(?<right>\d+)\)|do\(\)|don't\(\)")

(defn part1
  "Part 1: (part1)
  => 164730528
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [lines (-> (t/input-path test?)
                  t/path->lines)
        line (s/join lines)]
    (reduce
     (fn [acc [_ left right]]
       (let [result (* (t/str->int left) (t/str->int right))]
         (+ acc result)))
     0
     (re-seq mul-regexp line))))

(defn proc
  [s]
  (loop [items (re-seq mul-do-regexp s)
         skip? false
         acc 0]
    (if (empty? items)
      acc
      (let [[op l r] (first items)
            skip' (cond
                    (s/starts-with? op "don't(") true
                    (s/starts-with? op "do(") false
                    :else skip?)
            acc' (if (and (s/starts-with? op "mul(") (not skip?))
                   (+ acc (* (t/str->int l) (t/str->int r)))
                   acc)]

        (recur
         (rest items)
         skip'
         acc')))))

(defn part2
  "Part 2: (part2)
  => 70478672
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [lines (-> (t/input-path test?)
                  t/path->lines)
        line (s/join lines)]
    (proc line)))
