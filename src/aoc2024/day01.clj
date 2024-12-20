(ns aoc2024.day01
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn extract-ints
  [s]
  (reduce
   (fn [acc v]
     (let [[left right] (map t/str->int (s/split v #"\s+"))]
       (-> acc
           (update :l conj left)
           (update :r conj right))))
   {:l [] :r []}
   s))

(defn part1
  "Part 1: (part1)
  => 1320851
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [data (-> (t/input-path test?)
                 t/path->lines
                 extract-ints)
        left (-> data :l sort)
        right (-> data :r sort)
        pairs (map vector left right)]
    (reduce
     (fn [acc [l r]] (+ acc (abs (- l r))))
     0
     pairs)))

(defn part2
  "Part 2: (part 2)
  => 26859182
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [data (-> (t/input-path test?)
                 t/path->lines
                 extract-ints)
        left (-> data :l)
        freqs (-> data :r frequencies)]
    (reduce
     (fn [acc v]
       (+ acc (* v (get freqs v 0))))
     0
     left)))
