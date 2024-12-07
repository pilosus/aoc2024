(ns aoc2024.day05
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn not-contains?
  [actual excluded-by-rule]
  (empty?
   (clojure.set/intersection
    (set actual)
    (set excluded-by-rule))))

(defn check-item
  [item rules]
  (loop [before []
         after item]
    (if (empty? after)
      (let [mid-idx (quot (count item) 2)]
        (get item mid-idx))
      (let [n (first after)
            {:keys [a b]} (get rules n)
            comply? (and (not-contains? before a)
                         (not-contains? (rest after) b))]
        (if comply?
          (recur (conj before n) (rest after))
          0)))))

(defn check-order
  [data rules]
  (reduce
   (fn [acc item]
     (+ acc (check-item item rules)))
   0
   data))

(defn get-input
  [test?]
  (let [[rule-s data-s] (->> (t/input-path test?)
                             t/path->objects)
        rules  (reduce
                (fn [acc v]
                  (let [[l r] (map t/str->int (s/split v #"\|"))]
                    (-> acc
                        (update-in [l :a] conj r)
                        (update-in [r :b] conj l))))
                {}
                (s/split rule-s #"\n"))
        data (reduce
              (fn [acc v]
                (let [item (->> (s/split v #",")
                                (mapv t/str->int))]
                  (conj acc item)))
              []
              (s/split data-s #"\n"))]
    [rules data]))

(defn part1
  "Part 1: (part1)
  => 4872
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [[rules data] (get-input test?)]
    (check-order data rules)))
