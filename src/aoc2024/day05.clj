(ns aoc2024.day05
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn overlap
  [actual excluded-by-rule]
  (clojure.set/intersection
   (set actual)
   (set excluded-by-rule)))

(defn not-contains?
  [actual excluded-by-rule]
  (empty?
   (overlap actual excluded-by-rule)))

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

(defn b->a
  [before after overlaping]
  (let [before' (into [] (remove overlaping before))
        o' (into [] overlaping)
        after' (into o' after)]
    [before' after']))

(defn a->b
  [before after overlaping]
  (let [after' (into [] (remove overlaping after))
        o' (into [] overlaping)
        before' (into before o')]
    [before' after']))

(defn order-iteration
  [n before after rules]
  (let [{:keys [a b]} (get rules n)
        ba (overlap before a)
        ab (overlap after b)]
    (cond
      (seq ba) (b->a before after ba)
      (seq ab) (a->b before after ab)
      :else [before after])))

(defn correct-item
  [item rules]
  (loop [before []
         after item]
    (let [check-result (check-item (into before after) rules)]
      (if (not= 0 check-result)
        check-result
        (let [n (first after)
              [before' after'] (order-iteration n before (rest after) rules)]
          (if (empty? after)
            ;; start from the beginning
            (recur [] before)
            ;; adjusted before/after
            (recur (conj before' n) after')))))))

(defn calc-corrected
  [data rules]
  (reduce
   (fn [acc item]
     (let [r (check-item item rules)]
       (if (= r 0)
         (+ acc (correct-item item rules))
         acc)))
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

(defn part2
  "Part 2: (part2)
  => 5564
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [[rules data] (get-input test?)]
    (calc-corrected data rules)))
