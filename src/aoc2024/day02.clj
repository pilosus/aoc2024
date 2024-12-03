(ns aoc2024.day02
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn safe?
  [vs]
  (loop [items vs
         prev nil
         order nil
         result true]
    (if (empty? items)
      result
      (let [curr (first items)]
        (if (nil? prev)
          (recur (rest items) curr order result)
          (let [order' (compare curr prev)
                stop-cond (or (> (abs (- curr prev)) 3)
                              (and (not (nil? order)) (not (= order order'))))
                items' (if stop-cond nil (rest items))
                prev' curr
                result' (if stop-cond false result)]
            (recur items' prev' order' result')))))))

(defn safe-tolerated?
  [vs]
  (loop [head-items vs
         tail-items (list)]
    (if (safe? (into head-items (rest tail-items)))
      true
      (if (empty? head-items)
        false
        (recur (rest head-items) (conj tail-items (first head-items)))))))

(defn part1
  "Part 1: (part1)
  => 564
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [lines (-> (t/input-path test?)
                  t/path->lines)
        data (map t/str->ints lines)]
    (reduce
     (fn [acc v]
       (if (safe? v) (inc acc) acc))
     0
     data)))

(defn part2
  "Part 2: (part2)
  => 585, 600 - too low
  Test: (part2 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [lines (-> (t/input-path test?)
                  t/path->lines)
        data (map t/str->ints lines)]
    (reduce
     (fn [acc v]
       (if (safe-tolerated? v) (inc acc) acc))
     0
     data)))
