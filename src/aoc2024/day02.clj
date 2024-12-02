(ns aoc2024.day02
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]
   [clojure.string :as str]))

(defn safe?
  [vs]
  (loop [items vs
         idx 0
         prev nil
         order 0
         result true]
    (if (empty? items)
      result
      (let [curr (first items)]
        (cond
          (= idx 0)
          (recur (rest items) (inc idx) curr order result)
          (= idx 1)
          (let [order' (compare curr prev)
                stop-cond (or (= order' 0)
                              (> (abs (- curr prev)) 3))
                items' (if stop-cond nil (rest items))
                result' (if stop-cond false result)]
            (recur items' (inc idx) curr order' result'))
          :else
          (let [order' (compare curr prev)
                stop-cond (or (= order' 0)
                              (> (abs (- curr prev)) 3)
                              (not (= order' order)))
                items' (if stop-cond nil (rest items))
                result' (if stop-cond false result)]
            (recur items' (inc idx) curr order' result')))))))

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
