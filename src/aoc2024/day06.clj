(ns aoc2024.day06
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn get-data
  [test]
  (let [m (->> (t/input-path test)
               t/path->lines
               (map t/str->vec)
               (into []))
        grid (t/positions m)
        [start _] (first (filter (fn [[k v]] (= v "^")) grid))]
    {:grid grid
     :start start
     :max-x (dec (count (first m)))
     :max-y (dec (count m))}))

(defn out?
  [[x y] max-x max-y]
  (or (< x 0)
      (> x max-x)
      (< y 0)
      (> y max-y)))

(defn move
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn obstacle?
  [pos grid]
  (= (get grid pos) "#"))

(def deltas
  {:u [0 -1]
   :r [1 0]
   :d [0 1]
   :l [-1 0]})

(defn turn
  [delta]
  (cond
    (= delta (:u deltas)) (:r deltas)
    (= delta (:r deltas)) (:d deltas)
    (= delta (:d deltas)) (:l deltas)
    (= delta (:l deltas)) (:u deltas)))

(defn count-steps
  [{:keys [grid start max-x max-y]}]
  (loop [pos start
         delta (:u deltas)
         steps #{}]
    (let [next-pos (move pos delta)]
      (if (out? next-pos max-x max-y)
        (count (conj steps pos))
        (if (obstacle? next-pos grid)
          ;; avoid obstacle by turning
          (recur pos (turn delta) steps)
          ;; step forward
          (recur next-pos delta (conj steps pos)))))))

(defn part1
  "Part 1: (part1)
  => 4454 Elapsed time: 9.66775 msecs
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [data (get-data test?)]
    (count-steps data)))
