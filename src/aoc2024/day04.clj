(ns aoc2024.day04
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(defn get-word
  [positions [x y] [dx dy]]
  (str (get positions [x y])
       (get positions [(+ x dx) (+ y dy)])
       (get positions [(+ x dx dx) (+ y dy dy)])
       (get positions [(+ x dx dx dx) (+ y dy dy dy)])))

(defn count-words
  [positions]
  (count
   (for [pos (keys positions)
         dx [-1 0 1]
         dy [-1 0 1]
         :when (not= 0 dx dy)  ;; otherwise matches the letter itself
         :let [word (get-word positions pos [dx dy])]
         :when (= word "XMAS")]
     1)))

(defn part1
  "Part 1: (part1)
  => 2562
  Test: (part1 :test? true)"
  [& {:keys [test?] :or {test? false}}]
  (let [positions (->> (t/input-path test?)
                       t/path->lines
                       t/positions)]
    (count-words positions)))
