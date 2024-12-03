(ns aoc2024.day03
  (:require
   [aoc2024.tools :as t]
   [clojure.string :as s]))

(def mul-regexp #"mul\((?<left>\d+)\,(?<right>\d+)\)")

(defn part1
  "Part 1: (part1)
  =>
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
