(ns aoc2024.tools
  (:require [clojure.string :as string]
            [clojure.string :as s]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn letters
  []
  (string/split alphabet #""))

(defn letter->idx
  ([]
   (letter->idx 0))
  ([starting-idx]
   (let [ls (letters)]
     (zipmap ls (iterate inc starting-idx)))))

(defn path->lines
  "Return a vector of string read from the file"
  [path]
  (-> path
      slurp
      string/split-lines))

(defn path->line
  "Return a string read from the file"
  [path]
  (-> path
      slurp
      string/trim))

(defn path->objects
  "Return a vector of string read from the file with objects separated
  by a blank line"
  [path]
  (-> path slurp (string/split #"\n{2}")))

(defn input-path
  "Return a path to input file for the current namespace"
  ([]
   (input-path false))
  ([test?]
   (let [day (-> *ns*
                 str
                 (string/split #"\.")
                 last)
         postfix (if test? ".test" "")]
     (format "resources/input/%s%s.txt" day postfix))))

(defn str->vals
  "Returna vector of values that has been separated by whitespace(s)"
  [s]
  (string/split s #"\s+"))

(defn str->vec
  "Return a vector of letters"
  [s]
  (string/split s #""))

(defn str->int
  "Parse string into an integer"
  [s]
  (Integer/parseInt s))

(defn str->long
  "Parse string into an Long"
  [s]
  (parse-long s))

(defn str->ints
  [s]
  (reduce
   (fn [acc v]
     (conj acc (str->int v)))
   []
   (string/split s #"\s+")))

(defn str->longs
  [s]
  (reduce
   (fn [acc v]
     (conj acc (parse-long v)))
   []
   (string/split s #"\s+")))

;; Vectors

(defn ->v
  [s]
  (into [] s))

;; Positions

(defn vec-indexed [items]
  (map-indexed vector items))

(defn positions
  "Return a mapping of a [x y] position to its value in the lines matrix.
  x is a column index, y is a row index."
  [lines]
  (into {}
        (for [[y line] (vec-indexed lines)
              [x c]    (vec-indexed line)]
          [[x y] c])))

;; Matrix ops

(defn matrix-indexed
  "Return a matrix (vector of vectors) with each element indexed
  as [[row-idx col-idx] elem], ..."
  [matrix]
  (reduce
   (fn [result [row-idx heights]]
     (conj
      result
      (reduce
       (fn [row [col-idx height]]
         (conj row [[row-idx col-idx] height]))
       []
       (map-indexed vector heights))))
   []
   (map-indexed vector matrix)))
