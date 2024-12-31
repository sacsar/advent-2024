(ns advent24.dayfour
  (:require [advent24.core :as core]
            [clojure.string :as str]))

(def directions [:n :ne :e :se :s :sw :w :nw])

(defn parse-line [line]
  (str/split line #""))

(defn has-value? [v m i j num-row num-col]
  (cond
    (>= i num-row) false
    (>= j num-col) false
    (< i 0) false
    (< j 0) false
    :else (= v (get-in m [i j]))))

(defn move [dir [i j]]
  (case dir
    :n [(- i 1) j]
    :ne [(- i 1) (+ j 1)]
    :e [i (+ j 1)]
    :se [(+ i 1) (+ j 1)]
    :s [(+ i 1) j]
    :sw [(+ i 1) (- j 1)]
    :w [i (- j 1)]
    :nw [(- i 1) (- j 1)]))

;; check if the matrix has XMAS starting from i,j moving in direction dir
(defn has-xmas? [m i j num-row num-col dir]
  (let [locations (drop 1 (iterate (partial move dir) [i j]))
        location-letters (mapv vector ["M" "A" "S"] locations)
        check-value (fn [[v  [r c]]] (has-value? v m r c num-row num-col))
        value-match (map check-value location-letters)]
    (reduce #(and %1 %2) true value-match)))

(defn count-xmas [m i j num-row num-col]
  (let [v (get-in m [i j])]
    (case v
      "X" (count (filter (partial has-xmas? m i j num-row num-col) directions))
    ;; default
      0)))

(defn part-one [m]
  ;; iterate over each item in m and count the number of XMAS that start there
  (let [num-row (count m)
        num-col (count (first m))]
    (reduce + 0 (for [i (range num-row)
                      j (range num-col)]
                  (count-xmas m i j num-row num-col)))))

(defn -main [path]
  (let [m (core/load-input parse-line path)]
    (-> m
        vec ;; convert from a lazyseq to a vector
        part-one
        println)))
