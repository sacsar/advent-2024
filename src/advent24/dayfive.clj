(ns advent24.dayfive
  (:require [clojure.string :as str]))

(defn merge-relation
  "Merges an individual relation into the accumulator maps.

  acc has keys :lt and :comparable"
  [acc [a b]]
  ;; first define a conj in since we're essentially going to do the same
  ;; thing three times in row
  (let [conj-in (fn [m ks v] (update-in m ks (fn [vs] (if (nil? vs) #{v} (conj vs v)))))]
    (-> acc
        (conj-in ,,, [:lt a] b)
        (conj-in ,,, [:comparable a] b)
        (conj-in ,,, [:comparable b] a))))

(defn process-relations [raw-relations]
  (let [parse-relation (fn [l] (map Integer/parseInt (str/split l #"\|")))
        relations (map parse-relation raw-relations)]
    (reduce merge-relation {} relations)))

(defn parse-instructions [raw-instructions]
  (let [parse-line #(vec (map Integer/parseInt (str/split %1 #",")))]
    (map parse-line raw-instructions)))

(defn get-center [v]
  (get v (quot (count v) 2)))

(defn ordered-instruction? [relation-map instruction]
  (let [[x & xs] instruction
        remainder (drop-while #(contains? (get-in relation-map [:lt x] #{}) %1) xs)
        nxt (first remainder)]
    (cond
      (nil? xs) true
      (nil? nxt) (ordered-instruction? relation-map xs)
      ;; next is comparable to x, but not lt
      (contains? (get-in relation-map [:comparable x] #{}) nxt) false
      ;; otherwise, recurse
      :else (ordered-instruction? relation-map remainder))))

(defn part-one [instructions relation-map]
  (reduce + 0 (map get-center (filter (partial ordered-instruction? relation-map) instructions))))

(defn load-input [path]
  (let [[relations _ instructions] (->> path
                                        (slurp)
                                        (str/split-lines)
                                        (partition-by #(= %1 "")))]
    [(process-relations relations) (parse-instructions instructions)]))

(defn -main [path]
  ;; since the input is in two pieces, using the core function doesn't make sense here
  (let [[relation-map instructions] (load-input path)]
    (println (part-one instructions relation-map))))
