(ns advent24.dayone
  (:require [clojure.java.io :as io]))

; lines are of the form int [three spaces] int
(defn parse-line [ln]
  (map parse-long (re-seq #"\d+" ln)))

; annoyingly, we need m to be a vector of vectors and this is just
; transpose
(defn zip [m]
  (apply mapv vector m))

(defn unzip [pairs]
  (apply zip pairs))

(defn load-input [path]
  (let [lines (with-open [rdr (io/reader path)]
                (into [] (line-seq rdr)))]
    (unzip [(map parse-line lines)])))

(def diff (comp abs -))

(defn sum-diffs [pairs]
  (let [tpairs (zip pairs)]
    (println (count tpairs))
    (reduce + (map (fn [[a b]] (diff a b)) tpairs))))

(defn count-freqs [v]
  (let [counts (group-by identity v)]
    (zipmap (keys counts) (map #(count %) (vals counts)))))

(defn similarity-score [v freqs]
  (reduce + (map (fn [k] (* k (get freqs k 0))) v)))

(defn -main [path]
  (let [[left right] (load-input path)
        freqs (count-freqs right)]
    (println (similarity-score left freqs)))
  ;; part one
 ;; (println path)
 ;; (->> path
 ;;      (load-input)
 ;;      (map sort)
 ;;      (sum-diffs)
 ;;      (println))
  )
