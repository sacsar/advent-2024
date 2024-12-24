(ns advent24.daythree
  (:require [advent24.core :as core]))

(def mul-instruction #"mul\((\d+),(\d+)\)")

(defn extract-instructions [line]
  (re-seq mul-instruction line))

(defn execute-instruction [[_, a, b]]
  (let [n (Integer/parseInt a)
        m (Integer/parseInt b)]
    (* m n)))

(defn -main [path]
  (let  [lines (core/load-input identity path)
         instructions (mapcat extract-instructions lines)]
    (->> instructions
         (map execute-instruction)
         (reduce + 0)
         println)))
