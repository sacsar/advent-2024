(ns advent24.daythree
  (:require [advent24.core :as core]))

(def mul-instruction #"mul\((\d+),(\d+)\)")
(def instruction #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")

(defn execute-mul-instruction [[_, a, b]]
  (let [n (Integer/parseInt a)
        m (Integer/parseInt b)]
    (* m n)))

(defn extract-mul-instructions [line]
  (re-seq mul-instruction line))

(defn part-one [lines]
  (let [instructions (mapcat extract-mul-instructions lines)]
    (->> instructions
         (map execute-mul-instruction)
         (reduce + 0)
         println)))

;; Part two: extend to support do and don't
(defn parse-instruction [[m a b]]
  (case m
    "do()" :do
    "don't()" :dont
    ;; else it's a mul
    (execute-mul-instruction [m a b])))

(defn extract-instructions [line]
  (map parse-instruction (re-seq instruction line)))

(defn part-two [lines]
  (let [instructions (mapcat extract-instructions lines)]
    (:acc (reduce (fn [acc inst]
                    (case inst
                      :do (assoc acc :state :do)
                      :dont (assoc acc :state :dont)
                ;; default -- this is a mul
                      (case (:state acc)
                        :do (let [s (:acc acc)]
                              (assoc acc :acc (+ s inst)))
                        :dont acc))) {:state :do :acc 0} instructions))))

(defn -main [path]
  (let  [lines (core/load-input identity path)]
    (println (part-two lines))))
