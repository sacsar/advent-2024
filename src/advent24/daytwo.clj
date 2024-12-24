(ns advent24.daytwo
  (:require [clojure.string :as str]
            [advent24.core]))

(defn parse-line [line]
  (map Integer/parseInt (str/split line #" ")))

(defn safe-window? [[a b]]
  (let [diff (- a b)]
    (cond
      (= diff 0) :unsafe
      (> diff 3) :unsafe
      (< diff -3) :unsafe
      (> a b) :decreasing
      (< a b) :increasing
      :else :unsafe ;; should be impossible
      )))

(defn is-safe [report]
  (let [windows (map safe-window? (partition 2 1 report))
        merge-window (fn [acc x]
                       (if (not (:safe acc))
                         (reduced  {:safe false, :type nil}) ;; terminate early
                         (cond
                           ;; first item
                           (nil? (:type acc)) (if (= :unsafe x) (reduced {:safe false}) {:safe true :type x})
                           (not (= (:type acc)  x)) (reduced {:safe false})
                           ;; otherwise we're good and can keep going
                           :else acc)))]
    (-> (reduce merge-window {:safe true} windows)
        (:safe))))

;; for part two, I suspect it's easy enough to brute force it, since we're
;; talking about fairly small sequences. Otherwise, when we first encounter
;; a problem, we have two choices: drop the first, or drop the second
;; (in fact, in most cases, I think drop the second is the only viable option)

;; Let's try brute forcing it first

(defn -main [path]
 ;; (let [reports (advent24.core/load-input parse-line path)
 ;;       safe-count (count (filter is-safe reports))]
 ;;   (println safe-count))
  )
