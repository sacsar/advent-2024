(ns advent24.core
  (:require [clojure.java.io :as io]))

; annoyingly, we need m to be a vector of vectors and this is just
; transpose
(defn zip [m]
  (apply mapv vector m))

(defn unzip [pairs]
  (apply zip pairs))

(defn load-input [parse-line path]
  (let [lines (with-open [rdr (io/reader path)]
                (into [] (line-seq rdr)))]
    (map parse-line lines)))
