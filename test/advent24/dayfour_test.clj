(ns advent24.dayfour-test
  (:require [clojure.test :as t]
            [advent24.dayfour :as dayfour]
            [clojure.string :as str]))

(def M
  (vec (map #(str/split %1 #"") (-> (slurp "resources/sample4.txt")
                                    (str/split ,,, #"\n")))))

(t/deftest test-has-xmas?
  (t/testing "horizontal x-mas"
    (t/is (dayfour/has-xmas? [["X" "M" "A" "S"]] 0 0 1 4 :e))))

(t/deftest test-has-value?
  (let [m [["X" "M" "A" "S"]]]
    (t/testing "has X"
      (t/is (dayfour/has-value? "X" m 0 0 1 4)))
    (t/testing "has M"
      (t/is (dayfour/has-value? "M" m 0 1 1 4)))
    (t/testing "has A"
      (t/is (dayfour/has-value? "A" m 0 2 1 4)))
    (t/testing "has S"
      (t/is (dayfour/has-value? "S" m 0 3 1 4)))))

(t/deftest test-part-one
  (t/testing "counts 18 in sample"
    (t/is (= 18 (dayfour/part-one M)))))
