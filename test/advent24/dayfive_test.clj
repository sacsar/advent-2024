(ns advent24.dayfive-test
  (:require [clojure.test :as t]
            [advent24.dayfive :as dayfive]))

(def sample-input (dayfive/load-input "resources/sample5.txt"))

(t/deftest test-ordered-instruction
  (let [[relation-map _] sample-input]
    (t/testing "ordered samples"
      (t/is (true? (dayfive/ordered-instruction? relation-map [75 47 61 53 29])))
      (t/is (true? (dayfive/ordered-instruction? relation-map [97 61 53 29 13])))
      (t/is (true? (dayfive/ordered-instruction? relation-map [75 29 13]))))
    (t/testing "unordered samples"
      (t/is (false? (dayfive/ordered-instruction? relation-map [75 97 47 61 53])))
      (t/is (false? (dayfive/ordered-instruction? relation-map [61 13 29])))
      (t/is (false? (dayfive/ordered-instruction? relation-map [97 13 75 29 47]))))))

(t/deftest test-sample-data
  (let [[relation-map instructions] (dayfive/load-input "resources/sample5.txt")]
    (t/testing "part one sample"
      (t/is (= 143 (dayfive/part-one instructions relation-map))))))
