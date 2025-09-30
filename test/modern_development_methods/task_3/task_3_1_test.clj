(ns modern-development-methods.task-3.task-3-1-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-3.task-3-1 :as sut]))

(deftest split-test
  (let [n-1 5 set-1 (range 1 10) answer-1 [[1 2] [3 4] [5 6] [7 8] [9]]
        n-2 6 set-2 (range 1 10) answer-2 [[1 2] [3 4] [5 6] [7] [8] [9]]
        n-3 4 set-3 (range 1 3) answer-3 [[1] [2] [] []]]
    (is (= (sut/split-into-n n-1 set-1) answer-1))
    (is (= (sut/split-into-n n-2 set-2) answer-2))
    (is (= (sut/split-into-n n-3 set-3) answer-3))))

(deftest filter-test
  (let [set (range 1 200)]
    (letfn [(f [x] (zero? (mod x 2)))]
      (is (= (sut/parallel-filter f set) (filter f set))))))

(deftest get-cpu-core-test
  (is (sut/get-cpu-cores)))