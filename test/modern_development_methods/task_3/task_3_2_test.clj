(ns modern-development-methods.task-3.task-3-2-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-3.task-3-2 :as sut]))

(deftest filter-test
  (let [set (range 1 200)]
    (letfn [(f [x] (zero? (mod x 2)))]
      (is (= (sut/pfilter f set) (filter f set))))))

(deftest get-cpu-core-test
  (is (sut/get-cpu-cores)))


(deftest seq-and-lazy-seq
  (letfn [(f [x] (zero? (mod x 2)))]
    (let [lazy-set (range 1 20)
          seq-set (doall (take 20 (range 1 20)))
          lazy-set-res (doall (sut/pfilter f lazy-set))
          set-res (doall (sut/pfilter f seq-set))]
      (is (= lazy-set-res set-res)))))