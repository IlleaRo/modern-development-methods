(ns modern-development-methods.task-2-1-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-2.task-2-1 :as sut]))

(deftest test-trapezoid-quadratic
  (let [f (fn [x] (* x x))
        exact (/ 8.0 3.0)]
    (is (<= (Math/abs (double (- exact (sut/get-trapezoid-integral f 2.0)))) (* exact 0.05)))))


(deftest test-trapezoid-double
  (let [f (fn [x] (* x 2))
        exact 25.0]
    (is (= exact (sut/get-trapezoid-integral f 5)))))

(deftest test-trapezoid-quadratic
  (let [f (fn [x] (+ (* x x) (* 5 x) 2))
        exact 11310.0]
    (is (<= (Math/abs (double (- exact (sut/get-trapezoid-integral f 30)))) (* exact 0.05)))))