(ns modern-development-methods.task-2.task-2-2-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-2.task-2-2 :as sut]))

(def h 0.1)
(def eps 1e-9)

(deftest test-trapezoid-double
  (let [f (fn [x] (* x x))
        base (sut/get-trapezoid-integral f 2 h)
        stream   (sut/get-trapezoid-stream-integral f 2 h)
        exact (/ 8.0 3.0)]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream))) eps))))


(deftest test-trapezoid-double
  (let [f (fn [x] (* x 2))
        base (sut/get-trapezoid-integral f 5 h)
        stream   (sut/get-trapezoid-stream-integral f 5 h)
        exact 25.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream))) eps))))

(deftest test-trapezoid-quadratic
  (let [f (fn [x] (+ (* x x) (* 5 x) 2))
        base (sut/get-trapezoid-integral f 30 h)
        stream   (sut/get-trapezoid-stream-integral f 30 h)
        exact 11310.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream))) eps))))