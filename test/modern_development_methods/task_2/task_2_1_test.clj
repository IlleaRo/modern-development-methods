(ns modern-development-methods.task-2.task-2-1-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-2.task-2-1 :as sut]))

(def h 0.1)
(def eps 1e-9)

(defmacro time->err [expr]
  `(binding [*out* *err*]
     (time ~expr)))

(deftest test-trapezoid-double
  (let [f (fn [x] (* x x))
        base (sut/get-trapezoid-integral f 2 h)
        v1   (sut/get-trapezoid-integral-mem-var-1 f 2 h)
        v2   (sut/get-trapezoid-integral-mem-var-2 f 2 h)
        exact (/ 8.0 3.0)]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base v1))) eps))
    (is (< (Math/abs (double (- base v2))) eps))))


(deftest test-trapezoid-double
  (let [f (fn [x] (* x 2))
        base (sut/get-trapezoid-integral f 5 h)
        v1   (sut/get-trapezoid-integral-mem-var-1 f 5 h)
        v2   (sut/get-trapezoid-integral-mem-var-2 f 5 h)
        exact 25.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base v1))) eps))
    (is (< (Math/abs (double (- base v2))) eps))))

(deftest test-trapezoid-quadratic
  (let [f (fn [x] (+ (* x x) (* 5 x) 2))
        base (sut/get-trapezoid-integral f 30 h)
        v1   (sut/get-trapezoid-integral-mem-var-1 f 30 h)
        v2   (sut/get-trapezoid-integral-mem-var-2 f 30 h)
        exact 11310.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base v1))) eps))
    (is (< (Math/abs (double (- base v2))) eps))))