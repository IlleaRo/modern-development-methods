(ns modern-development-methods.task-2.task-2-2-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-2.task-2-2 :as sut]))

(def h 0.1)
(def eps 1e-9)

(deftest test-trapezoid-double
  (let [f (fn [x] (* x x))
        base (sut/get-trapezoid-integral f 2 h)
        stream-mem   (sut/get-mem-trapezoid-stream-integral f 2 h)
        stream-lazy-1  (sut/get-silly-trapezoid-integral f 2 h)
        stream-lazy-2  (sut/get-lazy-cat-trapezoid-integral f 2 h)
        exact (/ 8.0 3.0)]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream-mem))) eps))
    (is (< (Math/abs (double (- base stream-lazy-1))) eps))
    (is (< (Math/abs (double (- base stream-lazy-2))) eps))))


(deftest test-trapezoid-double
  (let [f (fn [x] (* x 2))
        base (sut/get-trapezoid-integral f 5 h)
        stream   (sut/get-mem-trapezoid-stream-integral f 5 h)
        stream-lazy-1  (sut/get-silly-trapezoid-integral f 5 h)
        stream-lazy-2  (sut/get-lazy-cat-trapezoid-integral f 5 h)
        exact 25.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream))) eps))
    (is (< (Math/abs (double (- base stream-lazy-1))) eps))
    (is (< (Math/abs (double (- base stream-lazy-2))) eps))))

(deftest test-trapezoid-quadratic
  (let [f (fn [x] (+ (* x x) (* 5 x) 2))
        base (sut/get-trapezoid-integral f 30 h)
        stream   (sut/get-mem-trapezoid-stream-integral f 30 h)
        stream-lazy-1  (sut/get-silly-trapezoid-integral f 30 h)
        stream-lazy-2  (sut/get-lazy-cat-trapezoid-integral f 30 h)
        exact 11310.0]
    (is (< (Math/abs (double (- base exact))) (* 0.05 exact)))
    (is (< (Math/abs (double (- base stream))) eps))
    (is (< (Math/abs (double (- base stream-lazy-1))) eps))
    (is (< (Math/abs (double (- base stream-lazy-2))) eps))))