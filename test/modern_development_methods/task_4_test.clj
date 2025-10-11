(ns modern-development-methods.task-4-test
  (:require [clojure.test :refer :all :as test]
            [modern-development-methods.task-4.core :as sut]))

(deftest variable-test
  (test/is (sut/variable (sut/variable :x)))
  (test/is (= :x (sut/variable-name (sut/variable :x))))
  (test/is (sut/same-variables?
             (sut/variable :x)
             (sut/variable :x)))

  (test/is (not (sut/same-variables?
                  (sut/variable :x)
                  (sut/variable :y)))))

(deftest constant-test
  (test/is (sut/constant? (sut/constant 1)))
  (test/is (= 1 (sut/constant-value (sut/constant 1)))))