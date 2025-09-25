(ns modern-development-methods.task-2.task-2-1-test
  (:require [clojure.test :refer :all]
            [modern-development-methods.task-2.task-2-1 :as sut]))

(deftest task-2-1-test
  (testing "Testing task-2-1"
    (is (= (sut/fact 0) 1))))