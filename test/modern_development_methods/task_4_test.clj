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
  (test/is (sut/constant? (sut/constant true)))
  (test/is (= true (sut/constant-value (sut/constant true)))))

(deftest inversion-false-test
  (let [res (sut/dnf (sut/inversion (sut/constant false)))
        answer (sut/constant true)]
    (test/is (= res answer))))

(deftest inversion-test
  (let [res-1 (sut/dnf (sut/inversion (sut/constant true)))
        res-2 (sut/dnf (sut/inversion (sut/inversion (sut/constant true))))
        res-3 (sut/dnf (sut/inversion (sut/constant false)))
        res-4 (sut/dnf (sut/inversion (sut/inversion (sut/variable 'A))))
        answer-1 (sut/constant false)
        answer-2 (sut/constant true)
        answer-3 (sut/constant true)
        answer-4 (sut/variable 'A)]
    (test/is (= res-1 answer-1))
    (test/is (= res-2 answer-2))
    (test/is (= res-3 answer-3))
    (test/is (= res-4 answer-4))))

(deftest conjunction-test
  (let [res-1 (sut/dnf (sut/conjunction (list (sut/constant true) (sut/constant false))))
        res-2 (sut/dnf (sut/conjunction (list (sut/constant true) (sut/variable 'A))))
        answer-1 (sut/constant false)
        answer-2 (sut/variable 'A)]
    (test/is (= res-1 answer-1))
    (test/is (= res-2 answer-2))))

(deftest disjunction-test
  (let [res (sut/dnf (sut/disjunction (list (sut/constant true) (sut/constant false))))
        answer (sut/constant true)]
    (test/is (= res answer))))

(deftest implementation-test
  (let [res-1 (sut/dnf (sut/implication (list (sut/constant false) (sut/constant true))))
        res-2 (sut/dnf (sut/implication (list (sut/constant true) (sut/constant false))))
        answer-1 (sut/constant true)
        answer-2 (sut/constant false)]
    (test/is (= res-1 answer-1))
    (test/is (= res-2 answer-2))))

(deftest distribution-test
  ; A ∧ (B ∨ C) ≡ (A ∧ B) ∨ (A ∧ C)
  (let [res (sut/dnf (sut/conjunction (list (sut/variable 'A) (sut/disjunction (list (sut/variable 'B) (sut/variable 'C))))))
        answer (sut/disjunction (list (sut/conjunction (list (sut/variable 'A) (sut/variable 'B))) (sut/conjunction (list (sut/variable 'A) (sut/variable 'C)))))]
   (test/is (= res answer))))

