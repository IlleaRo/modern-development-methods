(ns modern-development-methods.task-4.rules
  (:require [modern-development-methods.task-4.algebra.constant :as c]
            [modern-development-methods.task-4.algebra.variable :as v]
            [modern-development-methods.task-4.algebra.operations :as ops]))

(declare dnf)

(def dnf-rules
  (list
    (list
      ; Правила вывода констант
      [c/constant? identity]
      [v/variable? identity]

      ; Правила вывода для дизъюнкции: A ∨ (B ∨ C), (A ∨ B) ∨ C ≡ A ∨ B ∨ C
      [ops/disjunction?
       (fn [expr]
         (ops/disjunction (mapcat #(if (ops/disjunction? %)
                                     (map dnf (ops/args %))
                                     (list (dnf %)))
                                  (ops/args expr))))]

      [ops/conjunction?
       (fn [expr]
         (let
           ; Правила вывода для конъюнкции - закон дистрибутивности: A ∧ (B ∨ C) ≡ (A ∧ B) ∨ (A ∧ C)
           [dnf-args (mapcat #(if (ops/conjunction? %)
                                (map dnf (ops/args %))
                                (list (dnf %)))
                             (ops/args expr))
            founded-disjunct (first (filter ops/disjunction? dnf-args))]
           (if (nil? founded-disjunct)
             (ops/conjunction dnf-args)
             (let [other-args (first (filter #(not (identical? founded-disjunct %)) dnf-args))]
               (dnf (ops/disjunction (map #(dnf (ops/conjunction (list % other-args))) (ops/args founded-disjunct))))))))]

      ; Правила вывода для инверсии: ¬A
      [ops/inversion?
       (fn [expr]
         (let [arg (first (ops/args expr))]
           (cond
             ; ¬0 -> 1
             (and (c/constant? arg) (= 0 (c/constant-value arg)))
             (c/constant 1)

             ; ¬1 -> 0
             (and (c/constant? arg) (= 1 (c/constant-value arg)))
             (c/constant 0)

             ; ¬¬A -> A (двойное отрицание)
             (ops/inversion? arg)
             (dnf (first (ops/args arg)))

             ; ¬(A | B) -> ¬A & ¬B (закон де Моргана)
             (ops/disjunction? arg)
             (apply ops/conjunction
                    (map #(dnf (ops/inversion %))
                         (ops/args arg)))

             ; ¬(A & B) -> ¬A | ¬B (закон де Моргана)
             (ops/conjunction? arg)
             (apply ops/disjunction
                    (map #(dnf (ops/inversion %))
                         (ops/args arg)))

             ; В остальных случаях просто применяем инверсию
             :else
             (ops/inversion (dnf arg)))))]

      ; Правила вывода для импликации: A -> B эквивалентно ¬A | B
      [ops/implication?
       (fn [expr]
         (let [[antecedent consequent] (ops/args expr)]
           (dnf (ops/disjunction
                  (ops/inversion antecedent)
                  consequent))))]
      )))