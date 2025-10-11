(ns modern-development-methods.task-4.rules
  (:require [modern-development-methods.task-4.algebra.constant :as c]
            [modern-development-methods.task-4.algebra.variable :as v]
            [modern-development-methods.task-4.algebra.operations :as ops]))

(declare dnf)

(def dnf-rules
  (list
    (list
      ; Правила вывода констант
      [(fn [expr vr] (c/constant? expr))]
      [(fn [expr vr] (c/constant 0))]

      ; Правила вывода переменных
      [(fn [expr vr]
         (and
           (v/variable? expr)
           (v/same-variables? expr vr)))
       (fn [expr vr] (c/constant 1))]
      [(fn [expr vr] (v/variable? expr))
       (fn [expr vr] (c/constant 0))]

      ; Правила вывода для дизъюнкции: A | (B | C), (A | B) | C -> A | B | C
      [(fn [expr vr] (ops/disjunction? expr))
       (fn [expr vr]
         (apply ops/disjunction
                (map #(dnf % vr)
                     (ops/args expr))))]

      ; Правила вывода для конъюнкции: A & (B & C) -> A & B & C
      [(fn [expr vr] (ops/conjunction? expr))
       (fn [expr vr]
         (apply ops/conjunction
                (map #(dnf % vr)
                     (ops/args expr))))]

      ; Правила вывода для инверсии: ¬A
      [(fn [expr vr] (ops/inversion? expr))
       (fn [expr vr]
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
             (dnf (first (ops/args arg)) vr)

             ; ¬(A | B) -> ¬A & ¬B (закон де Моргана)
             (ops/disjunction? arg)
             (apply ops/conjunction
                    (map #(dnf (ops/inversion %) vr)
                         (ops/args arg)))

             ; ¬(A & B) -> ¬A | ¬B (закон де Моргана)
             (ops/conjunction? arg)
             (apply ops/disjunction
                    (map #(dnf (ops/inversion %) vr)
                         (ops/args arg)))

             ; В остальных случаях просто применяем инверсию
             :else
             (ops/inversion (dnf arg vr)))))]

      ; Правила вывода для импликации: A -> B эквивалентно ¬A | B
      [(fn [expr vr] (ops/implication? expr))
       (fn [expr vr]
         (let [[antecedent consequent] (ops/args expr)]
           (dnf (ops/disjunction
                  (ops/inversion antecedent)
                  consequent)
                vr)))]
      )))