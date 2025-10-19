(ns modern-development-methods.task-4.rules
  (:require [modern-development-methods.task-4.algebra.constant :as c]
            [modern-development-methods.task-4.algebra.variable :as v]
            [modern-development-methods.task-4.algebra.operations :as ops]))

(defn dnf [expr]
  (require '[modern-development-methods.task-4.core :as core])
  ((resolve 'modern-development-methods.task-4.core/dnf) expr))

(defn simplify [expr]
  (require '[modern-development-methods.task-4.core :as core])
  ((resolve 'modern-development-methods.task-4.core/simplify) expr))

(def dnf-rules
  (list
    ; Правила вывода констант
    [c/constant? identity]
    [v/variable? identity]

    ; Правила вывода для дизъюнкции: A ∨ (B ∨ C), (A ∨ B) ∨ C ≡ A ∨ B ∨ C
    [ops/disjunction?
     (fn [expr]
       (ops/disjunction (mapcat #(if (ops/disjunction? %)
                                   (map dnf (ops/arguments %))
                                   (list (dnf %)))
                                (ops/arguments expr))))]

    [ops/conjunction?
     (fn [expr]
       (let
         ; Правила вывода для конъюнкции - закон дистрибутивности: A ∧ (B ∨ C) ≡ (A ∧ B) ∨ (A ∧ C)
         [dnf-args (mapcat #(if (ops/conjunction? %)
                              (map dnf (ops/arguments %))
                              (list (dnf %)))
                           (ops/arguments expr))
          founded-disjunct (first (filter ops/disjunction? dnf-args))]
         (if (nil? founded-disjunct)
           (ops/conjunction dnf-args)
           (let [other-args (first (filter #(not (identical? founded-disjunct %)) dnf-args))]
             (dnf (ops/disjunction (map #(dnf (ops/conjunction (list % other-args))) (ops/arguments founded-disjunct))))))))]

    ; Правила вывода для инверсии: ¬A
    [ops/inversion?
     (fn [expr]
       (let [arg (first (ops/arguments expr))]
         (cond
           ; ¬¬A -> A (двойное отрицание)
           (ops/inversion? arg)
           (dnf (first (ops/arguments arg)))

           ; ¬(A | B) -> ¬A & ¬B (закон де Моргана)
           (ops/disjunction? arg)
           (apply ops/conjunction
                  (map #(dnf (ops/inversion %))
                       (ops/arguments arg)))

           ; ¬(A & B) -> ¬A | ¬B (закон де Моргана)
           (ops/conjunction? arg)
           (apply ops/disjunction
                  (map #(dnf (ops/inversion %))
                       (ops/arguments arg)))

           ; В остальных случаях просто применяем инверсию
           :else
           (ops/inversion (dnf arg)))))]

    ; Правила вывода для импликации: A -> B эквивалентно ¬A | B
    [ops/implication?
     (fn [expr]
       (let [[antecedent consequent] (ops/arguments expr)]
         (dnf (ops/disjunction (list (ops/inversion antecedent)
                                     consequent)
                               ))))]
    ))

(def simplify-rules
  (list
    [c/constant? identity]
    [v/variable? identity]

    [ops/disjunction?
     (fn [expr]
       (let [args (ops/arguments expr)
             simplified-args (map simplify args)
             ; Проверяем наличие true
             has-true (some #(and (c/constant? %)
                                  (= true (c/constant-value %)))
                            simplified-args)
             ; Убираем false константы
             non-false (filter #(not (and (c/constant? %)
                                          (= false (c/constant-value %))))
                               simplified-args)]
         (cond
           ; Если есть хотя бы одна true - вся дизъюнкция true
           has-true (c/constant true)

           ; Если после удаления false ничего не осталось - результат false
           (empty? non-false) (c/constant false)

           ; Если остался один элемент - возвращаем его
           (= (count non-false) 1) (first non-false)

           ; Иначе возвращаем дизъюнкцию оставшихся
           :else (ops/disjunction non-false))))]

    [ops/conjunction?
     (fn [expr]
       (let [args (ops/arguments expr)
             simplified-args (map simplify args)
             ; Проверяем наличие false
             has-false (some #(and (c/constant? %)
                                   (= false (c/constant-value %)))
                             simplified-args)
             ; Убираем true константы
             non-true (filter #(not (and (c/constant? %)
                                         (= true (c/constant-value %))))
                              simplified-args)]
         (cond
           ; Если есть хотя бы одна false - вся конъюнкция false
           has-false (c/constant false)

           ; Если после удаления true ничего не осталось - результат true
           (empty? non-true) (c/constant true)

           ; Если остался один элемент - возвращаем его
           (= (count non-true) 1) (first non-true)

           ; Иначе возвращаем конъюнкцию оставшихся
           :else (ops/conjunction non-true))))]

    [ops/inversion?
     (fn [expr]
       (let [arg (first (ops/arguments expr))
             simplified-arg (simplify arg)]
         (cond
           ; ¬true → false
           (and (c/constant? simplified-arg)
                (= true (c/constant-value simplified-arg)))
           (c/constant false)

           ; ¬false → true
           (and (c/constant? simplified-arg)
                (= false (c/constant-value simplified-arg)))
           (c/constant true)

           ; ¬¬A → A (двойное отрицание)
           (ops/inversion? simplified-arg)
           (simplify (first (ops/arguments simplified-arg)))

           ; Иначе оставляем инверсию упрощённого аргумента
           :else
           (ops/inversion simplified-arg))))]))