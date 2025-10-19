(ns modern-development-methods.task-4.core
  (:require [modern-development-methods.task-4.algebra.variable :as v]
            [modern-development-methods.task-4.algebra.constant :as c]
            [modern-development-methods.task-4.algebra.operations :as ops]
            [modern-development-methods.task-4.rules :as rules]))


; Будем считать, что у нас уже получено дерево синтаксического разбора
; Правила дифференцирования - однозначные правила
; Символьное интегрирование - плохой пример
; будем использовать key-word (то что предваряется двоеточием) - нечто между стройкой и значением
; Большинство правил вывода рекурсивны - выражение правил через другие. Само правило дифференцирования может вызвать дифференцирование

; Если выражение является константой возвращаем 0
; В зависимости от совпадения переменной дифференцирования возвращаем 1 или 0


; А теперь задание

;По аналогии с задачей дифференцирования
;реализовать представление символьных булевых
;выражений с операциями конъюнкции, дизъюнкции
;отрицания, импликации. Выражения могут включать
;как булевы константы, так и переменные.
;Реализовать подстановку значения переменной в
;выражение с его приведением к ДНФ.
;
;Код должен быть покрыт тестами, API документирован.

; Сводить можно к любой ДНФ
; Нужно продемонстрировать расширяемость - чтобы добавить штрих Шеффера, мне нужно...


(def variable v/variable)
(def variable? v/variable?)
(def variable-name v/variable-name)
(def same-variables? v/same-variables?)

(def constant c/constant)
(def constant? c/constant?)
(def constant-value c/constant-value)

(def disjunction ops/disjunction)
(def conjunction ops/conjunction)
(def inversion ops/inversion)
(def implication ops/implication)


(defn simplify [expr]
  "Функция упрощения булевых выражений"
  (if-let [action (some (fn [[check action]]
                          (when (check expr)
                            action))
                        rules/simplify-rules)]
    (let [result (action expr)]
      ; Применяем рекурсивно, пока результат меняется
      (if (= result expr)
        result
        (simplify result)))
    expr))

(defn dnf [expr]
  "Функция, которая преобразует логическое выражение в ДНФ.
  Применяет набор правил преобразования, чтобы упростить и преобразовать выражение в ДНФ."
  (simplify ((some (fn [[check, action]]
                     (if (check expr)
                       action
                       false))
                   rules/dnf-rules)
             expr)))

(defn -main [& _]
  (println (dnf (dnf (conjunction (list (variable 'A) (disjunction (list (variable 'B) (variable 'C))))))))
  (println (dnf (conjunction (list (constant true) (variable 'A))))))