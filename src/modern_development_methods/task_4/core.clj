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

(defn dnf [expr]
  "Функция преобразования выражения в форму dnf"
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         rules/dnf-rules)
   expr))

(defn -main [& _]
  (dnf (ops/disjunction (c/constant 1) (c/constant 0))))