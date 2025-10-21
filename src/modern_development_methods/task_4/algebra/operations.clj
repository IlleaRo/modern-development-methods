(ns modern-development-methods.task-4.algebra.operations
  (:require [modern-development-methods.task-4.algebra.variable :as v]
            [modern-development-methods.task-4.algebra.constant :as c]))

(defn conjunction [expr]
  "Порождение конъюнкции"
  (cons ::conj expr))

(defn conjunction? [expr]
  "Проверка типа для конъюнкции"
  (= ::conj (first expr)))


(defn disjunction [expr]
  "Порождение дизъюнкции"
  (cons ::disj expr))

(defn disjunction? [expr]
  "Проверка типа для дизъюнкции"
  (= ::disj (first expr)))


(defn inversion [expr]
  "Порождает инверсию"
  (list ::inv expr))

(defn inversion? [expr]
  "Проверка типа для инверсии"
  (= ::inv (first expr)))


(defn implication [expr]
  "Порождает импликацию"
  (cons ::impl expr))
(defn implication? [expr]
  "Проверка типа для импликации"
  (= ::impl (first expr)))

(defn arguments [expr]
  "Список аргументов выражения"
  {:pre [(or (conjunction? expr) (disjunction? expr) (implication? expr) (inversion? expr))]}
  (rest expr))

(defn op-type [expr]
  (first expr))

(defn expr-equal? [expr1 expr2]
  "Функция, проверяет, равны ли два выражения"
  (if (= (op-type expr1) (op-type expr2))                   ; Проверяем, что тип операции совпадает
    (cond
      (c/constant? expr1) (= (c/constant-value expr1) (c/constant-value expr2)) ; Совпадают значения констант?
      (v/variable? expr1) (= (v/variable-name expr1) (v/variable-name expr2)) ; Совпадают имена переменных?

      (or (disjunction? expr1) (conjunction? expr1))        ; В конъюнкции и дизъюнкции порядок не имеет значение
      (let [args1 (arguments expr1)
            args2 (arguments expr2)]
        (and (= (count args1) (count args2))
             ; Каждый элемент из args1 имеет эквивалент в args2 и наоборот (коммутативность)
             (every? (fn [a1]
                       (some #(expr-equal? a1 %) args2))
                     args1)
             (every? (fn [a2]
                       (some #(expr-equal? a2 %) args1))
                     args2)))

      ; Для импликации A -> B нужно проверить и A, и B
      (implication? expr1)
      (let [args1 (arguments expr1)
            args2 (arguments expr2)]
        (and (= (count args1) (count args2))
             (every? #(expr-equal? (first %) (second %)) (map vector args1 args2))))

      ; Для инверсии только сравниваем аргументы
      (inversion? expr1)
      (expr-equal? (second expr1) (second expr2))

      :else
      false)
    false))