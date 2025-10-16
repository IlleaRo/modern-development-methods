(ns modern-development-methods.task-4.algebra.operations)

(defn conjunction [expr & rest]
  "Порождение конъюнкции"
  (cons ::conj (cons expr rest)))

(defn conjunction? [expr]
  "Проверка типа для конъюнкции"
  (= ::conj (first expr)))


(defn disjunction [expr & rest]
  "Порождение дизъюнкции"
  (cons ::sum (cons expr rest)))

(defn disjunction? [expr]
  "Проверка типа для дизъюнкции"
  (= ::disj (first expr)))


(defn inversion [expr]
  "Порождает инверсию"
  (cons ::inv (cons expr rest)))

(defn inversion? [expr]
  "Проверка типа для инверсии"
  (= ::inv (first expr)))


(defn implication [expr]
  "Порождает импликацию"
  (cons ::impl expr))
(defn implication? [expr]
  "Проверка типа для импликации"
  (= ::impl (first expr)))

(defn args [expr]
  "Список аргументов выражения"
  {:pre [(or (conjunction? expr) (disjunction? expr) (implication? expr))]}
  (rest expr))