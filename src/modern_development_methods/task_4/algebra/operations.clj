(ns modern-development-methods.task-4.algebra.operations)

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