(ns modern-development-methods.task-4.algebra.variable)

(defn variable [name]
  "Порождение переменной"
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  "Проверка типа для переменной"
  (= (first expr) ::var))

(defn variable-name [expr]
  "Получение значения для переменной"
  (second expr))

(defn same-variables? [v1 v2]
  "Сравнение переменных"
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))