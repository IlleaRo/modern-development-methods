(ns modern-development-methods.task-4.algebra.constant)

(defn constant [num]
  "Порождение константы"
  {:pre [(boolean? num)]}
  (list ::const num))

(defn constant? [expr]
  "Проверка типа для константы"
  (= (first expr) ::const))

(defn constant-value [expr]
  "Получение значения константы"
  {:pre [(constant? expr)]}
  (second expr))