(ns modern-development-methods.task-2.task-2-2)

(def test_h 0.1)
(defn test_f [x]
  (+ (* x x) (* 5 x) 2))

(defn get-trapezoid-integral [f x h]                        ; Базовая ф-ия для простого расчета
  (let [N (long (Math/floor (/ x h))),
        A (* (f 0) 0.5)
        B (* (f x) 0.5)
        S (+ A
             (reduce + (map (fn [i] (f (* i h))) (range 1 N)))
             B)]
    (* h S)))


(defn trapezoid-integral-stream [f h]
  (let [integral-step (fn [[x prev-sum]]                    ; на вход ф-ии предыдущая точка и значение в ней
                        (let [next-x (+ x h)                ; next-x - следующая точка для вычисления
                              next-sum (+ prev-sum          ; Вычисляем значение суммы в новой точке
                                          (* h (/ (+ (f x) (f next-x)) 2.0)))] ; Площадь трапеции
                          [next-x next-sum]))]
    (->> [0.0 0.0]
         (iterate integral-step)
         (map (fn [[x sum]] [x sum])))))                    ; сохраняем пары [координата, значение]

; var 1. Только для определенной ф-ии и шага
; ----------------------------------------------
(def specific-integral-seq (trapezoid-integral-stream test_f test_h))

(defn get-integral-at-specific [x]
  (let [steps (long (/ x test_h))]
    (second (nth specific-integral-seq steps))))

; ----------------------------------------------



; var 2. Для разных ф-ий и шага, но с мемоизацией
; ----------------------------------------------

(def memoized-integral-stream (memoize trapezoid-integral-stream)) ; Мемоизация по ф-ям и шагам

(defn get-trapezoid-stream-integral [f x h]
  (let [integral-seq (memoized-integral-stream f h)         ; Нужно работать с заданным h
        steps (long (/ x h))]
    (second (nth integral-seq steps))))
; ----------------------------------------------


(defn -main
  [& _]
  (println "Basic func:")
  (time (get-trapezoid-integral test_f 40 test_h))
  (time (get-trapezoid-integral test_f 30 test_h))
  (time (get-trapezoid-integral test_f 50 test_h))
  (time (get-trapezoid-integral test_f 45 test_h))
  (println "Stream func:")
  (time (get-integral-at-specific 40))
  (time (get-integral-at-specific 30))
  (time (get-integral-at-specific 50))
  (time (get-integral-at-specific 45))
  (println "Stream func universe:")
  (time (get-trapezoid-stream-integral test_f 40 test_h))
  (time (get-trapezoid-stream-integral test_f 30 test_h))
  (time (get-trapezoid-stream-integral test_f 50 test_h))
  (time (get-trapezoid-stream-integral test_f 45 test_h)))