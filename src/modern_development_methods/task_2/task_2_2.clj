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


(defn measure-ms [f]
  (let [t0 (System/nanoTime)
        _ (f)
        t1 (System/nanoTime)]
    (/ (- t1 t0) 1e6)))                                     ; миллисекунды

(defn do-test [f n]
  (let [times (for [i (range n)]
                (let [m (+ 1 i)                   ; увеличиваем m каждый шаг
                      t0 (System/nanoTime)
                      _ (f test_f m test_h)
                      t1 (System/nanoTime)]
                  (/ (- t1 t0) 1e6)))]
    {:avg (/ (reduce + times) n)
     :min (apply min times)
     :max (apply max times)}))



(defn -main
  [& _]
  (println "Basic func:")
  (println (do-test get-trapezoid-integral 100))
  (println (do-test get-trapezoid-integral 1000))
  (println "Stream func:")
  (println (do-test get-trapezoid-stream-integral 1000)))