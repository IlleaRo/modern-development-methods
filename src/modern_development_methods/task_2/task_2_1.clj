(ns modern-development-methods.task-2.task-2-1)

(defn fact [n]
  (if (> n 1)
    (reduce * (range 2 (inc n)))
    1))

(defn double-x [x]
  (* 2 x))

(defn get-trapezoid-integral [f x d_step]                   ; TODO: наверное, нужно указывать не длину шага, а их количество
  (let [n (max 1 (long (Math/ceil (/ x d_step))))
        d_step (/ x n)
        s (+ (* 0.5 (f 0.0))                                ; Площадь первой трапеции
             (reduce + (map #(f (* % d_step)) (range 1 n))) ; Площадь неконечных трапеций TODO: оптимизировать
             (* 0.5 (f x)))]                                ; Площадь последней трапеции
    (* d_step s)))

(defn -main
  [& args]
  (println (get-trapezoid-integral double-x 5 10))
  )