(ns modern-development-methods.task-2.task-2-1)

(def N 100)

(defn get-trapezoid-integral [f x]
  (let [h (/ x N),
        A (* (f 0) 0.5)
        B (* (f x) 0.5)
        S (+ A
             (reduce + (map (fn [i] (f (* i h))) (range 1 N)))
             B)]
    (* h S)))