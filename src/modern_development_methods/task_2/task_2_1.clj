(ns modern-development-methods.task-2.task-2-1)

(defn get-trapezoid-integral [f x h]
  (let [N (long (Math/floor (/ x h))),
        A (* (f 0) 0.5)
        B (* (f x) 0.5)
        S (+ A
             (reduce + (map (fn [i] (f (* i h))) (range 1 N)))
             B)]
    (* h S)))

(def get-trapezoid-integral-mem-var-1        ; Мемоизация значений первообразной
  (memoize get-trapezoid-integral))          ; не снизит сложность построения графика


(defn summator [f h N]
  (reduce + (map (fn [i] (f (* i h))) (range 1 N))))
(def summator-mem ; кэш на узлах i*h
    (memoize summator))

(defn get-trapezoid-integral-mem-var-2 [f x h]
  (let [N (long (Math/floor (/ x h))),
        A (* (f 0) 0.5)
        B (* (f x) 0.5)
        S (+ A
             (summator-mem f h N)
             B)]
    (* h S)))


(defn bench [label fns]
  (println "=== " label " ===")
  (doseq [[tag f] fns]
    (print tag "-> ")
    (time (f)))
  (println))

(defn -main [& _]
  (let [f (fn [x] (* x x))
        h 0.1]
  (bench "Plain trapezoid"
         [["x=50" #(get-trapezoid-integral f 50 h)]
          ["x=50" #(get-trapezoid-integral f 50 h)]
          ["x=48" #(get-trapezoid-integral f 48 h)]
          ["x=30" #(get-trapezoid-integral f 30 h)]
          ["x=72" #(get-trapezoid-integral f 72 h)]
          ["x=71" #(get-trapezoid-integral f 71 h)]
          ["x=5"  #(get-trapezoid-integral f 5 h)]])

  (bench "Mem var-1"
         [["x=50" #(get-trapezoid-integral-mem-var-1 f 50 h)]
          ["x=50" #(get-trapezoid-integral-mem-var-1 f 50 h)]
          ["x=48" #(get-trapezoid-integral-mem-var-1 f 48 h)]
          ["x=30" #(get-trapezoid-integral-mem-var-1 f 30 h)]
          ["x=72" #(get-trapezoid-integral-mem-var-1 f 72 h)]
          ["x=71" #(get-trapezoid-integral-mem-var-1 f 71 h)]
          ["x=5"  #(get-trapezoid-integral-mem-var-1 f 5 h)]])

  (bench "Mem var-2"
         [["x=50" #(get-trapezoid-integral-mem-var-2 f 50 h)]
          ["x=50" #(get-trapezoid-integral-mem-var-2 f 50 h)]
          ["x=48" #(get-trapezoid-integral-mem-var-2 f 48 h)]
          ["x=30" #(get-trapezoid-integral-mem-var-2 f 30 h)]
          ["x=72" #(get-trapezoid-integral-mem-var-2 f 72 h)]
          ["x=71" #(get-trapezoid-integral-mem-var-2 f 71 h)]
          ["x=5"  #(get-trapezoid-integral-mem-var-2 f 5 h)]])))

