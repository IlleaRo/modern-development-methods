(ns modern-development-methods.task-3.task-3-1)

(def core_const 4)

(defn split-into-n [n coll]
  (let [len (count coll)
        base-size (quot len n)                              ; размер каждой части
        remainder (mod len n)                               ; остаток
        sizes (concat (repeat (dec n) base-size)            ; N - 1 равных частей
                      [(+ base-size remainder)])]           ; Последняя включает остаток от деления
    (loop [s sizes
           c coll
           acc []]
      (if (empty? s)
        acc
        (recur (rest s)
               (drop (first s) c)
               (conj acc (take (first s) c)))))))

(defn get-cpu-cores
  "Безопасно получает количество CPU ядер"
  []
  (try
    (let [cores (.availableProcessors (Runtime/getRuntime))]
      (println "Количество ядер процессора:" cores)
      cores)
    (catch Exception e
      (println "Ошибка при получении информации о процессоре:" (.getMessage e))
      nil)))

; Разбить последовательность на чанки, исходя из количества ядер или каких-то констант

(defn heavy-inc [n]
  (Thread/sleep 100)
  (inc n))


; Map, которая каждый элемент вычисляет в отдельном потоке (из-за doall неленивая вариация)
(defn parallel-map [f coll]
  (->> coll
       (map (fn [x] (future (f x))))
       (doall)
       (map deref)
       (doall)
       ))

(defn parallel-filter [f coll]
  (let [cores (get-cpu-cores)]
    (if (not (= cores nil))
      (println (split-into-n cores coll))
      (println (split-into-n core_const coll))
      )))

(defn -main
  [& _]
  ;(println (get-cpu-cores))
  ;(time
  ;  (->> (iterate inc 0)
  ;       (take 10)
  ;       (map heavy-inc)
  ;       (doall)))
  ;
  ;(time
  ;  (->> (iterate inc 0)
  ;       (take 10)
  ;       (parallel-map heavy-inc)
  ;       ))

  (parallel-filter #(inc %) (range 1 17)))