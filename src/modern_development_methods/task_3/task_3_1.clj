(ns modern-development-methods.task-3.task-3-1)

(def core_const 4)

(defn split-into-n [n coll]
  (let [len (count coll)
        base-size (quot len n)
        remainder (mod len n)
        sizes (concat (repeat remainder (inc base-size))    ; rem штук по base+1
                      (repeat (- n remainder) base-size))] ; остальные по base
    (loop [s sizes, c coll, acc []]
      (if (empty? s)
        acc
        (recur (rest s)
               (drop (first s) c)
               (conj acc (take (first s) c)))))))

(defn get-cpu-cores []
  (try
    (let [cores (.availableProcessors (Runtime/getRuntime))]
      cores)
    (catch Exception e
      (println "Ошибка при получении информации о процессоре:" (.getMessage e))
      nil)))

(defn parallel-filter [f coll]
  (let [cores (or (get-cpu-cores) core_const)
        chunks (split-into-n cores coll)]
    (->> chunks
         (map #(future (filter f %)))
         (doall)                                            ; Это обязательно
         (map deref)
         (doall)                                            ; Это нужно, чтобы получить результат сейчас
         (apply concat))))

(defn -main
  [& _]
  (time (doall (filter #(zero? (mod % 2)) (range 1 100000))))
  (time (parallel-filter #(zero? (mod % 2)) (range 1 1000000)))
  (shutdown-agents))