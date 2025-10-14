(ns modern-development-methods.task-3.task-3-2)

; В данном задании необходимо, чтобы фильтр работал как с конечной, так и с бесконечной последовательностью.
; Косяк возникает из-за фиксированного чанка - если размер последовательности не кратен чанку, то хвост нужно обрабатывать корректно
; Одновременно должен быть профит от параллелизма и должна быть ленивой

(def core_const 4)

(defn get-cpu-cores []
  (try
    (let [cores (.availableProcessors (Runtime/getRuntime))]
      cores)
    (catch Exception e
      (println "Ошибка при получении информации о процессоре:" (.getMessage e))
      nil)))

(defn pfilter [f coll]
  (let [cores (+ (or (get-cpu-cores) core_const) 2)         ; Вычисляем количество одновременных задач
        rets (map (fn [x] (future (when (f x) x))) coll)    ; Создаем ленивую последовательность из объектов future
        step (fn step [[x & xs :as vs] fs]                  ; Разбиваем аргументы коллекции на начало (x) и хвост (xs) и заворачиваем в vs
               (lazy-seq
                 (if-let [s (seq fs)]                       ; Если какие-то future еще запланированы
                   (let [v (deref x)]                       ; Ждем завершение future - получаем ее результат
                     (if (nil? v)                           ; Если элемент НЕ прошел проверку filter
                       (step xs (rest s))                   ; Пропускаем элемент
                       (cons v (step xs (rest s)))))        ; Добавляем элемент в результат
                   (->> vs                                  ; Когда закончились futures необходимо обработать хвост
                        (map deref)
                        (remove nil?)))))]
    (step rets (drop cores rets))))

(defn expensive-even?
  [x]
  (Thread/sleep 5)
  (zero? (mod x 3)))

(defn make-hdr [str]
  (println "======= " str " ======="))

(defn -main
  [& _]
  (make-hdr "sequential filter")
  (time (doall (filter expensive-even? (range 1 1000))))
  (time (doall (filter expensive-even? (range 1 1000))))
  (make-hdr "parallel filter")
  (time (doall (pfilter expensive-even? (range 1 1000))))
  (time (doall (pfilter expensive-even? (range 1 1000))))
  (make-hdr "test parallel filter")
  (println (take 20 (pfilter expensive-even? (range))))
  (println (doall (pfilter expensive-even? (doall (take 20 (range))))))
  (shutdown-agents))