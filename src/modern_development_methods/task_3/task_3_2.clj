(ns modern-development-methods.task-3.task-3-2)

; В данном задании необходимо, чтобы фильтр работал как с конечной, так и с бесконечной последовательностью.
; Косяк возникает из-за фиксированного чанка - если размер последовательности не кратен чанку, то хвост нужно обрабатывать корректно
; Одновременно должен быть профит от параллелизма и должна быть ленивой

(def core_const 4)


;(defn split-into-n [n coll]
;  (let [len (count coll)
;        base-size (quot len n)
;        remainder (mod len n)
;        sizes (concat (repeat remainder (inc base-size))    ; rem штук по base+1
;                      (repeat (- n remainder) base-size))]  ; остальные по base
;    (loop [s sizes, c coll, acc []]
;      (if (empty? s)
;        acc
;        (recur (rest s)
;               (drop (first s) c)
;               (conj acc (take (first s) c)))))))

(defn get-cpu-cores []
  (try
    (let [cores (.availableProcessors (Runtime/getRuntime))]
      cores)
    (catch Exception e
      (println "Ошибка при получении информации о процессоре:" (.getMessage e))
      nil)))

;
;(defn parallel-lazy-map [cores-num f coll]
;  (letfn [(step [chunk xs]
;            (lazy-seq
;              (when (seq xs)
;                (let [new-fut (future (f (first xs)))
;                      chs' (conj chunk new-fut)]
;                  (if (>= (count chs') cores-num)
;                    (cons @(first chs')
;                          (step (rest chs') (rest xs)))
;                    (step chs' (rest xs)))))))]
;    (step [] coll)))
;
;
;(defn parallel-lazy-filter [pred coll]
;  (let [cores (or (get-cpu-cores) core_const)]
;    (->> (parallel-lazy-map cores #(when (pred %) %) coll))))


(defn pfilter [f coll]
  (let [cores (+ (or (get-cpu-cores) core_const) 2)
        rets  (map (fn [x] (future (when (f x) x))) coll)
        step (fn step [[x & xs :as vs] fs]
               (lazy-seq
                 (if-let [s (seq fs)]
                   (let [v (deref x)]
                     (if (nil? v)
                       (step xs (rest s))
                       (cons v (step xs (rest s))))
                   (->> vs
                        (map deref)
                        (remove nil?))))))]
    (step rets (drop cores rets))))


(defn -main
  [& _]
  (time (doall (filter #(zero? (mod % 2)) (range 1 1000))))
  (time (doall (filter #(zero? (mod % 2)) (range 1 1000))))
  (time (doall (pfilter #(zero? (mod % 2)) (range 1 1000))))
  (time (doall (pfilter #(zero? (mod % 2)) (range 1 1000))))
  (time (println (filter #(zero? (mod % 2)) (range 1 100))))
  (time (println (doall (pfilter #(zero? (mod % 2)) (range 1 100)))))

  (shutdown-agents))