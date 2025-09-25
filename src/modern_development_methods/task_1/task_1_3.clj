(ns modern-development-methods.task-1.task-1-3)

(defn my-map [f coll]
  (reduce (fn [acc x] (conj acc (f x))) `[] coll)) ; Если мы используем вектор, то conj добавляет в конец

(defn my-filter [f coll]
  (reduce
   (fn [acc x]
     (if (f x)
       (conj acc x)
       acc))
   `[] coll))


(defn my-map-without-reduce [f coll]
  (loop [input-coll coll, res-coll (list)]
    (if (> (count input-coll) 0)
      (recur
       (rest input-coll)
       (conj res-coll (f (first input-coll)))) ; conj добавляет в начало списка
      (reverse res-coll))))                    ; по этой причине при использованиии списка, его нужно в конце развернуть

(defn my-filter-without-reduce [f coll]
  (loop [input-coll coll, res-coll '()]
    (if (> (count input-coll) 0)
      (let [x (first input-coll)]
        (recur (rest input-coll)
               (if (f x)
                 (conj res-coll x)
                 res-coll)))
      (reverse res-coll))))

(defn -main
  [& args]
  (if (> 1 (count args))
    (println "use program <list>")
    (let [nums (map #(Integer/parseInt %) args)]
      (println (my-map inc nums))
      (println (my-map-without-reduce inc nums))
      (println (my-filter (fn [n] (= 0 (mod n 2))) nums))
      (println (my-filter-without-reduce (fn [n] (= 0 (mod n 2))) nums)))))
