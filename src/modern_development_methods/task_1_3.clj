(ns modern-development-methods.task-1-3)

(defn my-map [f coll]
  (reduce (fn [acc x] (concat acc (list (f x)))) `() coll))

(defn my-filter [f coll]
  (reduce
   (fn [acc x]
     (if (f x)
       (concat acc (list x))
       acc))
   `() coll))

(defn -main
  [& args]
  (if (> 1 (count args))
    (println "use program <list>")
    (let [nums (map #(Integer/parseInt %) args)]
  (println (my-map inc nums))
  (println (my-filter (fn [n] (= 0 (mod n 2))) nums)))))
