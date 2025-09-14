(ns modern-development-methods.task-1-1)


(defn gen-words
  [alphabet prev k prefix]
  (if (zero? k)
    (println prefix)
    (letfn [(gen-loop [cur-set]
              (when-let [s (seq cur-set)]
                (let [cur (first s)]
                  (when (not= cur prev)
                    (gen-words alphabet cur (dec k) (str prefix cur))))
                (gen-loop (rest s))))]
      (gen-loop alphabet))))


(defn remove-all [x lst]
  (cond
    (empty? lst) '()
    (= x (first lst)) (remove-all x (rest lst))
    :else (cons (first lst) (remove-all x (rest lst)))))

(defn remove-duplicates [lst]
  (if (empty? lst)
    '()
    (let [head (first lst)]
      (cons head (remove-duplicates (remove-all head (rest lst)))))))

(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (remove-duplicates (seq (first args)))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (gen-words alphabet nil n ""))
    ))