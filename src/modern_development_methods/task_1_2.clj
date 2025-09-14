(ns modern-development-methods.task-1-2)

(defn gen-words
  [alphabet n]
  (loop [stack (list [nil n ""])]                           ; Вектор состояний
    (when-let [[prev k prefix] (first stack)]
      (if (zero? k)
        (do
          (println prefix)
          (recur (rest stack)))
        (let [next-stack
              (loop [cs (seq alphabet)
                     s (rest stack)]
                (if (seq cs)
                  (let [c (first cs)]
                    (recur (rest cs)
                           (if (= c prev)
                             s                              ; Текущий и предыдущий символы совпадают - ничего не добавляем, передаем s как есть
                             (cons [c (dec k) (str prefix c)] s))))
                  s))]
          (recur next-stack))))))

(defn remove-duplicates [lst]
  (loop [xs lst seen #{} acc '()]
    (if (empty? xs)
      (reverse acc)
      (let [head (first xs)]
        (if (seen head)
          (recur (rest xs) seen acc)
          (recur (rest xs) (conj seen head) (cons head acc)))))))

(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (remove-duplicates (seq (first args)))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (gen-words alphabet n))
    ))