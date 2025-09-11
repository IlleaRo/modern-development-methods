(ns modern-development-methods.task-1-2)

(defn gen-words
  [alphabet n]
  (loop [stack (list [nil n ""])]
    (when-let [[prev k prefix] (first stack)]
      (if (zero? k)
        (do
          (println prefix)
          (recur (rest stack)))
        (let [next-stack
              (loop [cs (seq (reverse alphabet))
                     s (rest stack)]
                (if (seq cs)
                  (let [c (first cs)]
                    (recur (rest cs)
                           (if (= c prev)
                             s
                             (cons [c (dec k) (str prefix c)] s))))
                  s))]
          (recur next-stack))))))


(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (seq (first args))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (gen-words alphabet n))
    ))