(ns modern-development-methods.task-1.task-1-2)

(defn expand-stack
  ([cs prev k prefix s]
   (if (seq cs)
     (let [c (first cs)]
       (recur (rest cs) prev k prefix
              (if (= c prev)
                s
                (cons [c (dec k) (str prefix c)] s))))
     s)))

(defn gen-words
  ([alphabet n]
   (gen-words alphabet n (list [nil n ""])))                ; стартовый стек
  ([alphabet n stack]
   (when-let [[prev k prefix] (first stack)]
     (if (zero? k)
       (do
         (println prefix)
         (recur alphabet n (rest stack)))
       (let [next-stack (expand-stack (seq alphabet) prev k prefix (rest stack))]
         (recur alphabet n next-stack))))))

(defn remove-duplicates
  ([lst]
   (remove-duplicates lst #{} '()))
  ([xs seen acc]
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