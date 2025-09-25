(ns modern-development-methods.task-1.task-1-4)

(defn gen-words
  [alphabet n]
  (map (fn [word] (apply str word))
       (reduce (fn [word_set _]                                   ; Мы не используем числа 0, 1, 2, ..., n
                 (reduce (fn [acc word]                               ; расширяем каждое слово word
                           (let [choices (if (empty? word)
                                           alphabet
                                           (filter (fn [x] (not= x (peek word))) alphabet))] ; запрет одинаковых подряд
                             (reduce (fn [acc ch] (conj acc (conj word ch))) acc choices)))
                         [] word_set))
               [[]] (range n)))) ; Нужно сделать n шагов

(defn remove-duplicates
  [alphabet]
  (reduce
   (fn [acc x]
     (if (not (some #{x} acc))
       (concat acc (list x))
       acc))
   `() alphabet))

(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (remove-duplicates (seq (first args)))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (println (gen-words alphabet n)))))
