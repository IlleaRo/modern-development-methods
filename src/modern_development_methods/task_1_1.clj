(ns modern-development-methods.task-1-1)


(defn gen-words
  [alphabet prev k prefix]
  (if (zero? k)
    (println prefix)
    (loop [cur_set alphabet]
      (when (seq cur_set)
        (let [cur (first cur_set)]
          (when (not= cur prev)
            (gen-words alphabet cur (dec k) (str prefix cur)))
          )
        (recur (rest cur_set))))))


(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (seq (first args))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (gen-words alphabet nil n ""))
    ))