(ns modern-development-methods.task-1-2)

;(defn gen-words
;  [alphabet n]
;  (letfn [(go [prev k prefix]
;            (if (zero? k)
;              (println prefix)
;              (loop [cur alphabet]
;                (when (seq cur)
;                  (let [c (first cur)]
;                    (when (not= c prev)
;                      (go c (dec k) (str prefix c)))
;                    (recur (rest cur)))))))]
;    (go nil n "")))
;
;
;
;
;(defn -main
;  [& args]
;  (if (not= (count args) 2)
;    (do
;      (println "Usage: <alphabet> <n>")
;      (System/exit 1))
;    (let [alphabet (seq (first args))
;          n (Integer/parseInt (second args))]
;      (println "Alphabet:" alphabet " num:" n)
;      (gen-words alphabet n))
;    ))

(defn gen-words
  [alphabet cur_set prev k prefix]
  (if (zero? k)
    do (
         (println prefix)
         (gen-words alphabet (rest alphabet) nil (count alphabet) ""))
    (when (seq cur_set)
      (let [cur (first cur_set)]
        (when (not= cur prev)
          (gen-words alphabet (rest cur_set) cur_set (dec k) (str prefix cur)))
        ))))


(defn -main
  [& args]
  (if (not= (count args) 2)
    (do
      (println "Usage: <alphabet> <n>")
      (System/exit 1))
    (let [alphabet (seq (first args))
          n (Integer/parseInt (second args))]
      (println "Alphabet:" alphabet " num:" n)
      (gen-words alphabet alphabet nil n ""))
    ))