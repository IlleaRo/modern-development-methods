(ns modern-development-methods.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn -main
  [& args]
  (foo "O Nice to meet you!"))