(ns hello-world.bad-indent)

(+ 1
2 3)

(defn hello
  ([a b]
    (println a b))
  ([a b c]
   (println a b c)))

