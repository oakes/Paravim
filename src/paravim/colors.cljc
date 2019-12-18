(ns paravim.colors)

(def bg-color [(/ 52 255) (/ 40 255) (/ 42 255) 0.95])

(def text-color [1 1 1 1])
(def cursor-color [(/ 112 255) (/ 128 255) (/ 144 255) 0.9])
(def select-color [(/ 148 255) (/ 69 255) (/ 5 255) 0.8])
(def search-color [(/ 127 255) (/ 52 255) (/ 83 255) 0.8])

(def text-alpha 1.0)
(def parinfer-alpha 0.15)
(def highlight-alpha 0.05)
(def unfocused-alpha 0.5)
(def completion-alpha 0.65)

(def yellow-color [(/ 255 255) (/ 193 255) (/ 94 255) 1])
(def tan-color [(/ 209 255) (/ 153 255) (/ 101 255) 1])
(def cyan-color [(/ 86 255) (/ 181 255) (/ 194 255) 1])
(def gray-color [(/ 150 255) (/ 129 255) (/ 133 255) 1])

(def colors {:number yellow-color
             :string tan-color
             :keyword cyan-color
             :comment gray-color})

(def orange-color [(/ 220 255) (/ 103 255) (/ 44 255) 1])
(def red-color [(/ 210 255) (/ 45 255) (/ 58 255) 1])
(def green-color [(/ 65 255) (/ 174 255) (/ 122 255) 1])

(def rainbow-colors [orange-color
                     red-color
                     green-color])

(defn get-color [class-name depth]
  (or (colors class-name)
      (case class-name
        :delimiter (nth rainbow-colors (mod depth (count rainbow-colors)))
        text-color)))

(defn set-alpha [color alpha]
  (assoc color 3 alpha))

