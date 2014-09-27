(ns xn.library.svg)

(defn nanan [x]
  (if (js/isNaN x) 0 x))

(defn translate
  ([x] (translate x x))
  ([x y]
   (str " translate(" (nanan x) " " (nanan y) ")")))

(defn rotate
  ([deg] (str " rotate(" (nanan deg) ")"))
  ([deg x y] (str " rotate(" (nanan deg) " " (nanan x) " " (nanan y) ")")))

(defn scale [s]
  (str " scale(" (nanan s) ")"))
