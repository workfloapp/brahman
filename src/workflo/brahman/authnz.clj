(ns workflo.brahman.authnz
  (:refer-clojure :exclude [set?]))

(defn set? [x]
  (not (nil? x)))

(defn equals? [x y]
  (= x y))
