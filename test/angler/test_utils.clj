(ns angler.test-utils)

(defn d=
  [^double x ^double y ^double e]
  (< (Math/copySign (- x y) 1.0) e))
