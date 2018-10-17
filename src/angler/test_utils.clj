(ns angler.test-utils)

(defn abs-no-branching
  [^double x]
  (Math/copySign x 1.0))

(defn d=
  [^double x ^double y ^double e]
  (< (abs-no-branching (- x y)) e))
