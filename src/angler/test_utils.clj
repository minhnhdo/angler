(ns angler.test-utils)

(defn abs-no-branching
  [^double x]
  (Math/copySign x 1.0))

(defn d=
  [^double x ^double y ^double e]
  (< (abs-no-branching (- x y)) e))

(defn d=5%
  [^double reference ^double result]
  (d= reference result (abs-no-branching (* 0.05 reference))))
