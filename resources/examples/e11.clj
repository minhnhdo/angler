(let [y-values [2.1 3.9 5.3 7.7 10.2]
      slope (sample (normal 0.0 10.0))
      intercept (sample (normal 0.0 10.0))]
  (foreach 5
           [x (range 1 6)
            y y-values]
           (let [fx (+ (* slope x) intercept)]
             (observe (normal fx 1.0) y)))
  [slope intercept])
