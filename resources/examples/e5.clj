(let [p (sample (beta 1 1))
      x (sample (beta (exp p) 1))
      d (bernoulli (* x p))]
  (observe d 1)
  p)
