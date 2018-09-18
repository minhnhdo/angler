(let [z (sample (bernoulli 0.5))]
  (observe (normal z 1) 1))
