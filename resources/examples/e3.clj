(let [data (vector 1 2 (sample (normal 1 1)))
      a (conj [] (sample (normal 0 2)))
      b (conj a (sample (normal 0 3)))]
  (observe (normal (second b) 4) (first (rest data)))
  b)
