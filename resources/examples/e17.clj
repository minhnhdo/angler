(let [weight-prior (normal 0 1)
      W_0 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
      W_1 (foreach 10 []
                   (foreach 10 [] (sample weight-prior)))
      W_2 (foreach 1 []
                   (foreach 10 [] (sample weight-prior)))
      b_0 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
      b_1 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
      b_2 (foreach 1 []
                   (foreach 1 [] (sample weight-prior)))
      x
      (mat-transpose [[1] [2] [3] [4] [5]])
      y
      [[1] [4] [9] [16] [25]]
      h_0 (mat-tanh (mat-add (mat-mul W_0 x)
                             (mat-repmat b_0 1 5)))
      h_1 (mat-tanh (mat-add (mat-mul W_1 h_0)
                             (mat-repmat b_1 1 5)))
      mu (mat-transpose
           (mat-tanh
             (mat-add (mat-mul W_2 h_1)
                      (mat-repmat b_2 1 5))))]
  (foreach 5 [y_r y
              mu_r mu ]
           (foreach 1 [y_rc y_r
                       mu_rc mu_r ]
                    (observe (normal mu_rc 1) y_rc)))
  [W_0 b_0 W_1 b_1 ])
