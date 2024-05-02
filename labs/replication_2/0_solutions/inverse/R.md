
# R


``` r
set.seed(12)
A <- matrix(rnorm(100),ncol = 10)
A[,10] = A[,1] * 2 + A[,8] * 3 + A[,9]
solve(A)
```

    Error in solve.default(A): system is computationally singular: reciprocal condition number = 1.09852e-18
