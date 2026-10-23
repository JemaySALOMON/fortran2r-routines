dyn.load("matinv.so")

solvef90 <- function(X) {
  n   <- nrow(X)
  res <- .C("solvef90",
            n    = as.integer(n),
            A    = as.double(X),
            Ainv = double(n*n),
            info = integer(1),
            msg  = strrep(" ", 256))
  if (res$info != 0)
    stop("inv(): ", res$msg, " (LAPACK info = ", res$info, ")")
  matrix(res$Ainv, n, n)
}

## correctness + error demo
set.seed(1)
n = 1000
A <- matrix(rnorm(n*n), n, n)
cat("invertible 1000x1000: max err =", max(abs(solve(A) - solvef90(A))), "\n")
S <- matrix(c(1,2,2,4), 2, 2)
cat("singular ->", tryCatch(solvef90(S), error = conditionMessage), "\n\n")
