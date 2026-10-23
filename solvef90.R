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

set.seed(1)
A <- matrix(rnorm(1e6), 1000, 1000)
cat("invertible: max err =", max(abs(solve(A) - solvef90(A))), "\n")
S <- matrix(c(1,2,2,4), 2, 2)
tryCatch(solvef90(S), error = conditionMessage)
