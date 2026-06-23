# The jackknife uses an analytic fast path (deflated_norm_fast) instead of
# re-fitting estimatr::lm_robust on every leave-one-out replicate. These tests
# lock in that it returns *identical* deflated squared-norms to run_ols().

test_that("deflated_norm_fast() matches run_ols() in the no-cluster case", {
  set.seed(1)
  n <- 60; D <- 8
  Y <- matrix(rnorm(n * D), n, D)
  X <- data.frame(a = rbinom(n, 1, 0.5), b = rbinom(n, 1, 0.5))
  ids <- factor(seq_len(n))                       # each obs its own cluster
  fast <- conText:::deflated_norm_fast(Y, X, ids)
  ref  <- conText:::run_ols(Y, X, ids)$normed_betas_deflated
  expect_equal(unname(fast), unname(ref), tolerance = 1e-8)
  expect_equal(names(fast), names(ref))
})

test_that("deflated_norm_fast() matches run_ols() in the clustered/weighted case", {
  set.seed(2)
  n <- 80; D <- 6
  Y <- matrix(rnorm(n * D), n, D)
  X <- data.frame(a = rbinom(n, 1, 0.5), b = rnorm(n))
  ids <- factor(sample(1:12, n, replace = TRUE))  # 12 clusters of varying size
  fast <- conText:::deflated_norm_fast(Y, X, ids)
  ref  <- conText:::run_ols(Y, X, ids)$normed_betas_deflated
  expect_equal(unname(fast), unname(ref), tolerance = 1e-8)
})

test_that("deflated_norm_fast() returns NULL on a rank-deficient design (caller falls back)", {
  set.seed(3)
  n <- 20; D <- 4
  Y <- matrix(rnorm(n * D), n, D)
  X <- data.frame(a = rbinom(n, 1, 0.5))
  X$b <- X$a                                       # perfectly collinear
  expect_null(conText:::deflated_norm_fast(Y, X, factor(seq_len(n))))
})

test_that("deflated_norm_fast() falls back on a near-singular (ill-conditioned) design", {
  set.seed(4)
  n <- 60; D <- 4
  Y <- matrix(rnorm(n * D), n, D)
  a <- rnorm(n)
  X <- data.frame(a = a, b = a + rnorm(n, sd = 1e-7))  # nearly collinear, still invertible
  expect_null(conText:::deflated_norm_fast(Y, X, factor(seq_len(n))))
  # a well-conditioned design still uses the fast path (returns a value)
  Xok <- data.frame(a = a, b = rnorm(n))
  expect_type(conText:::deflated_norm_fast(Y, Xok, factor(seq_len(n))), "double")
})
