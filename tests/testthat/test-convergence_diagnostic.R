# Tests for convergence_diagnostic()
library(quanteda)

immig_dem <- function() {
  toks <- tokens(cr_sample_corpus)
  it <- tokens_context(toks, "immigration", window = 6L, verbose = FALSE)
  dem(dfm(it), pre_trained = cr_glove_subset, transform = TRUE,
      transform_matrix = cr_transform, verbose = FALSE)
}

test_that("convergence_diagnostic() returns a tidy data.frame", {
  set.seed(1)
  conv <- convergence_diagnostic(immig_dem(), n_replicates = 10)
  expect_s3_class(conv, "data.frame")
  expect_true(all(c("n", "value", "std.error", "lower.ci", "upper.ci") %in% names(conv)))
  expect_true(all(conv$lower.ci <= conv$value & conv$value <= conv$upper.ci))
})

test_that("similarity to the full embedding approaches 1 as the sample grows", {
  set.seed(1)
  conv <- convergence_diagnostic(immig_dem(), n_replicates = 15)
  # the largest sample (= all instances) reproduces the full embedding exactly
  expect_equal(conv$value[which.max(conv$n)], 1, tolerance = 1e-8)
  # more instances -> higher mean similarity (allowing for sampling noise)
  expect_gt(conv$value[which.max(conv$n)], conv$value[which.min(conv$n)])
})

test_that("convergence_diagnostic() validates its inputs", {
  expect_error(convergence_diagnostic(matrix(rnorm(20), 4)), "must be a dem")
})
