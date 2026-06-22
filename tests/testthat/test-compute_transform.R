# Regression tests for compute_transform() (issue #28)
library(quanteda)

test_that("compute_transform() works with weighting = 'log'", {
  toks <- tokens(cr_sample_corpus)
  toks_fcm <- fcm(toks, context = "window", window = 6,
                  count = "weighted", weights = 1 / (1:6), tri = FALSE)
  A <- compute_transform(x = toks_fcm, pre_trained = cr_glove_subset, weighting = "log")
  expect_equal(dim(A), c(ncol(cr_glove_subset), ncol(cr_glove_subset)))
  expect_false(anyNA(as.matrix(A)))
})

test_that("compute_transform() works with a numeric weighting threshold", {
  toks <- tokens(cr_sample_corpus)
  toks_fcm <- fcm(toks, context = "window", window = 6,
                  count = "weighted", weights = 1 / (1:6), tri = FALSE)
  A <- compute_transform(x = toks_fcm, pre_trained = cr_glove_subset, weighting = 5)
  expect_equal(dim(A), c(ncol(cr_glove_subset), ncol(cr_glove_subset)))
})

test_that("compute_transform() tolerates NA / unnamed margin entries (issue #28)", {
  # off-the-shelf / non-local embeddings can leave NA or unnamed frequencies,
  # which previously produced a Matrix 'subscript out of bounds' error on the
  # 'log' path.
  toks <- tokens(cr_sample_corpus[1:40])
  toks_fcm <- fcm(toks, context = "window", window = 6, count = "frequency", tri = FALSE)
  toks_fcm@meta$object$margin[c(3, 7, 11)] <- NA
  A <- compute_transform(x = toks_fcm, pre_trained = cr_glove_subset, weighting = "log")
  expect_equal(nrow(A), ncol(cr_glove_subset))
})

test_that("compute_transform() errors informatively on insufficient overlap", {
  toks <- tokens(cr_sample_corpus[1:40])
  toks_fcm <- fcm(toks, context = "window", window = 6, count = "frequency", tri = FALSE)
  set.seed(2)
  pt <- cr_glove_subset[sample(nrow(cr_glove_subset), 120), ]
  expect_error(
    compute_transform(x = toks_fcm, pre_trained = pt, weighting = "log"),
    "too few to estimate"
  )
})
