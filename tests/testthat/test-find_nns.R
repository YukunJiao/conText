# Regression tests for find_nns() and prototypical_context()
library(quanteda)

test_that("find_nns() returns N neighbors and handles candidates", {
  res <- find_nns(target_embedding = cr_glove_subset["immigration", ],
                  pre_trained = cr_glove_subset, N = 5, candidates = NULL, norm = "l2")
  expect_length(res, 5)

  # character(0) candidates behaves like "all" (not an error)
  res_all <- find_nns(target_embedding = cr_glove_subset["immigration", ],
                      pre_trained = cr_glove_subset, N = 5, candidates = character(0))
  expect_length(res_all, 5)
})

test_that("find_nns() handles a single candidate and N larger than candidate set", {
  one <- find_nns(target_embedding = cr_glove_subset["immigration", ],
                  pre_trained = cr_glove_subset, N = 5,
                  candidates = "reform")
  # only one candidate -> exactly one neighbor, no NA padding
  expect_equal(one, "reform")
})

test_that("prototypical_context() returns N rows without NA padding", {
  ctx <- get_context(x = cr_sample_corpus, target = "immigration", window = 6, verbose = FALSE)
  pt <- prototypical_context(context = ctx$context, pre_trained = cr_glove_subset,
                             transform = TRUE, transform_matrix = cr_transform, N = 3, norm = "l2")
  expect_equal(nrow(pt), 3L)
  expect_false(anyNA(pt$typicality_score))
})
