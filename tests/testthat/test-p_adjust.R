# Tests for the p.adjust.method (multiple-comparison correction) option
library(quanteda)

immig_sample <- function() {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  it <- tokens_context(toks, "immigration", window = 6L, verbose = FALSE)
  tokens_sample(it, size = 120, by = docvars(it, "party"))
}

test_that("get_nns_ratio() adds an adjusted p-value matching stats::p.adjust()", {
  immig <- immig_sample()
  set.seed(1)
  raw <- get_nns_ratio(x = immig, N = 5, groups = docvars(immig, "party"),
                       pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                       bootstrap = FALSE, permute = TRUE, num_permutations = 100, verbose = FALSE)
  set.seed(1)
  adj <- get_nns_ratio(x = immig, N = 5, groups = docvars(immig, "party"),
                       pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                       bootstrap = FALSE, permute = TRUE, num_permutations = 100,
                       p.adjust.method = "BH", verbose = FALSE)

  expect_false("p.value.adjusted" %in% names(raw))        # default unchanged
  expect_true("p.value.adjusted" %in% names(adj))
  expect_equal(adj$p.value.adjusted, stats::p.adjust(adj$p.value, "BH"))
  expect_true(all(adj$p.value.adjusted >= adj$p.value - 1e-12))
})

test_that("get_nns_ratio() rejects an invalid adjustment method", {
  immig <- immig_sample()
  expect_error(
    get_nns_ratio(x = immig, groups = docvars(immig, "party"), pre_trained = cr_glove_subset,
                  transform_matrix = cr_transform, bootstrap = FALSE, permute = TRUE,
                  num_permutations = 100, p.adjust.method = "nope", verbose = FALSE)
  )
})

test_that("contrast_nns() adds an adjusted p-value when requested", {
  immig <- immig_sample()
  set.seed(1)
  adj <- contrast_nns(x = immig, groups = docvars(immig, "party"),
                      pre_trained = cr_glove_subset, transform = TRUE,
                      transform_matrix = cr_transform, bootstrap = FALSE,
                      permute = TRUE, num_permutations = 100, p.adjust.method = "holm",
                      candidates = NULL, N = 5, verbose = FALSE)
  expect_true("p.value.adjusted" %in% names(adj))
  expect_equal(adj$p.value.adjusted, stats::p.adjust(adj$p.value, "holm"))
})
