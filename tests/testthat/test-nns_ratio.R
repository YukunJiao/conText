# Regression test for nns_ratio()/get_nns_ratio() N over-indexing
library(quanteda)

test_that("get_nns_ratio() does not pad with NA when N exceeds candidate count", {
  toks <- tokens(cr_sample_corpus)
  immig <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)
  cands <- intersect(c("reform", "enforce", "border"), rownames(cr_glove_subset))
  skip_if(length(cands) < 2)
  set.seed(1)
  res <- get_nns_ratio(x = immig, N = 10, groups = docvars(immig, "party"),
                       candidates = cands, pre_trained = cr_glove_subset,
                       transform = TRUE, transform_matrix = cr_transform,
                       bootstrap = FALSE, permute = FALSE)
  expect_false(anyNA(res$feature))
  expect_true(nrow(res) <= length(cands))
})
