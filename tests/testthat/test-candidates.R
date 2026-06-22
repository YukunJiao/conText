# Regression tests for candidate-set handling robustness.
library(quanteda)

immig_context <- function() {
  get_context(x = cr_sample_corpus, target = "immigration", window = 6, verbose = FALSE)$context[1:300]
}

test_that("bootstrap_nns() treats candidates = character(0) as 'all' (like nns())", {
  ctx <- immig_context()
  set.seed(1)
  res_empty <- bootstrap_nns(context = ctx, pre_trained = cr_glove_subset, transform = TRUE,
                             transform_matrix = cr_transform, candidates = character(0),
                             bootstrap = TRUE, num_bootstraps = 100, N = 5)
  expect_s3_class(res_empty, "tbl_df")
  expect_true(nrow(res_empty) > 0)
})

test_that("bootstrap_nns() handles a single candidate (drop = FALSE)", {
  ctx <- immig_context()
  cand <- intersect("immigration", rownames(cr_glove_subset))
  skip_if(length(cand) == 0)
  set.seed(1)
  res_one <- bootstrap_nns(context = ctx, pre_trained = cr_glove_subset, transform = TRUE,
                           transform_matrix = cr_transform, candidates = cand,
                           bootstrap = TRUE, num_bootstraps = 100, N = 5)
  expect_equal(nrow(res_one), 1L)
})

test_that("nns() handles a single candidate (drop = FALSE)", {
  toks <- tokens(cr_sample_corpus)
  immig <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)
  d <- dem(dfm(immig), pre_trained = cr_glove_subset, transform = TRUE,
           transform_matrix = cr_transform, verbose = FALSE)
  wv <- dem_group(d, groups = d@docvars$party)
  cand <- intersect("reform", rownames(cr_glove_subset))
  skip_if(length(cand) == 0)
  res <- nns(wv, N = 1, candidates = cand, pre_trained = cr_glove_subset,
             as_list = FALSE, show_language = FALSE)
  expect_true(all(res$feature == cand))
})
