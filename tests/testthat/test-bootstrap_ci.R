# Regression tests for bootstrap percentile confidence intervals.
# The CI must use the (1 - cl)/2 and (1 + cl)/2 quantiles (standard percentile
# bootstrap, as in boot::boot.ci(type = "perc")), not (1 - cl) and cl, which
# previously yielded a 90% interval for a requested 95% level.
library(quanteda)

immig_toks <- function() {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  it <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)
  tokens_sample(it, size = 120, by = docvars(it, "party"))
}

test_that("get_cos_sim() CIs are ordered and widen with confidence_level", {
  it <- immig_toks()
  set.seed(42)
  narrow <- get_cos_sim(it, groups = docvars(it, "party"), features = c("reform", "enforce"),
                        pre_trained = cr_glove_subset, transform = TRUE,
                        transform_matrix = cr_transform, bootstrap = TRUE,
                        num_bootstraps = 200, confidence_level = 0.80, as_list = FALSE)
  set.seed(42)
  wide <- get_cos_sim(it, groups = docvars(it, "party"), features = c("reform", "enforce"),
                      pre_trained = cr_glove_subset, transform = TRUE,
                      transform_matrix = cr_transform, bootstrap = TRUE,
                      num_bootstraps = 200, confidence_level = 0.99, as_list = FALSE)

  expect_true(all(narrow$lower.ci <= narrow$value & narrow$value <= narrow$upper.ci))
  # a higher confidence level must give a (weakly) wider interval everywhere,
  # and strictly wider somewhere -- confirms the quantile probs are actually used
  expect_true(all((wide$upper.ci - wide$lower.ci) >= (narrow$upper.ci - narrow$lower.ci) - 1e-9))
  expect_gt(sum(wide$upper.ci - wide$lower.ci), sum(narrow$upper.ci - narrow$lower.ci))
})

test_that("bootstrap_nns() CIs are ordered and widen with confidence_level", {
  ctx <- get_context(x = cr_sample_corpus, target = "immigration", window = 6, verbose = FALSE)$context
  ctx <- ctx[1:300]
  set.seed(7)
  narrow <- bootstrap_nns(context = ctx, pre_trained = cr_glove_subset,
                          transform = TRUE, transform_matrix = cr_transform, candidates = NULL,
                          bootstrap = TRUE, num_bootstraps = 200, confidence_level = 0.80, N = 10)
  set.seed(7)
  wide <- bootstrap_nns(context = ctx, pre_trained = cr_glove_subset,
                        transform = TRUE, transform_matrix = cr_transform, candidates = NULL,
                        bootstrap = TRUE, num_bootstraps = 200, confidence_level = 0.99, N = 10)
  expect_true(all(narrow$lower.ci <= narrow$value & narrow$value <= narrow$upper.ci))
  expect_gt(mean(wide$upper.ci - wide$lower.ci), mean(narrow$upper.ci - narrow$lower.ci))
})
