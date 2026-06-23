# Direct tests for exported functions that lacked their own coverage.
library(quanteda)

test_that("fem() builds a feature-embedding matrix", {
  toks <- tokens(cr_sample_corpus)
  fcm1 <- fcm(toks, context = "window", window = 6, count = "frequency", tri = FALSE)
  f <- fem(fcm1, pre_trained = cr_glove_subset, transform = TRUE,
           transform_matrix = cr_transform, verbose = FALSE)
  expect_s4_class(f, "fem")
  expect_equal(ncol(f), ncol(cr_glove_subset))
})

test_that("get_local_vocab() returns the shared vocabulary", {
  ctx <- c("immigration reform debate", "zzqq immigration aabb")
  v <- get_local_vocab(ctx, pre_trained = cr_glove_subset)
  expect_type(v, "character")
  expect_true("immigration" %in% v)
  expect_false("zzqq" %in% v)            # not in pre-trained embeddings
})

test_that("feature_sim() returns 1 for identical embeddings", {
  sub <- cr_glove_subset[1:10, ]
  fs <- feature_sim(x = sub, y = sub)
  expect_s3_class(fs, "data.frame")
  expect_true(all(abs(fs$value - 1) < 1e-8))
})

build_immig <- function() {
  toks <- tokens(cr_sample_corpus)
  immig_toks <- tokens_context(toks, "immigration", window = 6L, verbose = FALSE)
  d <- dem(dfm(immig_toks), pre_trained = cr_glove_subset, transform = TRUE,
           transform_matrix = cr_transform, verbose = FALSE)
  list(toks = immig_toks, dem = d)
}

test_that("cos_sim() returns target/feature/value", {
  d <- build_immig()$dem
  wv <- dem_group(d, groups = d@docvars$party)
  cs <- cos_sim(wv, pre_trained = cr_glove_subset, features = c("reform", "enforce"),
                as_list = FALSE)
  expect_s3_class(cs, "data.frame")
  expect_true(all(c("target", "feature", "value") %in% names(cs)))
  expect_setequal(unique(cs$feature), c("reform", "enforce"))
})

test_that("dem_sample() returns a dem of the requested size", {
  d <- build_immig()$dem
  set.seed(1)
  s <- dem_sample(d, size = 0.5, replace = FALSE)
  expect_s4_class(s, "dem")
  expect_equal(nrow(s), round(0.5 * nrow(d)))
  expect_equal(ncol(s), ncol(d))
})

test_that("get_nns() runs (non-bootstrap) and returns nns per group", {
  immig <- build_immig()$toks
  res <- get_nns(x = immig, N = 5, groups = docvars(immig, "party"),
                 candidates = character(0), pre_trained = cr_glove_subset,
                 transform = TRUE, transform_matrix = cr_transform,
                 bootstrap = FALSE, as_list = FALSE)
  expect_s3_class(res, "data.frame")
  expect_setequal(unique(res$target), c("D", "R"))
})
