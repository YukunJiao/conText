# Tests for as_dem() and conText()'s precomputed-embedding (dem) input
library(quanteda)

test_that("as_dem() builds a dem from a matrix + docvars", {
  set.seed(1)
  E <- matrix(rnorm(20), nrow = 4)
  dv <- data.frame(party = c("D", "D", "R", "R"))
  d <- as_dem(E, docvars = dv)
  expect_s4_class(d, "dem")
  expect_equal(dim(d), c(4L, 5L))
  expect_equal(d@docvars$party, c("D", "D", "R", "R"))
})

test_that("as_dem() errors when docvars rows do not match", {
  E <- matrix(rnorm(20), nrow = 4)
  expect_error(as_dem(E, docvars = data.frame(party = c("D", "R"))), "must equal nrow")
})

test_that("conText() on a precomputed dem matches the tokens path exactly", {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  m_tok <- conText(immigration ~ party + gender, data = toks, pre_trained = cr_glove_subset,
                   transform = TRUE, transform_matrix = cr_transform,
                   jackknife = FALSE, permute = FALSE, verbose = FALSE)

  ctx <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)
  d <- dem(dfm(ctx), pre_trained = cr_glove_subset, transform = TRUE,
           transform_matrix = cr_transform, verbose = FALSE)
  set.seed(1)
  m_dem <- conText(. ~ party + gender, data = d, jackknife = FALSE, permute = FALSE, verbose = FALSE)

  expect_equal(unname(as.matrix(m_tok)), unname(as.matrix(m_dem)), tolerance = 1e-10)
  expect_identical(rownames(m_tok), rownames(m_dem))
  nc <- sapply(m_tok@normed_coefficients, is.numeric)
  expect_equal(m_tok@normed_coefficients[nc], m_dem@normed_coefficients[nc], tolerance = 1e-8)
})

test_that("conText() runs full inference on an as_dem() of arbitrary embeddings", {
  set.seed(2)
  E <- matrix(rnorm(80 * 20), nrow = 80)
  dv <- data.frame(g = rep(c("a", "b"), 40))
  m <- conText(. ~ g, data = as_dem(E, docvars = dv),
               jackknife = TRUE, permute = TRUE, num_permutations = 100, verbose = FALSE)
  expect_true(all(c("std.error", "lower.ci", "upper.ci", "p.value") %in% names(m@normed_coefficients)))
  expect_equal(rownames(m), c("(Intercept)", "g_b"))
})

test_that("conText() requires a `.` LHS when data is a dem", {
  d <- as_dem(matrix(rnorm(20), 4), docvars = data.frame(g = c("a", "a", "b", "b")))
  expect_error(conText(immigration ~ g, data = d, verbose = FALSE), "left-hand side must be")
})
