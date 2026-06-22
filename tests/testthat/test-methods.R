# Tests for tidy()/summary()/plot() methods on conText objects
library(quanteda)

fit <- function(jackknife = FALSE, permute = FALSE, num_permutations = 100) {
  set.seed(2021L)
  # subsample documents so jackknife/permute stay fast in tests
  toks <- tokens(cr_sample_corpus[sample(quanteda::ndoc(cr_sample_corpus), 80)])
  set.seed(2021L)
  conText(immigration ~ party + gender, data = toks, pre_trained = cr_glove_subset,
          transform = TRUE, transform_matrix = cr_transform,
          jackknife = jackknife, permute = permute, num_permutations = num_permutations,
          verbose = FALSE)
}

test_that("tidy() returns the normed-coefficient table as a tibble", {
  m <- fit()
  td <- tidy(m)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("coefficient", "normed.estimate.deflated") %in% names(td)))
  expect_equal(nrow(td), nrow(m@normed_coefficients))
})

test_that("summary() prints and returns the normed table invisibly", {
  m <- fit()
  expect_output(s <- summary(m), "conText embedding regression")
  expect_s3_class(s, "data.frame")
})

test_that("plot() returns a ggplot for both jackknife and non-jackknife models", {
  expect_s3_class(plot(fit()), "ggplot")
  m_ci <- fit(jackknife = TRUE)
  p <- plot(m_ci)
  expect_s3_class(p, "ggplot")
})

test_that("plot() stars significant coefficients when p-values are present", {
  m <- fit(permute = TRUE, num_permutations = 100)
  p <- plot(m)
  # the built plot's data should carry starred labels for any significant coef
  starred <- grepl("\\*", as.character(p$data$coefficient))
  expect_equal(sum(starred), sum(m@normed_coefficients$p.value < 0.05))
})
