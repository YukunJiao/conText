# Regression tests for conText() interaction support (issue #18)
library(quanteda)

test_that("conText() additive model produces the expected coefficients", {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  m <- conText(immigration ~ party + gender, data = toks, pre_trained = cr_glove_subset,
               transform = TRUE, transform_matrix = cr_transform,
               jackknife = FALSE, permute = FALSE, verbose = FALSE)
  expect_setequal(rownames(m), c("(Intercept)", "party_R", "gender_M"))
})

test_that("conText() supports interaction terms via * (issue #18)", {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  m <- conText(immigration ~ party * gender, data = toks, pre_trained = cr_glove_subset,
               transform = TRUE, transform_matrix = cr_transform,
               jackknife = FALSE, permute = FALSE, verbose = FALSE)
  expect_true("party_R:gender_M" %in% rownames(m))
  # interaction also gets its own normed estimate
  expect_true("party_R:gender_M" %in% m@normed_coefficients$coefficient)
  # labels are not backtick-quoted
  expect_false(any(grepl("`", rownames(m), fixed = TRUE)))
})

test_that("* and : produce the same coefficient set", {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  m_star <- conText(immigration ~ party * gender, data = toks, pre_trained = cr_glove_subset,
                    transform = TRUE, transform_matrix = cr_transform,
                    jackknife = FALSE, permute = FALSE, verbose = FALSE)
  set.seed(1)
  m_colon <- conText(immigration ~ party + gender + party:gender, data = toks,
                     pre_trained = cr_glove_subset, transform = TRUE,
                     transform_matrix = cr_transform,
                     jackknife = FALSE, permute = FALSE, verbose = FALSE)
  expect_setequal(rownames(m_star), rownames(m_colon))
})
