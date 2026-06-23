# Tests for SIF weighting in dem() and conText()
library(quanteda)

immig_dfm <- function() {
  toks <- tokens(cr_sample_corpus)
  dfm(tokens_context(toks, "immigration", window = 6L, verbose = FALSE))
}

test_that("dem() default weighting is unchanged and 'sif' differs", {
  d <- immig_dfm()
  unif <- dem(d, pre_trained = cr_glove_subset, transform = TRUE,
              transform_matrix = cr_transform, verbose = FALSE)
  sif  <- dem(d, pre_trained = cr_glove_subset, transform = TRUE,
              transform_matrix = cr_transform, weighting = "sif", verbose = FALSE)
  expect_equal(dim(unif), dim(sif))
  expect_identical(unif@docvars, sif@docvars)
  expect_false(isTRUE(all.equal(as.matrix(unif), as.matrix(sif))))
})

test_that("dem() rejects an invalid weighting", {
  d <- immig_dfm()
  expect_error(
    dem(d, pre_trained = cr_glove_subset, transform = FALSE, weighting = "nope", verbose = FALSE)
  )
})

test_that("conText() threads weighting through to the embeddings", {
  toks <- tokens(cr_sample_corpus)
  set.seed(1)
  m_unif <- conText(immigration ~ party, data = toks, pre_trained = cr_glove_subset,
                    transform_matrix = cr_transform, jackknife = FALSE, permute = FALSE, verbose = FALSE)
  set.seed(1)
  m_sif <- conText(immigration ~ party, data = toks, pre_trained = cr_glove_subset,
                   transform_matrix = cr_transform, jackknife = FALSE, permute = FALSE,
                   weighting = "sif", verbose = FALSE)
  expect_false(isTRUE(all.equal(unname(as.matrix(m_unif)), unname(as.matrix(m_sif)))))
})
