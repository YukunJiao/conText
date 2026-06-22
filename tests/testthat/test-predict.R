# Tests for predict.conText()
library(quanteda)

fit_model <- function(formula) {
  toks <- tokens(cr_sample_corpus)
  set.seed(2021L)
  conText(formula, data = toks, pre_trained = cr_glove_subset, transform = TRUE,
          transform_matrix = cr_transform, jackknife = FALSE, permute = FALSE, verbose = FALSE)
}

test_that("predict() with no newdata returns the intercept (base category) embedding", {
  m <- fit_model(immigration ~ party + gender)
  b <- predict(m)
  expect_equal(dim(b), c(1L, ncol(as.matrix(m))))
  expect_equal(unname(b[1, ]), unname(as.matrix(m)["(Intercept)", ]))
})

test_that("predict() combines coefficient rows for covariate profiles", {
  m <- fit_model(immigration ~ party + gender)
  B <- as.matrix(m)
  wv <- predict(m, newdata = data.frame(party_R = c(0, 1), gender_M = c(0, 1),
                                        row.names = c("DF", "RM")))
  expect_equal(nrow(wv), 2L)
  expect_equal(unname(wv["DF", ]), unname(B["(Intercept)", ]))
  expect_equal(unname(wv["RM", ]), unname(B["(Intercept)", ] + B["party_R", ] + B["gender_M", ]))
})

test_that("predict() auto-fills interaction columns from their components", {
  m <- fit_model(immigration ~ party * gender)
  skip_if_not("party_R:gender_M" %in% rownames(as.matrix(m)))
  B <- as.matrix(m)
  wv <- predict(m, newdata = data.frame(party_R = c(1, 0), gender_M = c(1, 1),
                                        row.names = c("RM", "DM")))
  # RM gets the interaction term; DM (only one component active) does not
  expect_equal(unname(wv["RM", ]),
               unname(B["(Intercept)", ] + B["party_R", ] + B["gender_M", ] + B["party_R:gender_M", ]))
  expect_equal(unname(wv["DM", ]), unname(B["(Intercept)", ] + B["gender_M", ]))
})

test_that("predict() output works with nns()", {
  m <- fit_model(immigration ~ party + gender)
  wv <- predict(m, newdata = data.frame(party_R = c(0, 1), row.names = c("D", "R")))
  nn <- nns(wv, N = 3, pre_trained = cr_glove_subset, as_list = FALSE, show_language = FALSE)
  expect_setequal(unique(nn$target), c("D", "R"))
})

test_that("predict() errors on unknown covariates and respects intercept = FALSE", {
  m <- fit_model(immigration ~ party + gender)
  expect_error(predict(m, newdata = data.frame(partyR = 1)), "not coefficients")
  with_int <- predict(m, data.frame(party_R = 1), intercept = TRUE)
  no_int <- predict(m, data.frame(party_R = 1), intercept = FALSE)
  expect_false(isTRUE(all.equal(with_int, no_int)))
})
