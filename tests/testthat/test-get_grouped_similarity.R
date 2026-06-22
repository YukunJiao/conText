# Regression tests for get_grouped_similarity() (issue #26)
library(quanteda)

make_corp <- function() {
  corp <- cr_sample_corpus
  quanteda::docvars(corp, "year") <- rep(2011:2014, each = 50)
  corp
}

test_that("get_grouped_similarity() returns group/val without bootstrap", {
  r <- get_grouped_similarity(make_corp(), group_var = "year", target = "immigration",
                              first_vec = c("reform"), second_vec = NULL,
                              pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                              window = 6L)
  expect_setequal(names(r), c("group", "val"))
})

test_that("single-vector and two-vector calls both return a 'val' column", {
  one <- get_grouped_similarity(make_corp(), group_var = "year", target = "immigration",
                                first_vec = c("reform"), second_vec = NULL,
                                pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                                window = 6L)
  two <- get_grouped_similarity(make_corp(), group_var = "year", target = "immigration",
                                first_vec = c("reform"), second_vec = c("enforce"),
                                pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                                window = 6L)
  expect_true("val" %in% names(one))
  expect_true("val" %in% names(two))
})

test_that("bootstrap = TRUE adds ordered confidence intervals (issue #26)", {
  set.seed(1)
  r <- get_grouped_similarity(make_corp(), group_var = "year", target = "immigration",
                              first_vec = c("reform"), second_vec = c("enforce"),
                              pre_trained = cr_glove_subset, transform_matrix = cr_transform,
                              window = 6L, bootstrap = TRUE, num_bootstraps = 100)
  expect_true(all(c("group", "val", "std.error", "lower.ci", "upper.ci") %in% names(r)))
  expect_true(all(r$lower.ci <= r$val & r$val <= r$upper.ci))
})
