# Tests for embed_target() transform/transform_matrix handling
library(quanteda)

test_that("embed_target(transform = FALSE) does not require a transform_matrix", {
  pt <- cr_glove_subset[c("immigration", "border", "reform"), ]
  ctx <- c("immigration border reform", "immigration reform")
  expect_no_error(embed_target(context = ctx, pre_trained = pt, transform = FALSE, verbose = FALSE))
})

test_that("embed_target(transform = TRUE) requires a transform_matrix", {
  pt <- cr_glove_subset[c("immigration", "border", "reform"), ]
  ctx <- "immigration reform"
  expect_error(
    embed_target(context = ctx, pre_trained = pt, transform = TRUE, verbose = FALSE),
    "transform_matrix must be provided"
  )
})
