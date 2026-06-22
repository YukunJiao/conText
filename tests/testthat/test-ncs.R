# Regression tests for ncs() / get_ncs() (issue #12)
library(quanteda)

build_immig_dem <- function() {
  toks <- tokens(cr_sample_corpus)
  immig <- tokens_context(toks, pattern = "immigration", window = 6L,
                          rm_keyword = FALSE, verbose = FALSE)
  d <- dem(dfm(immig), pre_trained = cr_glove_subset, transform = TRUE,
           transform_matrix = cr_transform, verbose = FALSE)
  list(immig = immig, dem = d)
}

test_that("ncs(group_var=) draws each target's contexts only from its own group (issue #12)", {
  obj <- build_immig_dem()
  d <- obj$dem
  wv <- dem_group(d, groups = d@docvars$party)
  # contexts = NULL -> the 'context' column holds the (unambiguous) doc ids
  res <- ncs(x = wv, contexts_dem = d, contexts = NULL, N = 5,
             as_list = FALSE, group_var = "party")
  grp_by_doc <- setNames(as.character(d@docvars$party), rownames(as.matrix(d)))
  expect_true(all(res$target == grp_by_doc[as.character(res$context)]))
})

test_that("ncs() default behaviour is unchanged when group_var is NULL", {
  obj <- build_immig_dem()
  d <- obj$dem
  wv <- dem_group(d, groups = d@docvars$party)
  res <- ncs(x = wv, contexts_dem = d, contexts = obj$immig, N = 5, as_list = FALSE)
  expect_setequal(names(res), c("target", "context", "rank", "value"))
  expect_setequal(unique(res$target), c("D", "R"))
})

test_that("get_ncs(bootstrap = FALSE) runs (regression: undefined contexts_dem)", {
  obj <- build_immig_dem()
  expect_no_error(
    get_ncs(x = obj$immig, N = 3, groups = docvars(obj$immig, "party"),
            pre_trained = cr_glove_subset, transform = TRUE,
            transform_matrix = cr_transform, bootstrap = FALSE, as_list = FALSE)
  )
})
