# Tests for dem_whiten()
library(quanteda)

aniso_dem <- function() {
  set.seed(1)
  shared <- matrix(rep(rnorm(20), each = 120), nrow = 120) # strong shared direction
  E <- shared + matrix(rnorm(120 * 20, sd = 0.3), nrow = 120)
  as_dem(E, docvars = data.frame(g = rep(c("a", "b"), 60)))
}

mean_abs_cos <- function(m) {
  m <- as.matrix(m)
  s <- text2vec::sim2(m, m, method = "cosine", norm = "l2")
  mean(abs(s[upper.tri(s)]))
}

test_that("dem_whiten() reduces anisotropy", {
  d <- aniso_dem()
  raw <- mean_abs_cos(d)
  for (method in c("all-but-the-top", "center", "zscore")) {
    dw <- dem_whiten(d, method = method)
    expect_lt(mean_abs_cos(dw), raw)
  }
})

test_that("dem_whiten() returns a dem with the same shape, ids and docvars", {
  d <- aniso_dem()
  dw <- dem_whiten(d)
  expect_s4_class(dw, "dem")
  expect_equal(dim(dw), dim(d))
  expect_identical(rownames(as.matrix(dw)), rownames(as.matrix(d)))
  expect_identical(dw@docvars, d@docvars)
})

test_that("dem_whiten() honors n_components and caps it safely", {
  d <- aniso_dem()
  expect_s4_class(dem_whiten(d, method = "all-but-the-top", n_components = 3), "dem")
  # n_components larger than dimensions is capped, not an error
  expect_s4_class(dem_whiten(d, method = "all-but-the-top", n_components = 999), "dem")
})

test_that("dem_whiten() output works in conText()", {
  d <- aniso_dem()
  m <- conText(. ~ g, data = dem_whiten(d), jackknife = FALSE, permute = FALSE, verbose = FALSE)
  expect_equal(rownames(m), c("(Intercept)", "g_b"))
})

test_that("dem_whiten() errors on non-dem input", {
  expect_error(dem_whiten(matrix(rnorm(20), 4)), "must be a dem")
})
