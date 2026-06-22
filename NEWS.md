# conText 3.1.0

## New features

* `conText()` supports interaction terms in the formula, using the usual syntax:
  `*` (e.g. `immigration ~ party*gender` expands to `party + gender + party:gender`)
  or `:` for an interaction on its own (#18).
* `conText()` accepts a precomputed `dem-class` object of per-instance embeddings
  as `data` (with `.` on the left-hand side of the formula), making embedding
  regression usable with embeddings computed outside conText -- e.g. contextual
  embeddings from a transformer (BERT/RoBERTa/ModernBERT) or decontextualized
  static embeddings. `pre_trained`/`transform_matrix` are not required in that case.
* `as_dem()`: wrap an arbitrary N x D embedding matrix (with optional docvars) as a
  `dem`, for use with `conText()`, `nns()`, `cos_sim()`, etc.
* `predict()` method for `conText` models: combine the regression coefficients into
  the implied ALC embedding(s) for one or more covariate profiles (interactions are
  auto-filled from their components; continuous covariates are supported). The output
  feeds directly into `nns()`/`cos_sim()`.
* `tidy()`, `summary()` and `plot()` methods for `conText` models.
* `get_grouped_similarity()` gained optional bootstrap confidence intervals
  (`bootstrap`, `num_bootstraps`, `confidence_level`) (#26).
* `ncs()` gained a `group_var` argument that restricts each group's nearest contexts
  to contexts belonging to that group (#12).

## Bug fixes

* **Bootstrap percentile confidence intervals** in `get_cos_sim()`, `get_ncs()`,
  `get_nns()`, `get_nns_ratio()`, `get_grouped_similarity()`, `bootstrap_nns()` and
  `contrast_nns()` used the `(1 - confidence_level)` and `confidence_level` quantiles
  instead of `(1 - confidence_level)/2` and `(1 + confidence_level)/2`, so a requested
  95% interval actually covered only 90% (always too narrow). They now use the standard
  percentile bootstrap, matching `boot::boot.ci(type = "perc")`. **This changes the
  reported confidence-interval values.**
* `compute_transform(weighting = "log")` no longer errors with a "subscript out of
  bounds" message for off-the-shelf or non-English pre-trained embeddings whose
  feature frequencies contain `NA`/unnamed entries; it now gives an informative error
  when too few features overlap to estimate the transformation matrix (#28).
* `get_ncs(bootstrap = FALSE)` no longer errors (it referenced an undefined object).
* Candidate handling: `candidates = character(0)` is treated as "all candidates"
  (consistent across `nns()`, `find_nns()`, `bootstrap_nns()`, etc.); subsetting to a
  single candidate no longer collapses the matrix; and `bootstrap_nns()`, `find_nns()`,
  `prototypical_context()`, `nns_ratio()` and `get_nns_ratio()` no longer pad their
  output with `NA` rows when fewer than `N` neighbors/contexts are available.
* `dem_group()` no longer misaligns group means when `groups` is a factor with unused
  levels.
* The grouping-variable setup in `get_cos_sim()`, `get_ncs()`, `get_nns()` and
  `get_nns_ratio()` no longer deletes a user's own `group` document variable when
  `groups = NULL` (missing braces around an `if`).
* `feature_sim()` prints the "missing features" notice once rather than once per
  missing feature.
* `get_grouped_similarity()`: fixed a self-referential `window` default and a
  single-vector result column named `first_val` instead of the documented `val`.

## Documentation and infrastructure

* `compute_transform()` is documented as the implementation of the ALC
  transformation-matrix estimator and its use for other languages (#27); `dem` rows
  are clarified as individual instances rather than distinct documents (#13).
* Added a `testthat` test suite covering the fixes and new features.
