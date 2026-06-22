Embedding Regression with conText
================

This tutorial walks through the **embedding regression** workflow in
**conText** end to end: from a tokenized corpus to a fitted model, to
reading and visualizing the results, predicting embeddings for covariate
profiles, and tracing how a term’s usage changes along a continuous
covariate. It complements the [Quick Start Guide](quickstart.md), which
focuses on the descriptive (“nearest neighbours”) tools.

We use the small objects bundled with the package – `cr_sample_corpus`,
`cr_glove_subset` and `cr_transform` – so everything here is
reproducible. They are tiny, so treat the numbers as illustrative only.

``` r
library(conText)
library(quanteda)
library(dplyr)
library(ggplot2)
```

# What embedding regression does

For a focal term, conText represents **each instance** of that term by
an “*a la carte*” (ALC) embedding: the average of the pre-trained
embeddings of the words in its context, multiplied by a transformation
matrix `A`. Stacking those per-instance embeddings as the outcome `Y`,
conText fits

![\\mathbf{Y} = \\mathbf{X}\\boldsymbol{\\beta} + \\mathbf{E},](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%7BY%7D%20%3D%20%5Cmathbf%7BX%7D%5Cboldsymbol%7B%5Cbeta%7D%20%2B%20%5Cmathbf%7BE%7D%2C "\mathbf{Y} = \mathbf{X}\boldsymbol{\beta} + \mathbf{E},")

a multivariate regression of the embeddings on document-level covariates
`X`. Each coefficient `β` is itself a `D`-dimensional vector; its
(debiased, squared) **norm** measures how much that covariate shifts the
term’s usage, and conText attaches inference to those norms (a
permutation test for p-values and, optionally, a jackknife for
confidence intervals).

# Fitting a model

`conText()` takes a formula with the focal term on the left-hand side
and the covariates on the right, plus a tokenized corpus and the
pre-trained embeddings / transformation matrix.

``` r
toks <- tokens(cr_sample_corpus)

set.seed(2021L)
model <- conText(immigration ~ party + gender,
                 data = toks,
                 pre_trained = cr_glove_subset,
                 transform = TRUE, transform_matrix = cr_transform,
                 jackknife = FALSE,        # set TRUE to add confidence intervals (slower)
                 permute = TRUE, num_permutations = 100,
                 verbose = FALSE)
```

Character/factor covariates are automatically turned into `0/1`
indicator variables, leaving out a base category. Here `party` becomes
`party_R` (base = Democrat) and `gender` becomes `gender_M` (base =
Female).

# Reading the results

The fitted object is a matrix of coefficients (one row per covariate,
including the intercept). Three methods make it easy to inspect:

`summary()` prints the model dimensions and the normed-coefficient
table:

``` r
summary(model)
```

    ## conText embedding regression
    ##   coefficients (incl. intercept): 3 | embedding dimensions: 300 | features: 488
    ## 
    ## # A tibble: 2 x 8
    ##   coefficient normed.estimate.orig normed.estimate.defl~1 normed.estimate.beta~2
    ##   <chr>                      <dbl>                  <dbl>                  <dbl>
    ## 1 party_R                    10.1                    8.67                   1.47
    ## 2 gender_M                    6.91                   5.15                   1.75
    ## # i abbreviated names: 1: normed.estimate.deflated,
    ## #   2: normed.estimate.beta.error.null
    ## # i 4 more variables: n <int>, n_obs <int>, covariate_mean <dbl>, p.value <dbl>

`tidy()` returns that table as a tibble (handy for further manipulation
or plotting):

``` r
tidy(model)
```

    ## # A tibble: 2 x 8
    ##   coefficient normed.estimate.orig normed.estimate.defl~1 normed.estimate.beta~2
    ##   <chr>                      <dbl>                  <dbl>                  <dbl>
    ## 1 party_R                    10.1                    8.67                   1.47
    ## 2 gender_M                    6.91                   5.15                   1.75
    ## # i abbreviated names: 1: normed.estimate.deflated,
    ## #   2: normed.estimate.beta.error.null
    ## # i 4 more variables: n <int>, n_obs <int>, covariate_mean <dbl>, p.value <dbl>

`plot()` shows the deflated norm of each coefficient. Coefficients
significant at the 0.05 level (using the permutation p-values) are
marked with a `*`; when the model is fit with `jackknife = TRUE`,
confidence intervals are drawn as well.

``` r
plot(model)
```

![](embedding-regression_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Interaction effects

Interactions use the usual formula syntax. `party*gender` expands to
`party + gender + party:gender`; the interaction enters as the product
of the corresponding indicator columns and gets its own coefficient and
test.

``` r
set.seed(2021L)
model_int <- conText(immigration ~ party * gender,
                     data = toks,
                     pre_trained = cr_glove_subset,
                     transform = TRUE, transform_matrix = cr_transform,
                     jackknife = FALSE, permute = TRUE, num_permutations = 100,
                     verbose = FALSE)

rownames(model_int)
```

    ## [1] "(Intercept)"      "party_R"          "gender_M"         "party_R:gender_M"

# Predicting embeddings for covariate profiles

The coefficients combine into the ALC embedding for any covariate
profile. Rather than adding rows by hand, use `predict()` with a
`newdata` of profiles (columns are the coefficient names; interaction
columns are filled automatically from their components).

``` r
group_wvs <- predict(model_int,
                     newdata = data.frame(
                       party_R  = c(0, 0, 1, 1),
                       gender_M = c(0, 1, 0, 1),
                       row.names = c("Dem-Female", "Dem-Male", "Rep-Female", "Rep-Male")))
dim(group_wvs)
```

    ## [1]   4 300

The result is an ordinary matrix (one row per profile) that feeds
directly into the descriptive tools. For example, the nearest neighbours
of each group’s embedding:

``` r
nns(group_wvs, N = 5, pre_trained = cr_glove_subset, as_list = FALSE)
```

    ## # A tibble: 20 x 4
    ##    target     feature        rank value
    ##    <fct>      <chr>         <int> <dbl>
    ##  1 Rep-Male   immigration       1 0.879
    ##  2 Dem-Female immigration       1 0.849
    ##  3 Dem-Male   immigration       1 0.803
    ##  4 Rep-Female immigration       1 0.786
    ##  5 Dem-Male   broken            2 0.706
    ##  6 Rep-Male   illegal           2 0.701
    ##  7 Dem-Male   reform            3 0.685
    ##  8 Rep-Female illegal           2 0.674
    ##  9 Dem-Male   comprehensive     4 0.671
    ## 10 Rep-Female enforcement       3 0.670
    ## 11 Dem-Female immigrants        2 0.660
    ## 12 Rep-Female border            4 0.656
    ## 13 Rep-Male   enforce           3 0.639
    ## 14 Rep-Male   amnesty           4 0.635
    ## 15 Rep-Male   immigrants        5 0.634
    ## 16 Dem-Female enforcement       3 0.621
    ## 17 Rep-Female laws              5 0.615
    ## 18 Dem-Female legal             4 0.603
    ## 19 Dem-Female law               5 0.601
    ## 20 Dem-Male   illegal           5 0.560

# Continuous covariates: usage along a dimension

`predict()` also accepts continuous covariates, so you can trace how a
term’s usage moves along a scale – for instance the first dimension of
DW-NOMINATE (`nominate_dim1`), included in the sample corpus.

``` r
set.seed(2021L)
model_nom <- conText(immigration ~ nominate_dim1,
                     data = toks,
                     pre_trained = cr_glove_subset,
                     transform = TRUE, transform_matrix = cr_transform,
                     jackknife = FALSE, permute = TRUE, num_permutations = 100,
                     verbose = FALSE)
```

Predict the ALC embedding at a grid of NOMINATE values, then measure its
cosine similarity to a few features of interest:

``` r
grid <- seq(-1, 1, by = 0.2)
nom_wvs <- predict(model_nom, newdata = data.frame(nominate_dim1 = grid,
                                                   row.names = as.character(grid)))

effects <- cos_sim(nom_wvs, pre_trained = cr_glove_subset,
                   features = c("reform", "enforce"), as_list = FALSE)
head(effects)
```

    ##   target feature     value
    ## 1   -1.0  reform 0.6855243
    ## 2   -0.8  reform 0.6831449
    ## 3   -0.6  reform 0.6741428
    ## 4   -0.4  reform 0.6575746
    ## 5   -0.2  reform 0.6330861
    ## 6    0.0  reform 0.6010969

Because `cos_sim()` returns a tidy data frame, plotting the “effect” of
the covariate is a one-liner:

``` r
effects %>%
  mutate(nominate = as.numeric(target)) %>%
  ggplot(aes(nominate, value, color = feature)) +
  geom_line() + geom_point() +
  labs(x = "DW-NOMINATE (dim 1)", y = "cosine similarity to feature",
       color = NULL) +
  theme_bw()
```

![](embedding-regression_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Exploratory tools with uncertainty

Alongside the regression, the `get_*()` wrappers go from a tokenized
corpus to group-level nearest neighbours / similarities, with bootstrap
standard errors and confidence intervals. The intervals are standard
percentile-bootstrap intervals at the requested `confidence_level`
(e.g. a 95% interval uses the 2.5th and 97.5th percentiles).

``` r
immig_toks <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)

set.seed(2021L)
immig_nns <- get_nns(x = immig_toks, N = 5,
                     groups = docvars(immig_toks, "party"),
                     candidates = character(0),
                     pre_trained = cr_glove_subset,
                     transform = TRUE, transform_matrix = cr_transform,
                     bootstrap = TRUE, num_bootstraps = 100,
                     confidence_level = 0.95,
                     as_list = FALSE)
```

    ## starting bootstraps 
    ## done with bootstraps

``` r
head(immig_nns)
```

    ## # A tibble: 6 x 7
    ##   target feature        rank value std.error lower.ci upper.ci
    ##   <fct>  <chr>         <int> <dbl>     <dbl>    <dbl>    <dbl>
    ## 1 D      immigration       1 0.843   0.0107     0.822    0.865
    ## 2 D      broken            2 0.675   0.0138     0.647    0.701
    ## 3 D      comprehensive     3 0.655   0.0141     0.622    0.677
    ## 4 D      reform            4 0.652   0.0157     0.617    0.676
    ## 5 D      immigrants        5 0.611   0.0148     0.584    0.638
    ## 6 R      immigration       1 0.867   0.00927    0.847    0.882

# Notes on inference and performance

-   **Test statistic.** conText tests the *debiased* squared norm of
    each coefficient (`normed.estimate.deflated`), which subtracts an
    estimate of the sampling-noise contribution from the raw norm (Green
    et al. 2025). The raw norm (`normed.estimate.orig`) is also
    reported.
-   **p-values vs. intervals.** `permute = TRUE` gives empirical
    p-values via a permutation test; `jackknife = TRUE` adds standard
    errors and confidence intervals.
-   **Clustering.** Pass `cluster_variable` to cluster the standard
    errors (e.g. by speaker), so repeated instances from the same unit
    are not treated as independent.
-   **Performance.** The jackknife is leave-one-out, so on large corpora
    it can be slow; use `jackknife_fraction` to subsample, or
    `parallel = TRUE` with a registered backend.

# References

Rodriguez, P. L., Spirling, A., and Stewart, B. M. (2023). Embedding
Regression: Models for Context-Specific Description and Inference.
*American Political Science Review*, 117(4), 1255-1274.

Khodak, M., Saunshi, N., Liang, Y., Ma, T., Stewart, B., and Arora, S.
(2018). A La Carte Embedding: Cheap but Effective Induction of Semantic
Feature Vectors. *ACL*.
