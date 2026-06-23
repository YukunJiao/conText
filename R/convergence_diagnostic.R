#' Diagnose how quickly an ALC embedding stabilizes with sample size
#'
#' For a focal term's per-instance ALC embeddings, assesses how many instances are
#' needed before the averaged ('a la carte') embedding stabilizes. For a grid of
#' sample sizes, the function repeatedly draws that many instances, averages them,
#' and measures the cosine similarity of the sub-sample embedding to the
#' full-sample embedding. As the sub-sample approaches the full set the similarity
#' approaches 1; how fast it does so indicates whether enough instances are
#' available -- a practical concern for rare terms, which ALC is often used for.
#'
#' @param x a `dem-class` object: the per-instance ALC embeddings of a single
#' focal term (e.g. from [dem()]).
#' @param sizes (integer) vector of sub-sample sizes to evaluate. Defaults to about
#' ten sizes spread between a small fraction of the instances and all of them.
#' @param n_replicates (integer) number of random sub-samples drawn at each size.
#' @param confidence_level (numeric in (0,1)) confidence level for the interval
#' around the mean similarity at each size.
#' @param norm (character) `"l2"` for cosine similarity, `"none"` for inner product.
#'
#' @return a `data.frame` with one row per size and columns: `n` (sample size),
#' `value` (mean similarity to the full-sample embedding), `std.error`, `lower.ci`
#' and `upper.ci`. Plot `value` against `n` to read off the convergence curve.
#'
#' @export
#' @rdname convergence_diagnostic
#' @keywords convergence_diagnostic
#' @examples
#'
#' library(quanteda)
#' toks <- tokens(cr_sample_corpus)
#' immig_toks <- tokens_context(toks, pattern = "immigration", window = 6L, verbose = FALSE)
#' immig_dem <- dem(dfm(immig_toks), pre_trained = cr_glove_subset,
#'                  transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' set.seed(2021L)
#' conv <- convergence_diagnostic(immig_dem, n_replicates = 10)
#' conv
convergence_diagnostic <- function(x, sizes = NULL, n_replicates = 20, confidence_level = 0.95, norm = "l2"){

  if(!methods::is(x, "dem")) stop("x must be a dem-class object (the per-instance embeddings of a focal term).", call. = FALSE)
  if(confidence_level >= 1 || confidence_level <= 0) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE)

  M <- as.matrix(x)
  n <- nrow(M)
  if(n < 2) stop("x must contain at least 2 instances.", call. = FALSE)

  full <- matrix(Matrix::colMeans(M), nrow = 1)

  if(is.null(sizes)) sizes <- unique(round(seq(max(2, round(n / 10)), n, length.out = 10)))
  sizes <- sort(unique(sizes[sizes >= 1 & sizes <= n]))

  alpha <- 1 - confidence_level
  out <- lapply(sizes, function(s){
    sims <- replicate(n_replicates, {
      idx <- sample.int(n, s, replace = FALSE)
      emb <- matrix(Matrix::colMeans(M[idx, , drop = FALSE]), nrow = 1)
      text2vec::sim2(emb, full, method = "cosine", norm = norm)[1, 1]
    })
    data.frame(n = s,
               value = mean(sims),
               std.error = stats::sd(sims),
               lower.ci = stats::quantile(sims, alpha / 2, names = FALSE),
               upper.ci = stats::quantile(sims, 1 - alpha / 2, names = FALSE))
  })

  do.call(rbind, out)
}
