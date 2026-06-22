# S3 methods for conText-class objects -----------------------------------------

# re-export the broom::tidy generic so tidy(model) works when conText is attached
#' @importFrom broom tidy
#' @export
broom::tidy

#' Tidy a conText model
#'
#' Returns the normed-coefficient table of a [conText()] model as a tibble, for
#' use with the broom / tidyverse workflow.
#'
#' @param x a `conText-class` object returned by [conText()].
#' @param ... unused.
#'
#' @return a `tibble` with one row per (non-intercept) coefficient and the
#' columns of the model's normed-coefficient table (e.g. `coefficient`,
#' `normed.estimate.deflated`, and, when available, `std.error`, `lower.ci`,
#' `upper.ci`, `p.value`).
#'
#' @importFrom broom tidy
#' @method tidy conText
#' @export
#' @rdname tidy.conText
#' @keywords tidy.conText
#' @examples
#'
#' library(quanteda)
#' toks <- tokens(cr_sample_corpus)
#' set.seed(2021L)
#' model <- conText(immigration ~ party + gender, data = toks,
#'                  pre_trained = cr_glove_subset, transform = TRUE,
#'                  transform_matrix = cr_transform, jackknife = FALSE,
#'                  permute = FALSE, verbose = FALSE)
#' tidy(model)
tidy.conText <- function(x, ...){
  dplyr::as_tibble(x@normed_coefficients)
}

#' Summarize a conText model
#'
#' @param object a `conText-class` object returned by [conText()].
#' @param ... unused.
#'
#' @return invisibly, the model's normed-coefficient `data.frame`. Prints the
#' model dimensions and the normed-coefficient table.
#'
#' @method summary conText
#' @export
#' @rdname summary.conText
#' @keywords summary.conText
summary.conText <- function(object, ...){
  cat("conText embedding regression\n")
  cat(sprintf("  coefficients (incl. intercept): %d | embedding dimensions: %d | features: %d\n\n",
              nrow(object), ncol(object), length(object@features)))
  print(dplyr::as_tibble(object@normed_coefficients))
  invisible(object@normed_coefficients)
}

#' Plot a conText model's coefficients
#'
#' Plots the deflated norm of each (non-intercept) coefficient, with confidence
#' intervals when the model was fit with `jackknife = TRUE`. When permutation
#' p-values are available, coefficients significant at the 0.05 level are marked
#' with a `*`.
#'
#' @param x a `conText-class` object returned by [conText()].
#' @param ... unused.
#'
#' @return a `ggplot` object.
#'
#' @method plot conText
#' @export
#' @rdname plot.conText
#' @keywords plot.conText
#' @examples
#'
#' library(quanteda)
#' toks <- tokens(cr_sample_corpus)
#' set.seed(2021L)
#' model <- conText(immigration ~ party + gender, data = toks,
#'                  pre_trained = cr_glove_subset, transform = TRUE,
#'                  transform_matrix = cr_transform, jackknife = FALSE,
#'                  permute = TRUE, num_permutations = 100, verbose = FALSE)
#' plot(model)
plot.conText <- function(x, ...){
  df <- as.data.frame(x@normed_coefficients)
  if(nrow(df) == 0) stop("no (non-intercept) coefficients to plot.", call. = FALSE)

  # mark significant coefficients with a star (matching plot_nns_ratio convention)
  if("p.value" %in% names(df))
    df$coefficient <- ifelse(df$p.value < 0.05, paste0(df$coefficient, "*"), df$coefficient)

  # preserve table order top-to-bottom
  df$coefficient <- factor(df$coefficient, levels = rev(df$coefficient))

  has_ci <- all(c("lower.ci", "upper.ci") %in% names(df))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = normed.estimate.deflated, y = coefficient))
  if(has_ci)
    p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower.ci, xmax = upper.ci), height = 0.15)
  p +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(x = "deflated norm of beta", y = NULL) +
    ggplot2::theme_bw()
}
