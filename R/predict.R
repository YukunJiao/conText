#' Predict ALC embeddings for covariate profiles from a conText model
#'
#' Combines the regression coefficients of a [conText()] model into the implied
#' 'a la carte' (ALC) embedding(s) for one or more covariate profiles. This
#' automates the manual addition of coefficient rows (e.g. intercept + `party_R`
#' + `gender_M`) otherwise needed to recover a group's embedding, and works with
#' interaction and continuous covariates.
#'
#' @param object a `conText-class` object returned by [conText()].
#' @param newdata a `data.frame` (or named list) of covariate profiles for which
#' to predict embeddings. Column names must match the model's coefficient names
#' (the dummified indicators, e.g. `party_R`, `gender_M`, or numeric covariates),
#' and each row is one profile. Coefficients not supplied are taken to be 0.
#' Interaction coefficients (e.g. `party_R:gender_M`) are filled automatically as
#' the product of their components when those components are supplied and the
#' interaction column itself is not. If `NULL` (default), the base-category
#' (intercept) embedding is returned.
#' @param intercept (logical) if `TRUE` (default) include the intercept, i.e.
#' return embeddings on the same scale as the underlying ALC embeddings; if
#' `FALSE`, return the covariate contribution only (the difference from the base
#' category).
#' @param ... unused.
#'
#' @return a (numeric) matrix with one row per covariate profile and D columns
#' (the embedding dimensions). The result can be passed directly to [nns()] or
#' [cos_sim()] to interpret each profile's embedding.
#'
#' @importFrom stats predict
#' @method predict conText
#' @export
#' @rdname predict.conText
#' @keywords predict.conText
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # fit an embedding regression
#' set.seed(2021L)
#' model <- conText(immigration ~ party + gender, data = toks,
#'                  pre_trained = cr_glove_subset, transform = TRUE,
#'                  transform_matrix = cr_transform, jackknife = FALSE,
#'                  permute = FALSE, verbose = FALSE)
#'
#' # ALC embeddings for the four party x gender groups
#' wvs <- predict(model, newdata = data.frame(
#'   party_R  = c(0, 0, 1, 1),
#'   gender_M = c(0, 1, 0, 1),
#'   row.names = c("Dem-Female", "Dem-Male", "Rep-Female", "Rep-Male")))
#'
#' # nearest neighbors of each group's embedding
#' nns(wvs, N = 5, pre_trained = cr_glove_subset, as_list = FALSE)
predict.conText <- function(object, newdata = NULL, intercept = TRUE, ...){

  betas <- as.matrix(object) # M x D matrix, rownames = coefficient names
  coef_names <- rownames(betas)
  if(is.null(coef_names)) stop("the conText object has no coefficient (row) names.", call. = FALSE)

  # assemble an n x M design matrix over all coefficients (defaulting to 0)
  build_design <- function(nd){
    design <- matrix(0, nrow = nrow(nd), ncol = length(coef_names),
                     dimnames = list(rownames(nd), coef_names))
    if(intercept && "(Intercept)" %in% coef_names) design[, "(Intercept)"] <- 1
    for(nm in intersect(names(nd), coef_names)) design[, nm] <- nd[[nm]]
    design
  }

  # base-category (intercept) embedding when no profiles are supplied
  if(is.null(newdata)){
    design <- build_design(data.frame(row.names = "(Intercept)"))
    return(design %*% betas)
  }

  newdata <- as.data.frame(newdata)

  # fill interaction columns from their components when not supplied explicitly
  interactions <- grep(":", setdiff(coef_names, "(Intercept)"), value = TRUE, fixed = TRUE)
  for(term in interactions){
    if(!(term %in% names(newdata))){
      comps <- strsplit(term, ":", fixed = TRUE)[[1]]
      if(all(comps %in% names(newdata))) newdata[[term]] <- Reduce(`*`, newdata[comps])
    }
  }

  unknown <- setdiff(names(newdata), coef_names)
  if(length(unknown) > 0) stop("newdata contains columns that are not coefficients in the model: ",
                               paste(unknown, collapse = ", "),
                               ". Available coefficients: ",
                               paste(setdiff(coef_names, "(Intercept)"), collapse = ", "), call. = FALSE)

  build_design(newdata) %*% betas
}
