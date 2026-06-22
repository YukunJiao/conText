#' Wrap a matrix of embeddings as a `dem` (document-embedding matrix)
#'
#' Constructs a `dem-class` object from an existing matrix of per-instance
#' embeddings and (optionally) their document variables. This lets you use
#' embeddings produced outside of conText -- e.g. contextual embeddings from a
#' transformer (BERT/RoBERTa/ModernBERT), decontextualized static embeddings, or
#' any other per-instance representation -- with conText's downstream tooling,
#' including the embedding regression in [conText()] (pass the resulting `dem` as
#' `data` with a `.` on the left-hand side of the formula) and [nns()]/[cos_sim()].
#'
#' @param x a (numeric) N x D matrix of embeddings: one row per instance
#' (document/context), D embedding dimensions. Row names, if present, are used as
#' document ids.
#' @param docvars a `data.frame` of document variables with one row per row of
#' `x` (same order). If `NULL` (default), an empty set of docvars is used.
#' @param features (character) optional vector of features used to compute the
#' embeddings (stored for reference only).
#'
#' @return a `dem-class` object.
#'
#' @export
#' @rdname as_dem
#' @keywords as_dem
#' @examples
#'
#' # a toy 4 x 5 embedding matrix with covariates
#' set.seed(1)
#' E <- matrix(rnorm(20), nrow = 4)
#' dv <- data.frame(party = c("D", "D", "R", "R"))
#' my_dem <- as_dem(E, docvars = dv)
#' my_dem@docvars
as_dem <- function(x, docvars = NULL, features = character(0)){
  x <- as.matrix(x)
  if(!is.numeric(x)) stop("x must be a numeric matrix.", call. = FALSE)
  if(is.null(rownames(x))) rownames(x) <- paste0("text", seq_len(nrow(x)))

  if(is.null(docvars)) docvars <- data.frame(row.names = seq_len(nrow(x)))
  docvars <- as.data.frame(docvars)
  if(nrow(docvars) != nrow(x)) stop("nrow(docvars) (", nrow(docvars), ") must equal nrow(x) (", nrow(x), ").", call. = FALSE)

  build_dem(Class = "dem",
            x_dem = x,
            docvars = docvars,
            features = features,
            Dimnames = list(docs = rownames(x), columns = NULL))
}
