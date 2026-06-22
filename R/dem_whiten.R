#' Whiten / reduce anisotropy of a document-embedding matrix
#'
#' Post-processes the embeddings in a `dem-class` object to reduce anisotropy.
#' Contextual embeddings (e.g. from BERT/RoBERTa/ModernBERT) and, to a lesser
#' extent, averaged static embeddings, tend to occupy a narrow cone in the
#' embedding space (Ethayarajh 2019), which inflates cosine similarities and
#' distorts the (squared) norms used by conText's hypothesis tests. Whitening the
#' embeddings before computing nearest neighbours, similarities or an embedding
#' regression on externally produced embeddings (see [as_dem()] and [conText()])
#' makes those quantities better behaved.
#'
#' @param x a `dem-class` object (e.g. from [dem()] or [as_dem()]).
#' @param method (character) one of:
#' \describe{
#'   \item{`"all-but-the-top"`}{(default) mean-center the embeddings and project
#'   out the top `n_components` principal directions (Mu and Viswanath 2018).}
#'   \item{`"center"`}{subtract the per-dimension mean only.}
#'   \item{`"zscore"`}{mean-center and scale each dimension to unit variance.}
#'   }
#' @param n_components (integer) for `method = "all-but-the-top"`, the number of
#' leading principal directions to remove. Defaults to `round(D / 100)` (with `D`
#' the number of embedding dimensions), following Mu and Viswanath (2018), and is
#' capped at `min(D - 1, N - 1)`.
#'
#' @return a `dem-class` object of the same dimensions, document ids and docvars
#' as `x`, with whitened embeddings.
#'
#' @references
#' Mu, J. and Viswanath, P. (2018). All-but-the-Top: Simple and Effective
#' Postprocessing for Word Representations. ICLR.
#'
#' Ethayarajh, K. (2019). How Contextual are Contextualized Word Representations?
#' EMNLP-IJCNLP.
#'
#' @export
#' @rdname dem_whiten
#' @keywords dem_whiten
#' @examples
#'
#' # toy anisotropic embeddings (a shared component plus noise)
#' set.seed(1)
#' shared <- matrix(rep(rnorm(10), each = 50), nrow = 50)
#' E <- shared + matrix(rnorm(50 * 10, sd = 0.3), nrow = 50)
#' d <- as_dem(E, docvars = data.frame(g = rep(c("a", "b"), 25)))
#'
#' # remove the dominant shared direction(s)
#' dw <- dem_whiten(d, method = "all-but-the-top")
#' dim(dw)
dem_whiten <- function(x, method = c("all-but-the-top", "center", "zscore"), n_components = NULL){

  method <- match.arg(method)
  if(!methods::is(x, "dem")) stop("x must be a dem-class object (see as_dem()).", call. = FALSE)

  M <- as.matrix(x)
  mu <- Matrix::colMeans(M)
  Mc <- sweep(M, 2, mu, "-") # mean-center

  if(method == "center"){
    W <- Mc
  } else if(method == "zscore"){
    s <- apply(Mc, 2, stats::sd)
    s[s == 0 | is.na(s)] <- 1 # avoid dividing constant/degenerate dimensions by 0
    W <- sweep(Mc, 2, s, "/")
  } else { # all-but-the-top
    if(is.null(n_components)) n_components <- round(ncol(M) / 100)
    n_components <- min(n_components, ncol(M) - 1, nrow(M) - 1)
    if(n_components >= 1){
      sv <- svd(Mc, nu = 0, nv = n_components)
      Vk <- sv$v[, seq_len(n_components), drop = FALSE]
      W <- Mc - (Mc %*% Vk) %*% t(Vk) # project out the top directions
    } else {
      W <- Mc
    }
  }

  rownames(W) <- rownames(M)
  build_dem(Class = "dem",
            x_dem = W,
            docvars = x@docvars,
            features = x@features,
            Dimnames = list(docs = rownames(W), columns = NULL))
}
