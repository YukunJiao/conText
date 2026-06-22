#' Compute transformation matrix A
#'
#' Computes a transformation matrix, given a feature-co-occurrence
#' matrix and corresponding pre-trained embeddings.
#'
#' @details This is the estimator of the 'a la carte' (ALC) transformation matrix
#' described in Khodak et al. (2018) and Rodriguez, Spirling and Stewart (2023):
#' it is the (frequency-weighted) least-squares solution that best recovers each
#' feature's pre-trained embedding from the average of the embeddings of the
#' features it co-occurs with. It is therefore the function used to build your own
#' transformation matrix instead of relying on a pre-computed one (e.g.
#' `cr_transform`).
#'
#' The procedure is language-agnostic: to apply ALC embeddings to another language
#' you only need (1) pre-trained embeddings for that language (e.g. GloVe, word2vec
#' or fastText vectors) and (2) a corpus in that language from which to build the
#' feature-co-occurrence matrix `x`. Pass both to `compute_transform()` to obtain a
#' language-specific transformation matrix. The pre-trained embeddings and the
#' co-occurrence matrix should share vocabulary and use the same preprocessing.
#'
#' @param x a (quanteda) `fcm-class` object.
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings,
#' usually trained on the same corpus as that used for `x`.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param weighting (character or numeric) weighting options:
#' \describe{
#'   \item{`1`}{no weighting.}
#'   \item{`"log"`}{weight by the log of the frequency count.}
#'   \item{`numeric`}{threshold based weighting (= 1 if token count meets threshold, 0 ow).}
#'   }
#' Recommended: use `log` for small corpora, a numeric threshold for larger corpora.
#'
#' @return a `dgTMatrix-class` D x D non-symmetrical matrix
#' (D = dimensions of pre-trained embedding space) corresponding
#' to an 'a la carte' transformation matrix. This matrix is optimized
#' for the corpus and pre-trained embeddings employed.
#'
#' @export
#' @rdname compute_transform
#' @keywords compute_transform
#' @examples
#'
#' \dontrun{
#' # example exceeds CRAN CPU time to elapsed time limit
#' library(quanteda)
#' # note, cr_sample_corpus is too small to produce sensical word vectors
#'
#' # tokenize
#' toks <- tokens(cr_sample_corpus)
#'
#' # construct feature-co-occurrence matrix
#' toks_fcm <- fcm(toks, context = "window", window = 6,
#' count = "weighted", weights = 1 / (1:6), tri = FALSE)
#'
#' # you will generally want to estimate a new (corpus-specific)
#' # GloVe model, we will use cr_glove_subset instead
#' # see the Quick Start Guide to see a full example.
#'
#' # estimate transform
#' local_transform <- compute_transform(x = toks_fcm,
#' pre_trained = cr_glove_subset, weighting = 'log')
#' }
compute_transform <- function(x, pre_trained, weighting = 500){

  # compute un-transformed additive embedding
  context_embeddings <- fem(x = x, pre_trained = pre_trained, transform_matrix, transform = FALSE, verbose = FALSE)

  # extract feature frequency from fcm object
  feature_frequency <- x@meta$object$margin

  # Restrict to features that (a) have a named, non-missing frequency and (b) exist
  # in BOTH the context embeddings and the pre-trained embeddings. Using a single
  # explicit common-feature set (rather than chained intersect() + name indexing)
  # avoids NA or unnamed margin entries silently producing out-of-bounds subscripts
  # when the pre-trained embeddings are not trained on the local corpus (e.g.
  # off-the-shelf or non-English embeddings). See issue #28.
  feature_frequency <- feature_frequency[!is.na(feature_frequency) & !is.na(names(feature_frequency))]
  common_features <- Reduce(intersect, list(names(feature_frequency), rownames(context_embeddings), rownames(pre_trained)))
  feature_frequency <- feature_frequency[common_features]

  # apply weighting threshold
  if(identical(weighting, 'log')) feature_frequency <- feature_frequency[feature_frequency >= 1] # avoid negatives when taking logs
  if(is.numeric(weighting)) feature_frequency <- feature_frequency[feature_frequency >= weighting] # threshold based weighting

  # need at least D features to solve for a D x D transformation matrix; fail with
  # an informative message rather than a cryptic 'singular'/'subscript' error.
  if(length(feature_frequency) < ncol(pre_trained))
    stop("only ", length(feature_frequency), " features overlap between the fcm, the pre-trained embeddings, and the weighting threshold - too few to estimate a ", ncol(pre_trained), "-dimensional transformation matrix. Try a less restrictive 'weighting' (e.g. 'log'), a larger corpus, or check that the fcm and pre-trained embeddings share vocabulary.", call. = FALSE)

  # make sure features are in the same order
  context_embeddings <- context_embeddings[names(feature_frequency), , drop = FALSE]
  pre_trained <- pre_trained[names(feature_frequency), , drop = FALSE]

  # weighting function
  alpha <- Matrix::Matrix(nrow = nrow(context_embeddings), ncol = nrow(context_embeddings), data=0, sparse=T) # initialize weight matrix to be modified
  if(is.numeric(weighting)) diag(alpha) <- 1 # threshold is applied above, hence can simply multiply by 1
  if(identical(weighting, 'log')) diag(alpha) <- log(feature_frequency) # weight by log of token count

  # solve for transformation matrix (just a weighted regression)
  # following lm, we use qr decomposition, faster and more stable
  # useful discussion: https://www.theissaclee.com/post/linearqrandchol/
  wx = sqrt(alpha)%*%context_embeddings
  wy = sqrt(alpha)%*%pre_trained
  transform_matrix <- qr.solve(wx, wy)

  return(t(transform_matrix))
}
