#' Given a set of embeddings and a set of tokenized contexts, find the top N nearest
#' contexts.
#'
#' @param x a (quanteda) `dem-class` or `fem-class` object.
#' @param contexts_dem a `dem-class` object corresponding to the ALC
#' embeddings of candidate contexts.
#' @param contexts a (quanteda) `tokens-class` object of
#' tokenized candidate contexts. Note, these must correspond to the same
#' contexts in `contexts_dem`. If NULL, then the context (document) ids
#' will be output instead of the text.
#' @param N (numeric) number of nearest contexts to return
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per embedding
#' @param group_var (character) optional name of a variable in `contexts_dem`'s
#' docvars identifying the group each candidate context belongs to. If provided,
#' the nearest contexts for each target (e.g. each group-specific embedding) are
#' drawn only from the contexts belonging to that same group, so that a context
#' from one group is never returned as a nearest context for another group's
#' embedding. The values of `group_var` must match the rownames of `x` (the
#' target labels). If NULL (default), all candidate contexts are eligible for
#' every target.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings. `NA` if `is.null(rownames(x))`.}
#'  \item{`context`}{(character) contexts collapsed into single documents (i.e. untokenized).
#'  If `contexts` is NULL then this variable will show the context (document) ids which
#'  you can use to merge.}
#'  \item{`rank`}{(character) rank of context in terms of similarity with `x`.}
#'  \item{`value`}{(numeric) cosine similarity between `x` and context.}
#'  }
#'
#' @export
#' @rdname ncs
#' @keywords ncs
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigr*",
#' window = 6L, rm_keyword = FALSE)
#'
#' # build document-feature matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' # to get group-specific embeddings, average within party
#' immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
#'
#' # find nearest contexts by party
#' # setting as_list = FALSE combines each group's
#' # results into a single data.frame (useful for joint plotting)
#' ncs(x = immig_wv_party, contexts_dem = immig_dem,
#' contexts = immig_toks, N = 5, as_list = TRUE)
ncs <- function(x,
                contexts_dem,
                contexts = NULL,
                N = 5,
                as_list = TRUE,
                group_var = NULL){

  # for single numeric vectors
  if(is.null(dim(x)) && length(x) == dim(contexts_dem)[2]) x <- matrix(x, nrow = 1)

  # compute cosine similarity
  cos_sim <- text2vec::sim2(x = as.matrix(contexts_dem), y = as.matrix(x), method = "cosine", norm = "l2") %>% data.frame()

  # name columns
  if(!is.null(rownames(x))) colnames(cos_sim) <- rownames(x)
  if(is.null(rownames(x))) colnames(cos_sim) <- 'target'

  # capture target (embedding) columns before adding bookkeeping columns
  target_cols <- colnames(cos_sim)

  # optional: restrict each target's candidate contexts to contexts belonging to
  # that same group (see issue #12). The group of each candidate context is read
  # from contexts_dem's docvars and must align row-wise with the similarity matrix.
  group_lookup <- NULL
  if(!is.null(group_var)){
    dv <- contexts_dem@docvars
    if(is.null(dv) || !(group_var %in% names(dv))) stop("group_var '", group_var, "' not found in contexts_dem docvars.", call. = FALSE)
    if(nrow(dv) != nrow(cos_sim)) stop("contexts_dem docvars do not align with the similarity matrix.", call. = FALSE)
    group_lookup <- as.character(dv[[group_var]])
  }

  if(!is.null(contexts)){
    # contexts data.frame
    contexts_df <- data.frame(docid = quanteda::docid(contexts), context = sapply(contexts, function(i) paste(i, collapse = " ")))

    # join with contexts
    cos_sim <- cos_sim %>% dplyr::mutate(docid = rownames(cos_sim))
    if(!is.null(group_lookup)) cos_sim[['.group']] <- group_lookup
    cos_sim <- cos_sim %>% dplyr::left_join(contexts_df, by = 'docid')

    # reshape data
    result <- tidyr::pivot_longer(cos_sim, cols = dplyr::all_of(target_cols), names_to = "target")
    if(!is.null(group_lookup)) result <- result[result[['.group']] == result[['target']], , drop = FALSE]
    result <- result %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'context', 'rank', 'value')
  } else {

    # add texts ids
    cos_sim <- cos_sim %>% dplyr::mutate(context = rownames(cos_sim))
    if(!is.null(group_lookup)) cos_sim[['.group']] <- group_lookup

    # reshape data
    result <- tidyr::pivot_longer(cos_sim, cols = dplyr::all_of(target_cols), names_to = "target")
    if(!is.null(group_lookup)) result <- result[result[['.group']] == result[['target']], , drop = FALSE]
    result <- result %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'context', 'rank', 'value')

  }

  # if rowname is missing, replace with NA
  if(is.null(rownames(x))) result$target <- NA

  # if !as_list return a list object with an item for each target data.frame
  if(as_list && !is.null(rownames(x))) result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))
  if(as_list && is.null(rownames(x))) message("although as_list = TRUE, will return a single data.frame as there is only one word vector")

  return(result)
}
