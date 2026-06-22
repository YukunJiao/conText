#' Get averaged similarity scores between target word(s) and one or two vectors of candidate words.
#'
#' Get similarity scores between a target word or words and a comparison vector
#' of one candidate word or words. When two vectors of candidate words are
#' provided (`second_vec` is not `NULL`), the function calculates the cosine
#' similarity between a composite index of the two vectors. This is
#' operationalized as the mean similarity of the target word to the first
#' vector of terms plus negative one multiplied by the mean similarity to the
#' second vector of terms.
#'
#' @param x a (quanteda) `corpus` object
#' @param target (character) vector of words
#' @param first_vec (character) vector of words
#' @param second_vec (character) vector of words
#' @param pre_trained  (numeric) a F x D matrix corresponding to pretrained embeddings,
#' usually trained on the same corpus as that used for `x`.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param group_var (character) variable name in corpus object defining grouping variable
#' @param window (numeric) - defines the size of a context (words around the target)
#' @param norm (character) - "l2" for l2 normalized cosine similarity and "none" for dot product
#' @param remove_punct (logical) - if `TRUE` remove all characters in the Unicode
#'   "Punctuation" `[P]` class
#' @param remove_symbols (logical) - if `TRUE` remove all characters in the Unicode
#'   "Symbol" `[S]` class
#' @param remove_numbers (logical) - if `TRUE` remove tokens that consist only of
#'   numbers, but not words that start with digits, e.g. `2day`
#' @param remove_separators (logical) - if `TRUE` remove separators and separator
#'   characters (Unicode "Separator" `[Z]` and "Control" `[C]` categories)
#' @param valuetype the type of pattern matching: `"glob"` for "glob"-style
#'   wildcard expressions; `"regex"` for regular expressions; or `"fixed"` for
#'   exact matching
#' @param hard_cut (logical) - if TRUE then a context must have `window` x 2 tokens,
#' if FALSE it can have `window` x 2 or fewer (e.g. if a doc begins with a target word,
#' then context will have `window` tokens rather than `window` x 2)
#' @param case_insensitive (logical) - if `TRUE`, ignore case when matching a
#' target patter
#' @param bootstrap (logical) if TRUE, bootstrap the grouped similarity scores --
#' resample contexts with replacement (stratified by `group_var`) and re-estimate
#' the scores for each sample, to obtain std. errors and confidence intervals.
#' @param num_bootstraps (integer) number of bootstraps to use (must be >= 100).
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#'
#' @return a `data.frame` with the following columns:
#' \describe{
#'  \item{`group`}{ the grouping variable specified for the analysis}
#'  \item{`val`}{(numeric) cosine similarity scores. Average over bootstrapped
#'  samples if `bootstrap = TRUE`.}
#'  \item{`std.error`}{(numeric) (if `bootstrap = TRUE`) std. error of `val`.}
#'  \item{`lower.ci`}{(numeric) (if `bootstrap = TRUE`) lower bound of the confidence interval.}
#'  \item{`upper.ci`}{(numeric) (if `bootstrap = TRUE`) upper bound of the confidence interval.}
#'  }
#' @export
#'
#' @examples
#' quanteda::docvars(cr_sample_corpus, 'year') <- rep(2011:2014, each = 50)
#' cos_simsdf <- get_grouped_similarity(cr_sample_corpus,
#'                                     group_var = "year",
#'                                     target = "immigration",
#'                                     first_vec = c("left", "lefty"),
#'                                     second_vec = c("right", "rightwinger"),
#'                                     pre_trained = cr_glove_subset,
#'                                     transform_matrix = cr_transform,
#'                                     window = 12L,
#'                                     norm = "l2")

get_grouped_similarity <- function(x,
                                   target,
                                   first_vec,
                                   second_vec,
                                   pre_trained,
                                   transform_matrix,
                                   group_var,
                                   window = 6L,
                                   norm = "l2",
                                   remove_punct = FALSE,
                                   remove_symbols = FALSE,
                                   remove_numbers = FALSE,
                                   remove_separators = FALSE,
                                   valuetype = "fixed",
                                   hard_cut = FALSE,
                                   case_insensitive = TRUE,
                                   bootstrap = FALSE,
                                   num_bootstraps = 100,
                                   confidence_level = 0.95) {

  # initial checks
  if(bootstrap && (confidence_level >= 1 || confidence_level <= 0)) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE)
  if(bootstrap && num_bootstraps < 100) stop('num_bootstraps must be at least 100', call. = FALSE)

  # Tokenize corpus
  toks <- quanteda::tokens(x, remove_punct = remove_punct, remove_symbols = remove_symbols,
                 remove_numbers = remove_numbers, remove_separators = remove_separators)

  # Build tokenized corpus of contexts surrounding the target word
  target_toks <- tokens_context(x = toks, pattern = target,
                                valuetype = valuetype, window = window,
                                hard_cut = hard_cut, case_insensitive = case_insensitive)

  # Compute ALC embeddings (one per context instance)
  target_dfm <- quanteda::dfm(target_toks)
  target_dem <- dem(x = target_dfm, pre_trained = pre_trained,
                    transform = TRUE, transform_matrix = transform_matrix,
                    verbose = TRUE)

  groups_vec <- target_dem@docvars[[group_var]]

  if(bootstrap){
    cat('starting bootstraps \n')
    bs <- replicate(num_bootstraps, {
      target_sample_dem <- dem_sample(x = target_dem, size = 1, replace = TRUE, by = groups_vec)
      grouped <- dem_group(target_sample_dem, groups = target_sample_dem@docvars[[group_var]])
      compute_grouped_similarity(grouped, first_vec, second_vec, pre_trained, norm, verbose = FALSE)
    }, simplify = FALSE)

    result <- do.call(rbind, bs) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(lower.ci = stats::quantile(val, probs = (1 - confidence_level)/2, names = FALSE),
                    upper.ci = stats::quantile(val, probs = (1 + confidence_level)/2, names = FALSE)) %>%
      dplyr::summarise(std.error = stats::sd(val),
                       val = mean(val),
                       lower.ci = mean(lower.ci),
                       upper.ci = mean(upper.ci),
                       .groups = 'drop') %>%
      dplyr::select(group, val, std.error, lower.ci, upper.ci)
    cat('done with bootstraps \n')
  }else{
    # Aggregate embeddings over the grouping variable
    target_dem_grouped <- dem_group(target_dem, groups = groups_vec)
    result <- compute_grouped_similarity(target_dem_grouped, first_vec, second_vec, pre_trained, norm, verbose = TRUE)
  }

  return(result)
}

# sub-functions ---------------------------------------------------------------

# mean cosine similarity of each (grouped) ALC embedding to a vector of features
grouped_similarity_vec <- function(grouped_dem, vec, pre_trained, norm, label, verbose = TRUE){
  intersect_words <- intersect(vec, rownames(pre_trained))
  missing_words <- setdiff(vec, intersect_words)
  if(verbose && length(missing_words) > 0) cat("Words in ", label, " not found in pre-trained embeddings: ", paste(missing_words, collapse = ", "), "\n", sep = "")
  if(length(intersect_words) == 0) stop("none of the words in ", label, " are present in the pre-trained embeddings.", call. = FALSE)
  # transpose when only a single word survives, so sim2 sees a 1 x D matrix
  y_matrix <- if(length(intersect_words) > 1) as.matrix(pre_trained[intersect_words,]) else t(as.matrix(pre_trained[intersect_words,]))
  Matrix::rowMeans(text2vec::sim2(grouped_dem, y = y_matrix, method = 'cosine', norm = norm))
}

# given a grouped dem, compute the (composite) similarity score per group
compute_grouped_similarity <- function(grouped_dem, first_vec, second_vec, pre_trained, norm, verbose = TRUE){
  first_val <- grouped_similarity_vec(grouped_dem, first_vec, pre_trained, norm, "first_vec", verbose)
  if(is.null(second_vec)){
    val <- first_val
  }else{
    sec_val <- grouped_similarity_vec(grouped_dem, second_vec, pre_trained, norm, "second_vec", verbose)
    val <- first_val - sec_val # aligned by group (same grouped_dem)
  }
  dplyr::tibble(group = factor(names(val)), val = unname(val))
}
