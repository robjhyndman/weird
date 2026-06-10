#' Augment data with results from a robust principal component analysis
#'
#' @description Augment the data with information from an rrcov `Pca*` object
#' (such as the output of [rrcov::PcaHubert()] or [rrcov::PcaClassic()]). The
#' returned tibble contains the principal component scores (`.fittedPC1`,
#' `.fittedPC2`, ...), the score distance (`.sd`) and the orthogonal distance
#' (`.od`) of each observation. The score distance measures how far an
#' observation lies from the centre *within* the projection subspace, while the
#' orthogonal distance measures how far it lies *from* the subspace. If `data`
#' is supplied, its columns are returned alongside these results.
#'
#' @param x An rrcov `Pca*` object.
#' @param data The original data matrix or data frame used to compute the PCA.
#' If supplied, its columns are bound to the left of the returned tibble.
#' @param ... Unused.
#' @return A [tibble::tibble()] with one row per observation.
#' @author Rob J Hyndman
#' @examplesIf requireNamespace("rrcov", quietly = TRUE)
#' Y <- oldfaithful[, c("duration", "waiting")]
#' pca <- rrcov::PcaHubert(as.matrix(Y), k = 1)
#' broom::augment(pca, data = Y)
#' @exportS3Method broom::augment
augment.Pca <- function(x, data = NULL, ...) {
  scores <- x$scores
  colnames(scores) <- paste0(".fittedPC", seq_len(ncol(scores)))
  out <- dplyr::bind_cols(
    dplyr::as_tibble(scores),
    tibble(.sd = as.numeric(x$sd), .od = as.numeric(x$od))
  )
  if (!is.null(data)) {
    out <- dplyr::bind_cols(dplyr::as_tibble(data), out)
  }
  out
}
