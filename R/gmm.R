#' Convert Gaussian mixture model to a distributional object
#'
#' @param object Object of class `Mclust`, output from the [[mclust::Mclust]] function
#' @return An object of class `distributional`
#' @examples
#' \dontrun{
#' library(mclust)
#' # Create a bivariate Gaussian mixture model for the Old Faithful data
#' gmm <- oldfaithful |>
#'   select(duration, waiting) |>
#'   Mclust() |>
#'   gmm_to_dist()
#' gg_density(gmm) +
#'   geom_point(data = oldfaithful, aes(x = duration, y = waiting), alpha = 0.1) +
#'   labs(x = "Duration", y = "Waiting time")
#' }
gmm_to_dist <- function(object) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' is required to use this function.")
  }
  if (!inherits(object, "Mclust")) {
    stop("Object must be of class 'Mclust'.")
  }
  par <- object$parameters
  dist_list <- lapply(seq_len(object$G), function(i) {
    distributional::dist_multivariate_normal(
      list(unname(par$mean[, i])),
      list(unname(par$variance$sigma[,, i]))
    )
  })
  do.call(distributional::dist_mixture, c(dist_list, list(weights = par$pro)))
}
