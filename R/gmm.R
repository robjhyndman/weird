#' Convert Gaussian mixture model to a distributional object
#'
#' @param object Object of class `Mclust`, output from the [[mclust::Mclust]] function
#' @return An object of class `distributional`
#' @examples
#' library(mclust)
#' # Univariate mixture density
#' gmm <- Mclust(oldfaithful$duration) |> dist_mclust()
#' gg_density(gmm) +
#'   geom_jitter(data = oldfaithful, aes(x=duration, y = -0.0002),
#'     width=0, height=0.0002, alpha = 0.1) +
#'   labs(x = "Eruption duration", y = "")
#' @export
dist_mclust <- function(object) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' is required to use this function")
  }
  if (!inherits(object, "Mclust")) {
    stop("Object must be of class 'Mclust'")
  }
  par <- object$parameters
  equal_var <- par$variance$modelName == "E"
  mean <- unname(par$mean)
  if (is.matrix(mean)) {
    sigma <- unname(par$variance$sigma)
  } else {
    sigma <- sqrt(par$variance$sigmasq)
    if (equal_var) {
      sigma <- rep(sigma, object$G)
    }
  }

  dist_list <- lapply(seq_len(object$G), function(i) {
    if (is.matrix(mean)) {
      distributional::dist_multivariate_normal(
        list(mean[, i]),
        list(sigma[,, i])
      )
    } else {
      distributional::dist_normal(par$mean[i], sigma[i])
    }
  })
  if (object$G == 1L) {
    return(dist_list[[1]])
  } else {
    unname(do.call(
      distributional::dist_mixture,
      c(dist_list, list(weights = par$pro))
    ))
  }
}
