#' Density score tail probabilities
#'
#' Compute the probability of a density score at least as extreme as `g` given
#' a specified distribution. A density score is given by \eqn{g = -\log(f)}
#' where \eqn{f} is the density or probability mass function of the
#' distribution. The argument `distribution` specifies which distribution to
#' use. The probabilities are estimated by simulating `n` random values from
#' the specified `distribution`. Consequently, slightly different values
#' will be returned each time unless a random seed is set using
#' \code{\link[base]{set.seed}}. If `distribution` is not specified, the
#' empirical distribution of `g` is used.
#'
#' @param g vector of density scores.
#' @param distribution A distributional object specifying the probability
#' distribution to use. If `distribution` is NULL, the empirical distribution
#' of `g` is used.
#' @param smallest_prob Smallest detectable tail probability. If the score has
#' lower probability than this, `smallest_prob` is returned.
#' @param gridsize Size of grid used in estimating the tail probability. A
# ; larger number gives more accurate estimates but takes more time.
#' @examples
#' score_tail_prob(-dnorm(1:3, log = TRUE), distributional::dist_normal())
#' @export

score_tail_prob <- function(g, distribution = NULL,
                            smallest_prob = 1e-6, gridsize = 100001) {
  if (is.null(distribution)) {
    # Just use empirical cdf
    p <- 1 - (rank(g) - 1) / length(g)
  } else if (family(distribution) == "normal") {
    # Fast computation for normal distribution
    mu <- mean(distribution)
    sigma2 <- variance(distribution)
    x <- sqrt(2 * g - log(2 * pi * sigma2))
    p <- 2 * (1 - pnorm(abs(x), mu, sqrt(sigma2)))
  } else {
    # Slower computation, but more general (although approximate)
    dist_x <- quantile(
      distribution,
      seq(smallest_prob, 1 - smallest_prob, length.out = gridsize)
    )
    dist_x <- unique(unlist(dist_x))
    dist_y <- -distributional:::density.distribution(
      distribution, dist_x,
      log = TRUE
    )[[1]]
    prob <- (rank(dist_y) - 1) / length(dist_y)
    p <- 1 - approx(dist_y, prob, xout = g, rule = 2, ties = mean)$y
  }
  return(p)
}
