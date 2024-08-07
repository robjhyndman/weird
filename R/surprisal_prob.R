#' Surprisal probabilities
#'
#' Compute the probability of a surprisal at least as extreme as `g`.
#' A surprisal is given by \eqn{g = -\log(f)}
#' where \eqn{f} is the density or probability mass function of the
#' distribution.
#' These probabilities may be computed in three different ways.
#' 1. Given a specified `distribution`.
#' 2. Using a Generalized Pareto Distribution fitted to the most extreme
#' values of `g` (those above the `threshold_probability` quantile). This
#' option is used if `GPD = TRUE`. For `g` values with tail probability
#' greater than `threshold_probability`, the value of `threshold_probability`
#' is returned.
#' 3. Empirically as the proportion of values above `g`. This option is
#' used when `GPD = FALSE` and `distribution = NULL`.
#'
#' @param g vector of surprisals.
#' @param distribution A distributional object specifying the probability
#' distribution to use.
#' @param GPD Logical value specifying if a Generalized Pareto distribution
#' should be used to estimate the tail probabilities.
#' @param smallest_prob Smallest detectable tail probability to be used
#' if empirical tail proportions are used. If the surprisal has lower probability
#' than this, `smallest_prob` is returned.
#' @param gridsize Size of grid used in estimating the empirical tail
#' probabilities. A larger number gives more accurate estimates but takes
#' more time.
#' @param threshold_probability Probability threshold when computing the GPD
#' distribution for the surprisals.
#' @examples
#' surprisal_prob(-dnorm(1:3, log = TRUE), dist_normal())
#' tibble(
#'   y = n01$v1,
#'   g = -dnorm(y, log = TRUE),
#'   prob1 = surprisal_prob(g, dist_normal()),
#'   prob2 = surprisal_prob(g, GPD = TRUE),
#'   prob3 = surprisal_prob(g)
#' ) |>
#'   filter(prob1 < 0.01 | prob2 < 0.01 | prob3 < 0.01)

#' @export

surprisal_prob <- function(
    g,
    distribution = NULL, GPD = FALSE,
    smallest_prob = 1e-6, gridsize = 100001,
    threshold_probability = 0.90) {
  n <- length(g)
  if (GPD & !is.null(distribution)) {
    warning("GPD is specified, so the distribution argument will be ignored.")
  }
  if (GPD) {
    threshold <- stats::quantile(g,
      prob = threshold_probability,
      type = 8, na.rm = TRUE
    )
    if (!any(g > threshold, na.rm = TRUE)) {
      warning("No surprisals above threshold.")
      return(rep(1, n))
    }
    finite <- g < Inf
    if (any(!finite, na.rm = TRUE)) {
      warning("Infinite surprisals will be ignored in GPD.")
    }
    gpd <- evd::fpot(g[finite], threshold = threshold, std.err = FALSE)$estimate
    p <- (1 - threshold_probability) * evd::pgpd(
      g,
      loc = threshold,
      scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
    )
  } else if (is.null(distribution)) {
    # Just use empirical cdf
    p <- 1 - (rank(g) - 1) / n
  } else if (stats::family(distribution) == "normal") {
    # Fast computation for normal distribution
    mu <- mean(distribution)
    sigma2 <- distributional::variance(distribution)
    x <- sqrt(2 * g - log(2 * pi * sigma2))
    p <- 2 * (1 - stats::pnorm(abs(x), mu, sqrt(sigma2)))
  } else {
    # Slower computation, but more general (although approximate)
    dist_x <- quantile(
      distribution,
      seq(smallest_prob, 1 - smallest_prob, length.out = gridsize)
    )
    dist_x <- unique(unlist(dist_x))
    dist_y <- -density(
      distribution, dist_x,
      log = TRUE
    )[[1]]
    prob <- (rank(dist_y) - 1) / length(dist_y)
    p <- 1 - approx(dist_y, prob, xout = g, rule = 2, ties = mean)$y
  }
  return(p)
}
