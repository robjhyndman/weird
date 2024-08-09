#' Surprisal probabilities
#'
#' Compute the probability of a surprisal at least as extreme as those observed.
#' A surprisal is given by \eqn{-\log(f)} where \eqn{f} is the density or
#' probability mass function of the distribution.
#' The surprisal values are computed from the `distribution` provided. If no
#' distribution is provided, a kernel density estimate is used.
#'
#' The surprisal probabilities may be computed in three different ways.
#' 1. Given the same `distribution` that was used to compute the surprisal values.
#' Under this option, surprisal probabilities are equal to 1 minus the
#' coverage probability of the largest HDR that contains each value. Surprisal
#' probabilities smaller than 1e-6 are returned as 1e-6.
#' 2. Using a Generalized Pareto Distribution fitted to the most extreme
#' surprisal values (those with probability less than `threshold_probability`).
#' This option is used if `GPD = TRUE`. For surprisal values with
#' probability less than `threshold_probability`, the value of
#' `threshold_probability` is returned. Under this option, the distribution is
#' used for computing the surprisal values but not for determining their
#' probabilities. Due to extreme value theory, the resulting probabilities should
#' be relatively insensitive to the distribution used in computing the surprisal
#' values.
#' 3. Empirically as the proportion of observations with greater surprisal values.
#' This option is used when `GPD = FALSE` and no distribution is explicitly
#' provided. This is also insensitive to the distribution used in computing the
#' surprisal values.
#'
#' @param object A model or numerical data set.
#' @param distribution A probability distribution stored as a distributional
#' object. Ignored if `object` is a model.
#' @param loo Logical value specifying if leave-one-out surprisals should be computed.
#' @param GPD Logical value specifying if a Generalized Pareto distribution
#' should be used to estimate the probabilities.
#' @param threshold_probability Probability threshold when computing the GPD
#' distribution for the surprisals.
#' @param ... Other arguments are passed to \code{\link{surprisals}}.
#' @examples
#' # Univariate data
#' tibble(
#'   y = c(5, rnorm(49)),
#'   p = surprisal_prob(y)
#' )
#' tibble(
#'   y = n01$v1,
#'   prob1 = surprisal_prob(y),
#'   prob2 = surprisal_prob(y, GPD = TRUE),
#'   prob3 = surprisal_prob(y, dist_normal()),
#'   prob4 = surprisal_prob(y, dist_normal(), GPD = TRUE)
#' ) |>
#'   arrange(prob1)
#' # Bivariate data
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   lookout = lookout_prob(cbind(x, y))
#' )
#' # Using a regression model
#' of <- oldfaithful |> filter(duration < 7200, waiting < 7200)
#' fit_of <- lm(waiting ~ duration, data = of)
#' of |>
#'   mutate(p = surprisal_prob(fit_of)) |>
#'   arrange(p)
#' @export

surprisal_prob <- function(
    object,
    distribution = NULL,
    loo = FALSE,
    GPD = FALSE,
    threshold_probability = 0.10,
    ...) {
  if (is.null(distribution)) {
    g <- surprisals(object, loo = loo, ...)
  } else {
    g <- surprisals(object, distribution = distribution, loo = loo, ...)
  }
  n <- length(g)
  if (GPD) {
    threshold <- stats::quantile(g,
      prob = 1 - threshold_probability,
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
    p <- threshold_probability * evd::pgpd(
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
    dist_x <- stats::quantile(distribution, seq(1e-6, 1 - 1e-6, length.out = 100001))
    dist_x <- unique(unlist(dist_x))
    dist_y <- -unlist(density(distribution, dist_x, log = TRUE))
    prob <- (rank(dist_y) - 1) / length(dist_y)
    p <- 1 - approx(dist_y, prob, xout = g, rule = 2, ties = mean)$y
  }
  return(p)
}

#' @importFrom stats quantile
#' @importFrom evd fpot
