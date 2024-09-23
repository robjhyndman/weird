# Compute probability of surprisals

surprisal_prob <- function(
    s,
    distribution,
    approximation = c("none", "gpd", "empirical"),
    threshold_probability = 0.10) {
  approximation <- match.arg(approximation)
  n <- length(s)
  d <- dimension_dist(distribution)
  if(d > 1) {
    warning("Using an empirical approximation for multivariate data.")
    approximation <- "empirical"
  }
  if (approximation == "gpd") {
    return(surprisal_gpd_prob(s, threshold_probability))
  } else if (approximation == "empirical") {
    # Just use empirical cdf
    p <- 1 - (rank(s) - 1) / n
  } else if (stats::family(distribution) == "normal") {
    return(surprisal_normal_prob(s, distribution))
  #} else if (is_symmetric(distribution)) {
    #centre <- stats::median(distribution)
    #p <- 2 * (1 - distributional::cdf(distribution, q = centre + abs(object - centre)))
  } else {
    # Slower computation, but more general (although approximate)
    dist_x <- stats::quantile(
      distribution,
      seq(1e-6, 1 - 1e-6, length.out = 10001)
    )
    dist_x <- unique(unlist(dist_x))
    dist_y <- -unlist(density(distribution, dist_x, log = TRUE))
    prob <- (rank(dist_y) - 1) / length(dist_y)
    p <- 1 - approx(dist_y, prob, xout = s, rule = 2, ties = mean)$y
  }
  p[s == Inf] <- 0
  return(p)
}

# Surprisal probabilities using GPD approximation
surprisal_gpd_prob <- function(s, threshold) {
  n <- length(s)
  threshold <- stats::quantile(s, prob = 1 - threshold, type = 8, na.rm = TRUE)
  if (!any(s > threshold, na.rm = TRUE)) {
    warning("No surprisals above threshold")
    return(rep(1, n))
  }
  finite <- s < Inf
  if (any(!finite, na.rm = TRUE)) {
    warning("Infinite surprisals will be ignored in GPD")
  }
  gpd <- evd::fpot(s[finite], threshold = threshold, std.err = FALSE)$estimate
  p <- threshold * evd::pgpd(
    s,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
  p[s == Inf] <- 0
  return(p)
}

# Surprisal probabilities for normal distributions
surprisal_normal_prob <- function(s, distribution) {
  mu <- mean(distribution)
  sigma2 <- distributional::variance(distribution)
  x <- sqrt(2 * s - log(2 * pi * sigma2))
  2 * (1 - stats::pnorm(abs(x), mu, sqrt(sigma2)))
}

# Check if distribution is symmetric
is_symmetric <- function(dist) {
  if (stats::family(dist) %in%
    c("student_t", "cauchy", "logistic", "triangular", "uniform")) {
    return(TRUE)
  } else {
    q1 <- unlist(stats::quantile(dist, seq(0.5, 0.99, length.out = 5)))
    q2 <- unlist(stats::quantile(dist, seq(0.5, 0.01, length.out = 5)))
    q1 <- q1 - q1[1]
    q2 <- q2 - q2[1]
    return(sum(abs(q1 + q2) / max(abs(c(q1, q2)))) < 1e-8)
  }
}



#' @importFrom stats quantile
#' @importFrom evd fpot
