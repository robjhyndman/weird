# Surprisals function that uses pre-calculated densities
# Same arguments as surprisals.numeric except den is numerical vector of log densities
surprisals_from_den <- function(object, den, distribution, loo) {
  object <- as.matrix(object)
  if (NCOL(object) == 1L) {
    object <- c(object)
  }
  if (length(distribution) > 1 & length(object) > 1) {
    if (length(distribution) != length(object)) {
      stop("Length of distribution and object must be the same or equal to 1")
    }
  }
  scores <- -den
  if (loo & all(stats::family(distribution) == "kde")) {
    n <- NROW(object)
    d <- NCOL(object)
    if (d == 1L) {
      h <- vctrs::vec_data(distribution)[[1]]$kde$h
      K0 <- 1 / (h * sqrt(2 * pi))
    } else {
      H <- vctrs::vec_data(distribution)[[1]]$kde$H
      K0 <- det(H)^(-1 / 2) * (2 * pi)^(-d / 2)
    }
    scores <- -log(pmax(0, (n * exp(-scores) - K0) / (n - 1)))
  }
  return(scores)
}

# Compute probability of surprisals
# Note that each value of s may come from a different distribution
# so distribution may be a vector

surprisal_prob_from_s <- function(
  s,
  distribution,
  approximation,
  threshold_probability = 0.10,
  y = NULL
) {
  n <- length(s)
  if (all(is.na(s))) {
    return(rep(NA_real_, n))
  }

  if (approximation == "none") {
    if (dimension_dist(distribution) > 1) {
      warning("Using a gpd approximation for multivariate data")
      approximation <- "gpd"
    } else if (identical(unique(stats::family(distribution)), "normal")) {
      approximation <- "normal"
    } else if (is_symmetric(distribution)) {
      approximation <- "symmetric"
    }
  }
  if (approximation == "none") {
    # Univariate, not normal, not symmetric
    if (length(unique(distribution)) == 1L) {
      distribution <- unique(distribution)
    } else {
      # Need to compute probabilities one by one
      dd <- length(distribution)
      if (dd != n) {
        stop("Length of distribution must be 1 or equal to length of s")
      }
      p <- numeric(n)
      for (i in seq(n)) {
        p[i] <- surprisal_prob_from_s(
          s[i],
          distribution[i],
          y = y[i],
          approximation = approximation,
          threshold_probability = threshold_probability
        )
      }
      return(p)
    }
  }

  if (approximation == "gpd") {
    p <- surprisal_gpd_prob(s, threshold_probability)
  } else if (approximation == "rank") {
    p <- 1 - rank(s) / n
  } else if (approximation == "normal") {
    p <- surprisal_normal_prob(s, distribution)
  } else if (approximation == "symmetric" & !is.null(y)) {
    centre <- stats::median(distribution)
    p <- 2 *
      (1 - distributional::cdf(distribution, q = centre + abs(y - centre)))
  } else {
    # Slower computation, but more general (although approximate)
    dist_x <- stats::quantile(
      distribution,
      seq(1e-6, 1 - 1e-6, length.out = 10001)
    )
    dist_x <- unique(unlist(dist_x))
    dist_y <- -unlist(density(distribution, dist_x, log = TRUE))
    prob <- (rank(dist_y) - 1) / length(dist_y)
    if (all(is.na(dist_y)) | all(is.na(prob))) {
      return(rep(NA_real_, n))
    }
    p <- 1 - approx(dist_y, prob, xout = s, rule = 2, ties = mean)$y
  }
  p[s == Inf] <- 0
  return(p)
}

# Surprisal probabilities using GPD approximation
surprisal_gpd_prob <- function(s, threshold_p) {
  n <- length(s)
  threshold_q <- stats::quantile(
    s,
    prob = 1 - threshold_p,
    type = 8,
    na.rm = TRUE
  )
  if (!any(s > threshold_q, na.rm = TRUE)) {
    warning("No surprisals above threshold")
    return(rep(1, n))
  }
  finite <- s < Inf
  # if (any(!finite, na.rm = TRUE)) {
  #  warning("Infinite surprisals will be ignored in GPD")
  # }
  gpd <- evd::fpot(s[finite], threshold = threshold_q, std.err = FALSE)$estimate
  p <- threshold_p *
    evd::pgpd(
      s,
      loc = threshold_q,
      scale = gpd["scale"],
      shape = gpd["shape"],
      lower.tail = FALSE
    )
  return(p)
}

# Surprisal probabilities for normal distributions
surprisal_normal_prob <- function(s, distribution) {
  sigma2 <- distributional::variance(distribution)
  z <- sqrt(abs(2 * s - log(2 * pi * sigma2)))
  2 * (1 - stats::pnorm(z))
}

# Check if distribution is symmetric
is_symmetric <- function(dist) {
  dist <- unique(dist)
  fam <- stats::family(dist)
  if (length(fam) > 1) {
    for (i in seq_along(fam)) {
      if (!is_symmetric(dist[i])) {
        return(FALSE)
      }
    }
    return(TRUE)
  } else if (
    fam %in% c("student_t", "cauchy", "logistic", "triangular", "uniform")
  ) {
    return(TRUE)
  } else {
    q1 <- unlist(stats::quantile(dist, seq(0.5, 0.99, length.out = 5)))
    q2 <- unlist(stats::quantile(dist, seq(0.5, 0.01, length.out = 5)))
    q1 <- q1 - q1[1]
    q2 <- q2 - q2[1]
    out <- sum(abs(q1 + q2) / max(abs(c(q1, q2))))
    if (is.na(out)) {
      return(FALSE)
    } else {
      return(out < 1e-8)
    }
  }
}

#' @importFrom stats quantile
#' @importFrom evd fpot
