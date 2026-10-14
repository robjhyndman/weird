# Surprisals function that uses pre-calculated densities
# Same arguments as surprisals.numeric except den is numerical vector of log densities
surprisals_from_den <- function(object, den, distribution, loo) {
  object <- as.matrix(object)
  if (NCOL(object) == 1L) {
    object <- c(object)
  }
  if (length(distribution) > 1 && length(object) > 1) {
    if (length(distribution) != length(object)) {
      stop("Length of distribution and object must be the same or equal to 1")
    }
  }
  scores <- -den
  if (loo && all(stats::family(distribution) == "kde")) {
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
    } else if (is_discrete(distribution)) {
      approximation <- "discrete"
    } else if (identical(unique(stats::family(distribution)), "normal")) {
      approximation <- "normal"
    } else if (is_symmetric(distribution)) {
      approximation <- "symmetric"
    }
  }
  if (approximation == "none") {
    # Univariate, not normal, not symmetric
    dd <- unique(distribution)
    if (length(dd) == 1L) {
      distribution <- unique(distribution)
    } else {
      # Need to compute probabilities one distribution at a time
      p <- numeric(n)
      for (i in seq_along(dd)) {
        j <- which(distribution == dd[i])
        p[j] <- surprisal_prob_from_s(
          s[j],
          dd[i],
          y = y[j],
          approximation = approximation,
          threshold_probability = threshold_probability
        )
      }
      return(p)
    }
  }

  if (approximation == "gpd") {
    p <- surprisal_gpd_prob(s, threshold_probability)
  } else if (approximation == "empirical") {
    p <- rank(-s, ties.method = "max", na.last = "keep") / sum(!is.na(s))
  } else if (approximation == "discrete") {
    p <- surprisal_discrete_prob(s, distribution)
  } else if (approximation == "normal") {
    p <- surprisal_normal_prob(s, distribution)
  } else if (approximation == "symmetric" && !is.null(y)) {
    centre <- stats::median(distribution)
    p <- 2 *
      (1 - distributional::cdf(distribution, q = centre + abs(y - centre))[[1]])
  } else {
    # Slower computation, but more general
    p <- surprisal_continuous_prob(s, distribution)
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

# Exact surprisal probabilities for discrete distributions: the total
# probability of all outcomes that are no more likely than the observation.
# Each value of s may come from a different distribution.
surprisal_discrete_prob <- function(s, distribution) {
  distribution <- distribution[rep_len(seq_along(distribution), length(s))]
  vapply(
    seq_along(s),
    function(i) {
      if (is.na(s[i])) {
        return(NA_real_)
      }
      d <- distribution[i]
      fy <- exp(-s[i])
      pmf <- function(x) unlist(stats::density(d, x))
      cdf <- function(x) unlist(distributional::cdf(d, x))
      lim <- vctrs::field(distributional::support(d), "lim")[[1]]
      # Enumerate the support, or for an infinite support, far enough into the
      # tails that every outcome beyond is less likely than the observation
      lower <- lim[1]
      if (!is.finite(lower)) {
        lower <- unlist(stats::quantile(d, 1e-15))
        while (pmf(lower) > fy) lower <- 2 * lower - 1
      }
      upper <- lim[2]
      if (!is.finite(upper)) {
        upper <- unlist(stats::quantile(d, 1 - 1e-15))
        while (pmf(upper) > fy) upper <- 2 * upper + 1
      }
      f <- pmf(seq(lower, upper))
      # Allow for rounding error so the observation itself is counted, and in
      # the sum (which can exceed 1 when the observation is at the mode)
      min(1, sum(f[f <= fy * (1 + 1e-9)]) + cdf(lower - 1) + (1 - cdf(upper)))
    },
    numeric(1)
  )
}

# Surprisal probabilities for a univariate continuous distribution: the
# probability of the region where the density is no more than at the
# observation. The density is evaluated on a grid of quantiles, extended into
# the tails until it is below the density of every observation; the region's
# boundaries are found where the log density crosses that of the observation.
surprisal_continuous_prob <- function(s, distribution) {
  log_den <- function(x) {
    pmax(unlist(stats::density(distribution, x, log = TRUE)), -1e10)
  }
  cdf <- function(x) unlist(distributional::cdf(distribution, x))
  x <- stats::quantile(distribution, seq(1e-6, 1 - 1e-6, length.out = 10001))
  x <- sort(unique(unlist(x)))
  if (all(is.na(x))) {
    return(rep(NA_real_, length(s)))
  }
  lim <- vctrs::field(distributional::support(distribution), "lim")[[1]]
  target <- -max(s[is.finite(s)], -Inf)
  # Extend the grid geometrically until the density is below the target, or
  # the edge of the support is reached
  width <- max(diff(range(x)), 1)
  extend <- function(end, direction, limit) {
    out <- numeric(0)
    for (k in 0:60) {
      new <- end + direction * width * 2^k
      if (direction * (new - limit) >= 0) {
        return(c(out, limit))
      }
      out <- c(out, new)
      if (log_den(new) <= target) {
        return(out)
      }
    }
    out
  }
  x <- c(extend(x[1], -1, lim[1]), x, extend(x[length(x)], 1, lim[2]))
  x <- sort(unique(x[is.finite(x)]))
  lx <- log_den(x)
  m <- length(x)
  # Grid interval i is crossed by every level L with lo[i] <= L < hi[i]. With
  # the levels sorted, findInterval() gives the range of levels crossing each
  # interval, so all crossings are found without scanning the grid for each
  # observation.
  level <- -s
  ok <- which(!is.na(s))
  ord <- ok[order(level[ok])]
  sorted <- level[ord]
  lo <- pmin(lx[-m], lx[-1])
  hi <- pmax(lx[-m], lx[-1])
  first <- findInterval(lo, sorted, left.open = TRUE) + 1
  count <- pmax(0, findInterval(hi, sorted, left.open = TRUE) - first + 1)
  obs <- ord[sequence(count, from = first)]
  i <- rep(seq_len(m - 1), count)
  root <- find_crossings(log_den, x[i], x[i + 1], lx[i], lx[i + 1], level[obs])
  # Each observation's probability is the sum of the gaps between the runs
  # where the density is above its own (rather than 1 minus the runs), so
  # small tail probabilities keep their precision. The run boundaries are the
  # crossings, plus the ends of the grid when the density there is above the
  # observation's. In order along the grid, the gaps are (e1, e2), (e3, e4), ...
  # with e1 = 0 and the last = 1, so p is the alternating sum of the edges.
  n <- length(s)
  first_above <- which(lx[1] > level)
  last_above <- which(lx[m] > level)
  edge <- c(rep(0, n), rep(0, length(first_above)), cdf(root),
    rep(1, length(last_above)), rep(1, n))
  group <- c(seq_len(n), first_above, obs, last_above, seq_len(n))
  key <- c(rep(0, n), rep(1, length(first_above)), 1 + i,
    rep(m + 1, length(last_above)), rep(m + 2, n))
  o <- order(group, key)
  position <- sequence(tabulate(group, nbins = n))
  sign <- ifelse(position %% 2 == 0, 1, -1)
  p <- numeric(n)
  p[sort(unique(group))] <- rowsum(sign * edge[o], group[o])[, 1]
  # Levels above the whole grid (no outcome more likely) or below it
  p[level >= max(lx)] <- 1
  p[level < min(lx)] <- 0
  p[is.na(s)] <- NA_real_
  pmax(0, pmin(1, p))
}

# Points where f(z) = level, each bracketed by [a, b] with f(a) = fa and
# f(b) = fb on either side of it, all solved at once by false position
# (Illinois variant), with a bisection step every fifth iteration in case it
# stalls (f is clamped to a large negative value at the edges of the support)
find_crossings <- function(f, a, b, fa, fb, level) {
  ga <- fa - level
  gb <- fb - level
  root <- (a + b) / 2
  active <- seq_along(a)
  last <- integer(length(a))
  for (iter in seq_len(100)) {
    if (length(active) == 0L) {
      break
    }
    A <- a[active]
    B <- b[active]
    gA <- ga[active]
    gB <- gb[active]
    z <- if (iter %% 5 == 0) (A + B) / 2 else (A * gB - B * gA) / (gB - gA)
    z <- pmin(pmax(z, pmin(A, B)), pmax(A, B))
    gz <- f(z) - level[active]
    root[active] <- z
    # Keep the endpoint on the other side of the crossing; halve the value at
    # an endpoint retained twice in a row (the Illinois modification)
    left <- sign(gz) == sign(gA)
    a[active[left]] <- z[left]
    ga[active[left]] <- gz[left]
    b[active[!left]] <- z[!left]
    gb[active[!left]] <- gz[!left]
    side <- ifelse(left, 1L, 2L)
    repeat_left <- left & last[active] == 1L
    repeat_right <- !left & last[active] == 2L
    gb[active[repeat_left]] <- gb[active[repeat_left]] / 2
    ga[active[repeat_right]] <- ga[active[repeat_right]] / 2
    last[active] <- side
    done <- abs(gz) < 1e-10 |
      abs(b[active] - a[active]) <= 1e-12 * pmax(1, abs(z))
    active <- active[!done]
  }
  root
}

# Check if all distributions are discrete (integer-valued support)
is_discrete <- function(dist) {
  all(vapply(
    vctrs::field(distributional::support(dist), "x"),
    is.integer,
    logical(1)
  ))
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
