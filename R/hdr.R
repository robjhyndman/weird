#' @title Table of Highest Density Regions
#' @description
#' Compute the highest density regions (HDR) for a kernel density estimate. The HDRs
#' are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `density` (the value of the density at the boundary of the HDR),
#' `mode` (the local mode equal to the highest density value within the corresponding HDR).
#' For one dimensional density functions, the tibble also has columns
#' `lower` (the lower ends of the intervals), and
#' `upper` (the upper ends of the interval).
#'
#' @param y Numerical vector or matrix of data
#' @param density Probablity density function, either estimated by `ks::kde()` or
#' a list with components `x` and `y` defining the density function.
#' @param prob Probability of the HDR
#' @param ... If `y` is supplied, other arguments are passed to \code{\link[ks]{kde}}.
#' Otherwise, additional arguments are ignored.
#' @return A tibble
#' @author Rob J Hyndman
#' @examples
#' y <- c(rnorm(100), rnorm(100, 3, 1))
#' hdr_table(y = y)
#' hdr_table(density = ks::kde(y))
#' x <- seq(-4, 4, by = 0.01)
#' hdr_table(density = list(x = x, y = dnorm(x)), prob = 0.95)
#' @export
hdr_table <- function(y = NULL, density = NULL,
                      prob = c(0.50, 0.99), ...) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  alpha <- sort(1 - prob)
  # Data supplied
  if (!is.null(y)) {
    r <- diff(range(y))
    if (r == 0) {
      stop("Insufficient data")
    }
    if (!is.null(density)) {
      warning("Ignoring density")
    }
    density <- ks::kde(y, binned = length(y) > 1000, ...)
    falpha <- approx(seq(99)/100, density$cont, xout = 1 - alpha)$y
  } else if (!inherits(density, "kde")) {
    # Interpolate density on finer grid
    density$eval.points <- seq(min(density$x), max(density$x), length = 10000)
    density$estimate <- approx(density$x, density$y, xout = density$eval.points)$y
    # Find falpha using quantile method
    samplex <- sample(density$estimate, size = 50000, replace = TRUE, prob = density$estimate)
    falpha <- quantile(samplex, prob = 1 - alpha, type = 8)
  } else {
    falpha <- approx(seq(99)/100, density$cont, xout = 1 - alpha)$y
  }
  hdr.store <- tibble(
    prob = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    mode = numeric(0),
    density = numeric(0),
  )
  for (i in 1:length(alpha)) {
    junk <- hdr.ends(density, falpha[i])$hdr
    n <- length(junk) / 2
    for (j in seq(n)) {
      within <- density$eval.points >= junk[2 * j - 1] & density$eval.points <= junk[2 * j]
      subden <- list(x = density$eval.points[within], y = density$estimate[within])
      hdr.store <- dplyr::bind_rows(
        hdr.store,
        tibble(
          prob = 1 - alpha[i], lower = junk[2 * j - 1], upper = junk[2 * j],
          mode = subden$x[which.max(subden$y)],
          density = falpha[i]
        )
      )
    }
  }
  return(hdr.store)
}

# Remaining functions adapted from hdrcde package

hdr.ends <- function(den, falpha) {
  # falpha is above the density, so the HDR does not exist
  if (falpha > max(den$estimate)) {
    return(list(falpha = falpha, hdr = NA))
  }
  # Return density at specific x values
  f <- function(x, den, falpha) {
    approx(den$eval.points, den$estimate - falpha, xout = x)$y
  }
  # Find all end points of HDR
  intercept <- all_roots(f, interval = range(den$eval.points), den = den, falpha = falpha)
  ni <- length(intercept)
  # No roots -- use the whole line
  if (ni == 0L) {
    intercept <- c(den$eval.points[1], den$eval.points[n])
  } else {
    n <- length(den$eval.points)
    # Check behaviour outside the smallest and largest intercepts
    if (f(0.5 * (intercept[1] + den$eval.points[1]), den, falpha) > 0) {
      intercept <- c(den$eval.points[1], intercept)
    }
    if (f(0.5 * (utils::tail(intercept, 1) + den$eval.points[n]), den, falpha) > 0) {
      intercept <- c(intercept, den$eval.points[n])
    }
  }
  # Check behaviour -- not sure if we need this now
  if (length(intercept) %% 2) {
    warning("Some HDRs are incomplete")
  }
  #  intercept <- sort(unique(intercept))
  return(list(falpha = falpha, hdr = intercept))
}


all_roots <- function(
    f, interval,
    lower = min(interval), upper = max(interval), n = 100L, ...) {
  x <- seq(lower, upper, len = n + 1L)
  fx <- f(x, ...)
  roots <- x[which(fx == 0)]
  fx2 <- fx[seq(n)] * fx[seq(2L, n + 1L, by = 1L)]
  index <- which(fx2 < 0)
  for (i in index) {
    roots <- c(roots, stats::uniroot(f, lower = x[i], upper = x[i + 1L], ...)$root)
  }
  return(sort(roots))
}

#' @importFrom utils head tail
#' @importFrom tibble tibble
utils::globalVariables(c("ends", "type", "lower", "upper"))
