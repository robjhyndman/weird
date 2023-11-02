#' @title Table of Highest Density Regions
#' @description
#' Compute the highest density regions (HDR) for a kernel density estimate. The HDRs
#' are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `density` (the value of the density at the boundary of the HDR),
#' For one dimensional density functions, the tibble also has columns
#' `lower` (the lower ends of the intervals),
#' `upper` (the upper ends of the interval),
#' `mode` (the point at which the density is maximized within each interval).
#' @param y Numerical vector or matrix of data
#' @param density Probability density function, either estimated by `ks::kde()` or
#' a list with components `y` and `density` defining the density function.
#' (Here `y` can be a vector or a matrix, and `density` is a vector containing the density values
#' for each row of `y`.)
#' @param prob Probability of the HDR
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link[ks]{hns}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link[ks]{Hns.diag}}.
#' @param ... If `y` is supplied, other arguments are passed to \code{\link[ks]{kde}}.
#' Otherwise, additional arguments are ignored.
#' @return A tibble
#' @author Rob J Hyndman
#' @examples
#' # Univariate HDRs
#' y <- c(rnorm(100), rnorm(100, 3, 1))
#' hdr_table(y = y)
#' hdr_table(density = ks::kde(y))
#' y <- seq(-4, 4, by = 0.01)
#' hdr_table(density = list(y = y, density = dnorm(y)), prob = 0.95)
#' # Bivariate HDRs
#' y <- cbind(rnorm(100), rnorm(100))
#' hdr_table(y = y)
#' grid <- seq(-4, 4, by=0.1)
#' y <- expand.grid(grid, grid) |> as.matrix()
#' hdr_table(density = list(y = y, density = dnorm(y[,1]) * dnorm(y[,2])))
#' @export
hdr_table <- function(y = NULL, density = NULL,
    prob = c(0.50, 0.99), h, H, ...) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  alpha <- sort(1 - prob)
  if (!is.null(y)) {
    # Data supplied
    if (!is.null(density)) {
      warning("Ignoring density")
    }
    density <- density(y, h, H, ...)
  } else if (!inherits(density, "kde")) {
    # Density given as list(y, density)
    density <- as_kde(density)
  }
  falpha <- approx(seq(99)/100, density$cont, xout = 1 - alpha)$y
  if(inherits(density$eval.points, "list")) {
    d <- length(density$eval.points)
  } else {
    d <- 1L
  }
  if(d == 1L) {
    # Find endpoints of each interval
    hdr.store <- tibble(
      prob = numeric(0),
      lower = numeric(0),
      upper = numeric(0),
      mode = numeric(0),
      density = numeric(0),
    )
    for (i in seq_along(alpha)) {
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
  } else {
    # Just return the density on the relevant contours
    hdr.store <- tibble(
      prob = 1-alpha,
      density = falpha
    )
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
