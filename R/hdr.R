#' @title Table of Highest Density Regions
#' @description
#' Compute the highest density regions (HDR) for a kernel density estimate. The HDRs
#' are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `density` (the value of the density at the boundary of the HDR),
#' For one dimensional density functions, the tibble also has columns
#' `lower` (the lower ends of the intervals), and
#' `upper` (the upper ends of the interval).
#' @param y Numerical vector or matrix of data
#' @param density Probability density function, either estimated by `ks::kde()` or
#' a list with components `x` and `y` defining the density function.
#' (Here `x` can be a vector or a matrix, and `y` is a vector containing the density values
#' for each row of `x`.)
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
#' x <- seq(-4, 4, by = 0.01)
#' hdr_table(density = list(x = x, y = dnorm(x)), prob = 0.95)
#' # Bivariate HDRs
#' y <- cbind(rnorm(100), rnorm(100))
#' hdr_table(y = y)
#' @export
hdr_table <- function(y = NULL, density = NULL,
    prob = c(0.50, 0.99), h, H, ...) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  alpha <- sort(1 - prob)
  # Data supplied
  if (!is.null(y)) {
    y <- as.matrix(y)
    d <- NCOL(y)
    r <- apply(apply(y, 2, range), 2, diff)
    if (any(r == 0)) {
      stop("Insufficient data")
    }
    if (!is.null(density)) {
      warning("Ignoring density")
    }
    if(missing(h) & NCOL(y) == 1) {
      h <- ks::hns(y[,1])
    } else if(missing(H)) {
      H <- ks::Hns.diag(y)
    }
    density <- ks::kde(y, h = h, H = H, binned = length(y) > 1000, ...)
    falpha <- approx(seq(99)/100, density$cont, xout = 1 - alpha)$y
  } else if (!inherits(density, "kde")) {
    # Find the dimension
    d <- NCOL(density$x)
    if(d == 1L) {
      # Interpolate density on finer grid
      density$eval.points <- seq(min(density$x), max(density$x), length = 10000)
      density$estimate <- approx(density$x, density$y, xout = density$eval.points)$y
    } else if(d == 2L) {
      density$x <- as.matrix(density$x)
      # Create grid of points
      density$eval.points <- list(
        seq(min(density$x[,1]), max(density$x[,1]), length=500),
        seq(min(density$x[,2]), max(density$x[,2]), length=500)
      )
      # Bivariate interpolation
      density$estimate <- interp::bilinear(x = density$x[,1], y = density$x[,2],
        z = density$y, x0 = density$eval.points[[1]], y0 = density$eval.points[[2]])
    } else {
      stop("Only univariate and bivariate densities are supported")
    }
    # Find falpha using quantile method
    samplex <- sample(density$estimate, size = 50000, replace = TRUE, prob = density$estimate)
    falpha <- quantile(samplex, prob = alpha, type = 8)
  } else {
    if(inherits(density$eval.points, "list")) {
      d <- length(density$eval.points)
    } else {
      d <- 1L
    }
    falpha <- approx(seq(99)/100, density$cont, xout = 1 - alpha)$y
  }
  if(d == 1L) {
    # Find endpoints of each interval
    hdr.store <- tibble(
      prob = numeric(0),
      lower = numeric(0),
      upper = numeric(0),
      #mode = numeric(0),
      density = numeric(0),
    )
    for (i in seq_along(alpha)) {
      junk <- hdr.ends(density, falpha[i])$hdr
      n <- length(junk) / 2
      for (j in seq(n)) {
        #within <- density$eval.points >= junk[2 * j - 1] & density$eval.points <= junk[2 * j]
        #subden <- list(x = density$eval.points[within], y = density$estimate[within])
        hdr.store <- dplyr::bind_rows(
          hdr.store,
          tibble(
            prob = 1 - alpha[i], lower = junk[2 * j - 1], upper = junk[2 * j],
            #mode = subden$x[which.max(subden$y)],
            density = falpha[i]
          )
        )
      }
    }
  } else {
    # Just return the density on the relevant contours
    # Find mode
    #j <- density$estimate == max(density$estimate)
    #mode <- numeric(d)
    #for(i in seq(d)) {
    #  idx <- which(apply(j, seq(d)[-i], sum) == 1)
    #  mode[i] <- density$eval.points[[i]][idx]
    #}
    hdr.store <- tibble(
      prob = 1-alpha,
      #mode = list(mode),
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
