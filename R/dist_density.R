#' Create distributional object based on a specified density
#'
#' Creates a distributional object using a density specified as pair of vectors
#' giving (x, f(x)). The density is assumed to be piecewise linear between the
#' points provided, and 0 outside the range of x.
#'
#' @param x Numerical vector of ordinates, or a list of such vectors.
#' @param density Numerical vector of density values, or a list of such vectors.
#' @examples
#' dist_density(seq(-4, 4, by = 0.01), dnorm(seq(-4, 4, by = 0.01)))
#'
#' @export

dist_density <- function(x, density) {
  if (!is.list(x)) {
    x <- list(x)
    density <- list(density)
  }
  if (length(x) != length(density)) {
    stop("x and density must be lists of the same length")
  }
  n <- lengths(x)
  if (any(n != lengths(density))) {
    stop("x and density must have the same length")
  }
  # Ensure density integrates to 1
  density <- mapply(
    function(x, f) f / integral(x, f),
    x,
    density,
    SIMPLIFY = FALSE
  )
  # Make sure x and f are ordered
  idx <- lapply(x, order)
  x <- mapply(function(u, i) u[i], x, idx, SIMPLIFY = FALSE)
  density <- mapply(function(f, idx) f[idx], density, idx, SIMPLIFY = FALSE)
  # Construct result
  output <- distributional::new_dist(x = x, f = density, class = "dist_density")
  # Replace degenerate distributions
  degenerate <- (n == 1L)
  if (any(degenerate)) {
    output[degenerate] <- distributional::dist_degenerate(x = x[degenerate])
  }
  return(output)
}

#' @export
format.dist_density <- function(x, ...) {
  sprintf("density[%s]", length(x[["x"]]))
}

#' @export
density.dist_density <- function(x, at, ..., na.rm = TRUE) {
  d <- stats::approx(x$x, x$f, xout = at)$y
  d[is.na(d)] <- 0
  return(d)
}

#' @exportS3Method distributional::log_density
log_density.dist_density <- function(x, at, ..., na.rm = TRUE) {
  log(density.dist_density(x, at = at, ..., na.rm = na.rm))
}

#' @exportS3Method distributional::cdf
cdf.dist_density <- function(x, q, ..., na.rm = TRUE) {
  # Compute CDF at density ordinates
  F <- cumintegral(x$x, x$f)
  stats::approx(F$x, F$y, xout = q, yleft = 0, yright = 1, ..., na.rm = na.rm)$y
}

#' @export
quantile.dist_density <- function(x, p, ..., na.rm = TRUE) {
  # Compute CDF at density ordinates
  F <- cumintegral(x$x, x$f)
  stats::approx(
    F$y,
    F$x,
    xout = p,
    yleft = min(x$x),
    yright = max(x$x),
    ties = mean,
    ...,
    na.rm = na.rm
  )$y
}

#' @exportS3Method distributional::generate
generate.dist_density <- function(x, times, ...) {
  # Set up fine grid
  n <- length(x$x)
  delta <- (x$x[n] - x$x[1]) / 1000
  xgrid <- seq(x$x[1] + delta / 2, x$x[n] - delta / 2, l = 1000)
  ygrid <- stats::approx(x$x, x$f, xout = xgrid)$y
  sample(xgrid, size = times, replace = TRUE, prob = ygrid) +
    stats::runif(times, -delta / 2, delta / 2)
}

#' @export
mean.dist_density <- function(x, ...) {
  density_moment(x)
}

#' @exportS3Method stats::median
median.dist_density <- function(x, na.rm = FALSE, ...) {
  quantile(x, 0.5, na.rm = na.rm, ...)
}

#' @exportS3Method distributional::covariance
covariance.dist_density <- function(x, ...) {
  density_moment(x, 2)
}

#' @exportS3Method distributional::skewness
skewness.dist_density <- function(x, ..., na.rm = FALSE) {
  density_moment(x, 3) / distributional::variance(x)^1.5
}

#' @exportS3Method distributional::kurtosis
# Excess kurtosis for consistency with distributional package
kurtosis.dist_density <- function(x, ..., na.rm = FALSE) {
  density_moment(x, 4) / distributional::variance(x)^2 - 3
}

# Trapezoidal integration of y(x) over x
# Assumes x is ordered and x and y are the same length
# Returns cumulative integral over a grid
cumintegral <- function(x, y, grid = TRUE) {
  n <- length(x)
  if (n == 1) {
    return(list(x = x, y = 0))
  }
  if (grid) {
    # Set up fine grid
    xgrid <- seq(x[1], x[n], l = 1001)
    ygrid <- stats::approx(x, y, xout = xgrid)$y
  } else {
    xgrid <- x
    ygrid <- y
  }
  # Apply trapezoidal rule
  cell <- 0.5 * (ygrid[1:1000] + ygrid[2:1001]) * (xgrid[2] - xgrid[1])
  list(x = xgrid, y = cumsum(c(0, cell)))
}

# Trapezoidal integration of y(x) over x
integral <- function(x, y, grid = TRUE) {
  tail(cumintegral(x, y, grid = grid)$y, 1)
}

# Integral of either x^k * f(x) over x or (x - mean)^k * f(x) over x
# Can't use integral directly because x^k * f(x) is not piecewise linear
density_moment <- function(x, k = 1, central = (k > 1)) {
  # Set up fine grid
  xgrid <- seq(head(x$x, 1), tail(x$x, 1), l = 1001)
  fgrid <- stats::approx(x$x, x$f, xout = xgrid)$y
  integrand <- if (central) {
    (xgrid - mean(x))^k * fgrid
  } else {
    xgrid^k * fgrid
  }
  integral(xgrid, integrand, grid = FALSE)
}
