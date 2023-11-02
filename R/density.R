#' Kernel density estimation
#'
#' Kernel density estimation for multivariate data up to 6 dimensions. The
#' bandwidths are chosen to be suitable for tail estimation. The calculations
#' are handled by the \code{\link[ks]{kde}} function, but using different default
#' bandwidths.
#'
#' @param x A numeric vector or matrix of data values.
#' @param h A numeric vector of bandwidths for univariate data. The default is
#' obtained using \code{\link[ks]{hns}}.
#' @param H A numeric matrix of bandwidths for multivariate data. The default is
#' obtained using \code{\link[ks]{Hns}}.
#' @param ... Additional arguments are passed to \code{\link[ks]{kde}}.
#' @examples
#' # Univariate density
#' y <- c(rnorm(100), rnorm(100, 3, 1))
#' density(y) |>
#'   autoplot() +
#'   geom_rug(data = tibble(y=y), aes(x=y))
#' # Bivariate density
#' tibble(y1 = rnorm(100), y2 = y1 + rnorm(100)) |>
#'   density() |>
#'   autoplot(filled = TRUE)
#' @export
density.matrix <- function(x, h, H, ...) {
  y <- as.matrix(x)
  d <- NCOL(y)
  r <- apply(apply(y, 2, range), 2, diff)
  if (any(r == 0)) {
    stop("Insufficient data")
  }
  if (missing(h) & NCOL(y) == 1) {
    h <- ks::hns(y[, 1])
  } else if (missing(H)) {
    H <- ks::Hns(y)
  }
  output <- ks::kde(y, h = h, H = H, binned = length(y) > 1000, ...)
  output$names <- colnames(y)
  return(output)
}

#' @export
#' @rdname density.matrix
density.data.frame <- function(x, h, H, ...) {
  density.matrix(as.matrix(x), h, H, ...)
}

#' @export
#' @rdname density.matrix
density.numeric <- function(x, h, ...) {
  output <- density.matrix(as.matrix(x), h=h, ...)
  output$names <- deparse(substitute(x))
  return(output)
}

#' @export
print.kde <- function(x, ...) {
  kde <- !(is.null(x$h) & is.null(x$H))
  if (inherits(x$eval.points, "list")) {
    d <- length(x$eval.points)
  } else {
    d <- 1L
  }
  if(!kde) {
    cat("Density of: [",
        paste0(x$names, collapse = ", "), "]\n", sep = "")
  } else {
    cat("Kernel density estimate of:",
        paste0(x$names, collapse = ", "), "]\n", sep = "")
  }
  ngrid <- lapply(x$eval.points, length)
  cat("Computed on a grid of size", paste(ngrid, collapse = " x "), "\n")
  if(kde) {
    cat("Bandwidth: ")
    if (d == 1L) {
      cat("h = ", format(x$h, digits = 4))
    } else {
      cat("H = \n")
      print(format(x$H, digits = 4), quote=FALSE)
    }
  }
  invisible(x)
}

#' Convert list object to density class
#'
#' @param object List with components `y` (a matrix of points) and `density`
#' (the value of the density at each row of `y`)
#' @param ngrid Number of points to use for the grid in each dimension. Default is
#' 10001 for univariate densities and 101 for multivariate densities.
#' @param ... Additional arguments are ignored.
#' @return An object of class "kde"
#' @export

as_kde <- function(object, ngrid, ...) {
  # Check object components
  if (!all(c("y", "density") %in% names(object))) {
    stop("object must be a list with components y and density")
  }
  # Find the dimension
  d <- NCOL(object$y)
  if (d == 1L) {
    # Interpolate density on finer grid
    density <- list(eval.points = seq(min(object$y), max(object$y), length = 10001))
    density$estimate <- approx(object$y, object$density, xout = density$eval.points)$y
  } else if (d == 2L) {
    density <- density_on_grid(as.matrix(object$y), object$density, 101)
  } else {
    stop("Only univariate and bivariate densities are supported")
  }
  # Find falpha using quantile method
  missing <- is.na(density$estimate)
  samplex <- sample(density$estimate[!missing],
    size = 50000, replace = TRUE,
    prob = density$estimate[!missing]
  )
  density$cont <- quantile(samplex, prob = (99:1) / 100, type = 8)
  # Set missing values to 0
  density$estimate[is.na(density$estimate)] <- 0
  # Add names
  density$names <- colnames(object$y)
  structure(density, class = "kde")
}

density_on_grid <- function(y, fy, ngrid) {
  y <- as.matrix(y)
  if (NCOL(y) != 2L) {
    stop("y must be a matrix with 2 columns")
  }
  # Create grid of points
  density <- list(eval.points = list(
    seq(min(y[, 1]), max(y[, 1]), length = ngrid),
    seq(min(y[, 2]), max(y[, 2]), length = ngrid)
  ))
  # Bivariate interpolation
  grid <- expand.grid(density$eval.points[[1]], density$eval.points[[2]])
  density$estimate <- interp::interpp(
    x = y[, 1], y = y[, 2], z = fy,
    xo = grid[, 1], yo = grid[, 2]
  )$z |>
    suppressWarnings() |>
    matrix(nrow = ngrid)
  return(density)
}
