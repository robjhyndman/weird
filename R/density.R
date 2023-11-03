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
    cat("Kernel density estimate of: [",
        paste0(x$names, collapse = ", "), "]\n", sep = "")
  }
  if(d == 1L){
    ngrid <- length(x$eval.points)
  } else {
    ngrid <- lapply(x$eval.points, length)
  }
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
#' @author Rob J Hyndman
#' @examples
#' y <- seq(-4, 4, by = 0.01)
#' as_kde(list(y = y, density = dnorm(y)))
#' @export

as_kde <- function(object, ngrid, ...) {
  # Check object components
  if (!all(c("y", "density") %in% names(object))) {
    stop("object must be a list with components y and density")
  }
  # Find the dimension
  d <- NCOL(object$y)
  if (d == 1L) {
    if(missing(ngrid)) {
      ngrid <- 10001
    }
    # Interpolate density on finer grid
    density <- list(eval.points = seq(min(object$y), max(object$y), length = ngrid))
    density$estimate <- approx(object$y, object$density, xout = density$eval.points)$y
  } else if (d == 2L) {
    if(missing(ngrid)) {
      ngrid <- 101
    }
    density <- density_on_grid(as.matrix(object$y), object$density, ngrid)
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
  if(is.null(density$names)) {
    if(d == 1) {
      density$names <- "y"
    } else {
      density$names <- paste0("y", seq_len(d))
    }
  }
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
