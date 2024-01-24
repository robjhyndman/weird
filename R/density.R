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
      cat(format(x$H, digits = 4), quote=FALSE)
    }
  }
  invisible(x)
}

#' Convert data frame or matrix object to kde class
#'
#' A density specified as a data frame or matrix can be converted to a kde object.
#' This is useful for plotting the density using \code{\link{autoplot.kde}}.
#' As kde objects are defined on a grid, the density values are interpolated
#' based on the points in the data frame or matrix.
#'
#' @param object Data frame or matrix with numerical columns, where one column
#' (specified by `density_column`) contains the density values, and the
#' remaining columns define the points at which the density is evaluated.
#' @param density_column Name of the column containing the density values, specified
#' as a bare expression. If missing, the last column is used.
#' @param ngrid Number of points to use for the grid in each dimension. Default is
#' 10001 for univariate densities and 101 for multivariate densities.
#' @param ... Additional arguments are ignored.
#' @return An object of class "kde"
#' @author Rob J Hyndman
#' @examples
#' tibble(y = seq(-4, 4, by = 0.01), density = dnorm(y)) |>
#'   as_kde()
#' @export

as_kde <- function(object, density_column, ngrid, ...) {
  # Check columns of object are all numerical
  object <- as.data.frame(object)
  if(!all(sapply(object, is.numeric))) {
    stop("All columns of object must be numeric")
  }
  # Check density_column is in object
  if(missing(density_column)) {
    density_column <- tail(colnames(object), 1)
  } else {
    density_column <- dplyr::as_label(dplyr::enquo(density_column))
  }
  if(!(density_column %in% colnames(object))) {
    stop(paste(density_column, "not found"))
  }
  # Separate points from density values
  den <- object[[density_column]]
  object[[density_column]] <- NULL
  # Find the dimension
  d <- NCOL(object)
  if (d == 1L) {
    if(missing(ngrid)) {
      ngrid <- 10001
    }
    # Interpolate density on finer grid
    density <- list(eval.points = seq(min(object), max(object), length = ngrid))
    density$estimate <- approx(object[[1]], den, xout = density$eval.points)$y
  } else if (d == 2L) {
    if(missing(ngrid)) {
      ngrid <- 101
    }
    density <- density_on_grid(as.matrix(object), den, ngrid)
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
  density$names <- colnames(object)
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
  ifun <- interpolation::interpfun(x = y[, 1], y = y[, 2], z = fy)
  density$estimate <- ifun(grid[,1], grid[,2]) |>
    matrix(nrow = ngrid)
  return(density)
}

#' Robust bandwidth estimation for kernel density estimation
#'
#' @param data A numeric matrix or data frame.
#' @param method Method to use for selecting the bandwidth.
#' `robust_normal` uses a robust version of the normal reference rule.
#' `lookout` uses the topological data analysis approach that is part of the lookout algorithm.
#' @param max.iter How many times should the `lookout` method be iterated. That is, outliers
#' (probability < 0.05) are removed and the bandwidth is re-computed from the
#' remaining observations.
#' @return A matrix of bandwidths (or scalar in the case of univariate data).
#' @author Rob J Hyndman
#' @examples
#' # Univariate bandwidth calculation
#' kde_bandwidth(oldfaithful$duration)
#' # Bivariate bandwidth calculation
#' kde_bandwidth(oldfaithful[,2:3])
#' @export

kde_bandwidth <- function(data, method = c("robust_normal", "double", "lookout"),
                          max.iter = 2) {
  method <- match.arg(method)
  d <- NCOL(data)
  n <- NROW(data)
  if(d > 1) {
    # Find robust covariance matrix of data
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_IQR)$cov
  }
  if(method != "lookout") {
    k <- ifelse(method == "double", 2, 1)
    if(d == 1L)
      return(k * 1.06 * robustbase::s_IQR(data) * n^(-0.2))
    else {
      return((4/(n * (d + 2)))^(2/(d + 4)) * k^2 * S)
    }
  } else {
    stop("Not yet implemented")
    # # Initial estimate
    # if(d == 1L) {
    #   S <- 1
    # } else {
    #   # Normalize data
    #   U <- chol(solve(S))
    #   data <- as.matrix(data) %*% t(U)
    # }
    # h <- lookout::find_tda_bw(data, fast = (n > 1000)) |>
    #   suppressWarnings()
    # iter <- 1
    # oldh <- 0
    # while(iter < max.iter & h != oldh) {
    #   iter <- iter + 1
    #   oldh <- h
    #   scores <- calc_kde_scores(data, h=h, H=h*diag(d))
    #   p <- lookout(density_scores = scores$scores, loo_scores = scores$loo) |>
    #     suppressWarnings()
    #   data <- as.matrix(data)[p > 0.05,]
    #   # Refined estimate
    #   h <- lookout::find_tda_bw(data, fast = (n > 1000))
    # }
    # return(h * S)
  }
}

