#' Create kde distributional object
#'
#' Creates a distributional object from a kernel density estimate. The
#' `ks::kde()` function is used to compute the density. The cdf and quantiles
#' are consistent with the kde.
#' @details This is a replacement for `distributional::dist_kde()` which also
#' uses a kde when computing the density. However, it uses `stats::density()` on
#' the univariate margins, rather than computing a potentially multivariate
#' density using `ks::kde()`. Also, the quantiles and cdf of a `dist_kde`
#' object are not consistent with the density estimate. (e.g., the cdf is not
#' the integral of the density, and the quantiles are not the inverse of the
#' cdf).
#' @param y Numerical vector or matrix of data, or a list of such objects. If a
#' list is provided, then all objects should be of the same dimension. e.g.,
#' all vectors, or all matrices with the same number of columns.
#' @param bandwidth Function to compute bandwidth.
#' @param multiplier to be used for bandwidth. Set to 2 for anomaly detection, and 1 otherwise.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @examples
#' dist_kde(c(rnorm(200), rnorm(100, 5)))
#'
#' @export

dist_kde <- function(
    y,
    bandwidth = kde_bandwidth,
    k = 1,
    ...) {
  if (!is.list(y)) {
    y <- list(y)
  }
  d <- unlist(lapply(y, function(u) NCOL(u)))
  if (!all(d == d[1])) {
    stop("All data sets must have the same dimension")
  }
  y <- vctrs::as_list_of(y, .ptype = vctrs::vec_ptype(y[[1]]))
  # Estimate density using ks::kde
  density <- lapply(
    y,
    function(u) {
      # browser()
      if (NCOL(u) == 1L) {
        ks::kde(u, h = kde_bandwidth(u, method = "double"), ...)
      } else {
        ks::kde(u, H = kde_bandwidth(u, method = "double"), ...)
      }
    }
  )
  # Convert to distributional object
  distributional::new_dist(kde = density, class = "dist_kde")
}

#' @export
format.dist_kde <- function(x, ...) {
  d <- vapply(x, function(u) {
    NCOL(u$x)
  }, integer(1L))
  ngrid <- vapply(x, function(u) {
    length(u$eval.points)
  }, integer(1L))
  if (d == 1) {
    # Find bandwidth and convert to string
    h <- vapply(x, function(u) {
      u$h
    }, numeric(1L))
    sprintf("kde[%sd, h=%.2g]", d, h)
  } else {
    # Create matrix as string
    H <- c(vapply(x, function(u) {
      u$H
    }, numeric(d * d)))
    Hstring <- "{"
    for (i in seq(d)) {
      Hstring <- paste0(
        Hstring, "(",
        paste0(sprintf("%.2g", H[(i - 1) * d + seq(d)]),
          sep = "", collapse = ", "
        ),
        ")'"
      )
      if (i < d) {
        Hstring <- paste0(Hstring, ", ")
      }
    }
    Hstring <- paste0(Hstring, "}")
    sprintf("kde[%sd, H=%s]", d, Hstring)
  }
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
#' kde_bandwidth(oldfaithful[, 2:3])
#' @export

kde_bandwidth <- function(data, method = c("robust_normal", "double", "lookout"),
                          max.iter = 2) {
  method <- match.arg(method)
  d <- NCOL(data)
  n <- NROW(data)
  if (d > 1) {
    # Find robust covariance matrix of data
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_IQR)$cov
  }
  if (method != "lookout") {
    k <- ifelse(method == "double", 2, 1)
    if (d == 1L) {
      return(k * 1.06 * robustbase::s_IQR(data) * n^(-0.2))
    } else {
      return((4 / (n * (d + 2)))^(2 / (d + 4)) * k^2 * S)
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

#' @export
density.dist_kde <- function(x, at, ..., na.rm = TRUE) {
  d <- NCOL(x$kde$x)
  if (d == 1) {
    d <- stats::approx(x$kde$eval.points, x$kde$estimate, xout = at)$y
  } else {
    stop("Not yet implemented")
  }
  d[is.na(d)] <- 0
  return(d)
}

#' @exportS3Method distributional::cdf
cdf.dist_kde <- function(x, q, ..., na.rm = TRUE) {
  # Apply independently over sample variates
  if (is.matrix(x$x)) {
    return(
      apply(x$x, 2,
        function(x, ...) cdf.dist_kde(list(x = x), ...),
        q = q, ..., na.rm = TRUE
      )
    )
  }
  # Integrate density
  delta <- x$kde$eval.points[2] - x$kde$eval.points[1]
  F <- cumsum(x$kde$estimate) * delta
  stats::approx(x$kde$eval.points, F, xout = q, yleft = 0, yright = 1, ..., na.rm = na.rm)$y
}

#' @exportS3Method distributional::generate
generate.dist_kde <- function(x, times, ...) {
  d <- NCOL(x$kde$x)
  if (d == 1) {
    h <- x$kde$h
    sample(x$kde$x, size = times, replace = TRUE) + rnorm(times, sd = h)
  } else {
    stop("Not yet implemented")
  }
}

#' @export
mean.dist_kde <- function(x, ...) {
  if (is.matrix(x$kde$x)) {
    apply(x$kde$x, 2, mean, ...)
  } else {
    mean(x$kde$x, ...)
  }
}

#' @export
median.dist_kde <- function(x, na.rm = FALSE, ...) {
  if (is.matrix(x$kde$x)) {
    apply(x$kde$x, 2, median, na.rm = na.rm, ...)
  } else {
    median(x$kde$x, na.rm = na.rm, ...)
  }
}

#' @exportS3Method distributional::covariance
covariance.dist_kde <- function(x, ...) {
  if (is.matrix(x$kde$x)) {
    stats::cov(x$kde$x, ...)
  } else {
    stats::var(x$kde$x, ...) + x$kde$h^2
  }
}

#' @exportS3Method distributional::skewness
skewness.dist_kde <- function(x, ..., na.rm = FALSE) {
  if (is.matrix(x$kde$x)) {
    abort("Multivariate sample skewness is not yet implemented.")
  }
  n <- lengths(x$kde$x, use.names = FALSE)
  x <- lapply(x$kde$x, function(.) . - mean(., na.rm = na.rm))
  sum_x2 <- vapply(x$kde$x, function(.) sum(.^2, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  sum_x3 <- vapply(x$kde$x, function(.) sum(.^3, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  y <- sqrt(n) * sum_x3 / (sum_x2^(3 / 2))
  y * ((1 - 1 / n))^(3 / 2)
}
