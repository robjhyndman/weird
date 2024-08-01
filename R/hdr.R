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
#' a data frame or matrix with numerical columns that can be passed to `as_kde()`.
#' @param prob Probability of the HDR
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param ... If `y` is supplied, other arguments are passed to \code{\link[ks]{kde}}.
#' Otherwise, additional arguments are passed to \code{\link{as_kde}}.
#' @return A tibble
#' @references Hyndman, R J. (1996) Computing and Graphing Highest Density Regions,
#' \emph{The American Statistician}, \bold{50}(2), 120–126.
#' @author Rob J Hyndman
#' @examples
#' # Univariate HDRs
#' y <- c(rnorm(100), rnorm(100, 3, 1))
#' hdr_table(y = y)
#' hdr_table(density = ks::kde(y))
#' x <- seq(-4, 4, by = 0.01)
#' hdr_table(density = data.frame(y = x, density = dnorm(x)), prob = 0.95)
#' # Bivariate HDRs
#' y <- cbind(rnorm(100), rnorm(100))
#' hdr_table(y = y)
#' grid <- seq(-4, 4, by = 0.1)
#' density <- expand.grid(grid, grid) |>
#'   mutate(density = dnorm(Var1) * dnorm(Var2))
#' hdr_table(density = density)
#' @export
hdr_table <- function(
    y = NULL, density = NULL,
    prob = c(0.50, 0.99), h = kde_bandwidth(y, method = "double"),
    H = kde_bandwidth(y, method = "double"), ...) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  alpha <- sort(1 - prob)
  if (!is.null(y)) {
    # Data supplied
    if (!is.null(density)) {
      warning("Ignoring density")
    }
    n <- NROW(y)
    if (NCOL(y) == 1L) {
      density <- ks::kde(y,
        h = h,
        gridsize = 10001, binned = n > 2000,
        approx.cont = FALSE, ...
      )
    } else {
      density <- ks::kde(y,
        H = H,
        gridsize = 101, binned = n > 2000,
        approx.cont = FALSE, ...
      )
    }
  } else if (!inherits(density, "kde")) {
    # Density given as list(y, density)
    density <- as_kde(density, ...)
  }
  falpha <- approx(seq(99) / 100, density$cont, xout = 1 - alpha)$y
  falpha[is.na(falpha) & alpha < 0.01] <- min(density$estimate)
  if (inherits(density$eval.points, "list")) {
    d <- length(density$eval.points)
  } else {
    d <- 1L
  }
  if (d == 1L) {
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
      prob = 1 - alpha,
      density = falpha
    )
  }
  return(hdr.store)
}

#' @title HDR plot
#' @description Produces a 1d or 2d box plot of HDR regions. The darker regions
#' contain observations with higher probability, while the lighter regions contain
#' points with lower probability. Points outside the largest HDR are shown as
#' individual points. Points with lookout probabilities
#' less than 0.05 are optionally shown in red.
#' @details The original HDR boxplot proposed by Hyndman (1996), R can be produced with
#' all arguments set to their defaults other than `lookout`.
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 Optionally, the name of the second variable to plot (a bare expression).
#' @param prob A numeric vector specifying the coverage probabilities for the HDRs.
#' @param scatterplot A logical argument indicating if a regular HDR plot is required
#' (\code{FALSE}), or if a scatterplot in the same colors is required (\code{TRUE}).
#' @param color The base color to use for the mode. Colors for the HDRs are generated
#' by whitening this color.
#' @param show_lookout A logical argument indicating if the plot should highlight observations with "lookout"
#' probabilities less than 0.05.
#' @param ... Other arguments passed to \code{\link[ks]{kde}}.
#' @return A ggplot object showing an HDR plot or scatterplot of the data.
#' @author Rob J Hyndman
#' @references Hyndman, R J (1996) Computing and Graphing Highest Density Regions,
#' *The American Statistician*, **50**(2), 120–126. \url{https://robjhyndman.com/publications/hdr/}
#' Kandanaarachchi, S & Hyndman, R J (2022) "Leave-one-out kernel density estimates for outlier detection",
#' *J Computational & Graphical Statistics*, **31**(2), 586-599. \url{https://robjhyndman.com/publications/lookout/}
#' @examples
#' df <- data.frame(x = c(rnorm(1000), rnorm(1000, 5, 1)))
#' df$y <- df$x + rnorm(200, sd = 2)
#' gg_hdrboxplot(df, x)
#' gg_hdrboxplot(df, x, y, scatterplot = TRUE)
#' oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   gg_hdrboxplot(duration, waiting, scatterplot = TRUE)
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   gg_hdrboxplot(Average)
#'
#' @rdname hdrplot
#' @export

gg_hdrboxplot <- function(data, var1, var2 = NULL, prob = c(0.5, 0.99),
                          color = "#00659e", scatterplot = FALSE,
                          show_lookout = FALSE, ...) {
  if (missing(var1)) {
    # Grab first variable
    data <- as.data.frame(data)
    var1 <- rlang::sym(names(data)[1])
    if (NCOL(data) > 1L) {
      message("No variable selected. Using ", rlang::as_name(var1))
    }
  }
  v2 <- dplyr::as_label(dplyr::enquo(var2))
  if (v2 == "NULL") {
    d <- 1L
    data <- data |> select({{ var1 }})
  } else {
    d <- 2L
    data <- data |> select({{ var1 }}, {{ var2 }})
  }
  dist <- dist_kde(data[,seq(d)], method = "double")

  if (d == 2L) {
    gg_density2(dist,
      prob = prob,
      colors = NULL, color = color, fill = TRUE, alpha = NULL,
      show_points = TRUE, show_mode = TRUE, show_lookout = show_lookout
    ) +
    ggplot2::guides(fill = "none")

  } else {
    gg_density1(dist,
      show_hdr = TRUE, show_density = FALSE, ngrid = 501,
      prob = prob, alpha = NULL, jitter = TRUE,
      color = color, fill = TRUE, show_points = TRUE, show_mode = TRUE,
      show_lookout = show_lookout, scatterplot = scatterplot, ...
    ) +
      ggplot2::guides(alpha = "none") +
      ggplot2::scale_y_continuous(breaks = NULL) +
      labs(y = "", x = names(data)[1])
  }
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
  if (ni == 0L) {
    # No roots
    if (falpha > min(den$eval.points)) {
      stop("Unable to find HDR")
    } else {
      intercept <- range(den$eval.points)
    }
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
utils::globalVariables(c("ends", "type", "lower", "upper", "group"))
