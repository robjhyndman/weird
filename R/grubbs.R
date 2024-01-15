#' Statistical tests for anomalies using Grubbs' test and Dixon's test
#'
#' Grubbs' test (proposed in 1950) identifies possible anomalies in univariate
#' data using z-scores assuming the data come from a normal distribution.
#' Dixon's test (also from 1950) compares the difference in the largest two values
#' to the range of the data. Critical values for Dixon's test have been
#' computed using simulation with interpolation using a quadratic model on
#' logit(alpha) and log(log(n)).
#'
#' @details Grubbs' test is based on z-scores, and a point is identified as an
#' anomaly when the associated absolute z-score is greater than a threshold value.
#' A vector of logical values is returned, where \code{TRUE} indicates an anomaly.
#' This version of Grubbs' test looks for outliers anywhere in the sample.
#' Grubbs' original test came in several variations which looked for one outlier,
#' or two outliers in one tail, or two outliers on opposite tails. These variations
#' are implemented in the \code{\link[outliers]{grubbs.test}} function.
#' Dixon's test only considers the maximum (and possibly the minimum) as potential outliers.
#' @references Grubbs, F. E. (1950). Sample criteria for testing outlying observations.
#' *Annals of Mathematical Statistics*, 21(1), 27–58.
#' Dixon, W. J. (1950). Analysis of extreme values.
#' *Annals of Mathematical Statistics*, 21(4), 488–506.
#' @return A logical vector
#' @author Rob J Hyndman
#' @param y numerical vector of observations
#' @param alpha size of the test.
#' @seealso \code{\link[outliers]{grubbs.test}}, \code{\link[outliers]{dixon.test}}
#' @examples
#' x <- c(rnorm(1000), 5:10)
#' tibble(x = x) |> filter(grubbs_anomalies(x))
#' tibble(x = x) |> filter(dixon_anomalies(x))
#' y <- c(rnorm(1000), 5)
#' tibble(y = y) |> filter(grubbs_anomalies(y))
#' tibble(y = y) |> filter(dixon_anomalies(y))
#' @export

grubbs_anomalies <- function(y, alpha = 0.05) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  n <- length(y)
  t2 <- stats::qt(1 - alpha / (2 * n), n - 2)
  threshold <- (n - 1) / sqrt(n) * sqrt(t2^2 / (n - 2 + t2^2))
  return(abs(z) > threshold)
}

# grubbs_test <- function(y, ...) {
#   z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
#   n <- length(y)
#   maxz <- max(abs(z))
#   alpha <- c(10^(-(7:2)), seq(0.02,0.50, by=0.01))
#   t2 <- qt(1 - alpha / (2 * n), n - 2)
#   threshold <- (n - 1) / sqrt(n) * sqrt(t2^2 / (n - 2 + t2^2))
#   p <-
#
#   pval <- pt(maxz, n-2)
#   output <- list(statistic = c(maxz = max(abs(z))), alternative = alt, p.value = pval,
#                  method = "Dixon test for outliers", data.name = DNAME)
#   class(RVAL) <- "htest"
#   return(RVAL)
#
# }

#' @rdname grubbs_anomalies
#' @param two_sided If \code{TRUE}, both minimum and maximums will be considered. Otherwise
#' only the maximum will be used. (Take negative values to consider only the minimum with
#' \code{two_sided=FALSE}.)
#' @export

dixon_anomalies <- function(y, alpha = 0.05, two_sided = TRUE) {
  if (two_sided) {
    miny <- which.min(y)
  }
  maxy <- which.max(y)
  sorty <- sort(y)
  n <- length(y)
  if (two_sided) {
    Q <- max(sorty[2] - sorty[1], sorty[n] - sorty[n-1]) / (sorty[n] - sorty[1])
  } else {
    Q <- (sorty[n] - sorty[n-1]) / (sorty[n] - sorty[1])
    alpha <- 2 * alpha
  }

  # Find critical value using linear model fitted to simulated critical values
  # Subset data to nearest alpha and n values
  logit <- function(u) { log(u/(1-u)) }
  loglog <- function(u) { log(log(u)) }
  # Find four nearest alpha values
  alpha_grid <- sort(unique(dixon_cv$alpha))
  nearest_alpha <- (alpha_grid[order(abs(logit(alpha_grid) - logit(alpha)))])[1:4]
  # Fit model using only alpha values?
  alpha_only_model <- (n %in% 3:50)
  # Find nearest n values
  if (alpha_only_model) {
    nearest_n <- n
  } else {
    # Find four nearest n values
    n_grid <- sort(unique(dixon_cv$n))
    nearest_n <- (n_grid[order(abs(loglog(n_grid) - loglog(n)))])[1:4]
  }
  cv_subset <- dixon_cv[dixon_cv$alpha %in% nearest_alpha & dixon_cv$n %in% nearest_n, ]
  cv_subset$loglogn <- loglog(cv_subset$n)
  cv_subset$logitalpha <- logit(cv_subset$alpha)
  if (alpha_only_model) {
    # Cubic interpolation to 4 points. 4 df
    dixonfit <- stats::lm(log(cv) ~ poly(logitalpha, 3), data = cv_subset)
  } else {
    # Quadratic bivariate model to 16 points. 6 df
    dixonfit <- stats::lm(log(cv) ~ poly(loglogn, 2) + poly(logitalpha, 2) + I(logitalpha * loglogn),
      data = cv_subset
    )
  }
  threshold <- exp(stats::predict(dixonfit,
    newdata = data.frame(logitalpha = logit(alpha), loglogn = loglog(n))
  ))
  # Return logical vector showing where outliers are
  output <- rep(FALSE, n)
  if (Q > threshold) {
    if (two_sided) {
      output[miny] <- (sorty[2] - sorty[1]) / (sorty[n] - sorty[1]) > threshold
    }
    output[maxy] <- (sorty[n] - sorty[n - 1]) / (sorty[n] - sorty[1]) > threshold
  }
  return(output)
}

#' @importFrom stats lm predict qt
#' @importFrom tibble tibble
