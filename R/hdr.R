#' @title Table of Highest Density Regions
#' @description
#' Compute the highest density regions (HDR) for either a specified density function, or
#' a kernel density estimate based on the data provided. Either 1 or 2 dimensional HDRs can be
#' calculated. The HDRs are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `lower` (the lower end of the interval),
#' `upper` (the upper end of the interval), and
#' `density` (the value of the density at the ends of the interval).
#' The first row of the tibble has `prob` set to 0 and specifies the mode of the distribution.
#'
#' @param x Numerical vector of data, or a 2-column matrix of data
#' @param y Numerical vector of data
#' @param density Density estimate
#' @param prob Probability of the HDR
#' @param bw Bandwidth for density estimate
#' @return A tibble with columns prob, lower and upper
#' @author Rob J Hyndman
#' @examples
#' hdr_table(c(rnorm(50), rnorm(50, 3, 1)), prob = 0.95)
#' hdr_table(c(rnorm(50), rnorm(50, 3, 1)), prob = 0.95)
#' @export
hdr_table <- function(x = NULL, y = NULL, density = NULL,
      prob = c(0.50, 0.99), bw = NULL, ...) {
  if(min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  # density supplied
  if (!is.null(density)) {
    if (!is.null(x) & !is.null(y)) {
      warning("Ignoring x and y")
    } else if (!is.null(x) & is.null(y)) {
      warning("Ignoring x")
    } else if (is.null(x) & !is.null(y)) {
      warning("Ignoring y")
    }
    hdr <- hdr(prob = prob, den = den)
  } else if (!is.null(x) & is.null(y)) {
    # only x supplied
    if(is.null(bw)) {
      bw <- stats::bw.nrd0(x)
    }
    hdr <- hdr(x = x, prob = prob, h = bw, ...)
  } else if (!is.null(x) & !is.null(y)) {
    # both x and y supplied
    hdr <- hdrcde::hdr.2d(x = x, y = y, prob = prob, ...)
  } else {
    stop("Must supply at least one of density, x and y")
  }
  hdr_df <- as_tibble(hdr$hdr, .name_repair = "unique", rownames = "prob") |>
    suppressMessages()
  nint <- (ncol(hdr_df) - 1) / 2
  colnames(hdr_df) <- c("prob", paste0(c("L", "U"), rep(seq(nint), each = nint)))
  hdr_df <- hdr_df |>
    mutate(density = hdr$falpha) |>
    pivot_longer(cols = matches("^[LU].*"), names_to = "names", values_to = "ends") |>
    mutate(
      type = if_else(grepl("L", names), "lower", "upper"),
      prob = readr::parse_number(prob)
    ) |>
    select(-names) |>
    filter(!is.na(ends)) |>
    bind_rows(
      tibble(
        prob = c(0, 0),
        type = c("lower", "upper"),
        ends = rep(hdr$mode, 2),
        density = rep(hdr$fmode, 2),
      )
    ) |>
    arrange(prob, ends) |>
    mutate(n = 1 + trunc((row_number() / 2.01))) |>
    pivot_wider(id_cols = c("n", "prob", "density"), names_from = type, values_from = ends)

  hdr_df |>
    select(prob, lower, upper, density)
}

# Remaining functions adapted from hdrcde package

hdr <- function(x = NULL, prob, den = NULL, h,
                nn = 5000, all.modes = FALSE) {
  if (!is.null(x)) {
    r <- diff(range(x))
    if (r == 0) {
      stop("Insufficient data")
    }
  }
  if (is.null(den)) {
    den <- density(x, bw = h)
  }
  alpha <- sort(1 - prob)
  falpha <- calc.falpha(x, den, alpha, nn = nn)
  hdr.store <- matrix(NA, length(alpha), 100)
  for (i in 1:length(alpha)){
    junk <- hdr.ends(den, falpha$falpha[i])$hdr
    if (length(junk) > 100) {
      junk <- junk[1:100]
      warning("Too many sub-intervals. Only the first 50 returned.")
    }
    hdr.store[i, ] <- c(junk, rep(NA, 100 - length(junk)))
  }
  cj <- colSums(is.na(hdr.store))
  hdr.store <- matrix(hdr.store[, cj < nrow(hdr.store)], nrow = length(prob))
  rownames(hdr.store) <- paste(100 * (1 - alpha), "%", sep = "")
  if (all.modes) {
    y <- c(0, den$y, 0)
    n <- length(y)
    idx <- ((y[2:(n - 1)] > y[1:(n - 2)]) & (y[2:(n - 1)] > y[3:n])) | (den$y == max(den$y))
    mode <- den$x[idx]
  } else {
    mode <- falpha$mode
  }
  return(list(hdr = hdr.store, mode = mode, falpha = falpha$falpha, fmode = falpha$fmode))
}

calc.falpha <- function(x = NULL, den, alpha, nn = 5000) {
  # Calculates falpha needed to compute HDR of density den.
  # Also finds approximate mode.
  # Input: den = density on grid.
  #          x = independent observations on den
  #      alpha = level of HDR

  if (is.null(x)) {
    calc.falpha(x = sample(den$x, nn, replace = TRUE, prob = den$y), den, alpha)
  } else {
    fx <- approx(den$x, den$y, xout = x, rule = 2)$y
    falpha <- quantile(fx, prob = alpha, type = 8)
    mode <- den$x[den$y == max(den$y)]
    return(list(falpha = falpha, mode = mode, fx = fx, fmode = max(den$y)))
  }
}

hdr.ends <- function(den, falpha) {
  miss <- is.na(den$x) # | is.na(den$y)
  den$x <- den$x[!miss]
  den$y <- den$y[!miss]
  n <- length(den$x)
  # falpha is above the density, so the HDR does not exist
  if (falpha > max(den$y)) {
    return(list(falpha = falpha, hdr = NA))
  }
  f <- function(x, den, falpha) {
    approx(den$x, den$y - falpha, xout = x)$y
  }
  intercept <- all_roots(f, interval = range(den$x), den = den, falpha = falpha)
  ni <- length(intercept)
  # No roots -- use the whole line
  if (ni == 0L) {
    intercept <- c(den$x[1], den$x[n])
  } else {
    # Check behaviour outside the smallest and largest intercepts
    if (f(0.5 * (head(intercept, 1) + den$x[1]), den, falpha) > 0) {
      intercept <- c(den$x[1], intercept)
    }
    if (f(0.5 * (tail(intercept, 1) + den$x[n]), den, falpha) > 0) {
      intercept <- c(intercept, den$x[n])
    }
  }
  # Check behaviour -- not sure if we need this now
  if (length(intercept) %% 2) {
    warning("Some HDRs are incomplete")
  }
  #  intercept <- sort(unique(intercept))
  return(list(falpha = falpha, hdr = intercept))
}


all_roots <- function(f, interval,
    lower = min(interval), upper = max(interval), n = 100L, ...) {
  x <- seq(lower, upper, len = n + 1L)
  fx <- f(x, ...)
  roots <- x[which(fx == 0)]
  fx2 <- fx[seq(n)] * fx[seq(2L, n + 1L, by = 1L)]
  index <- which(fx2 < 0)
  for (i in index) {
    roots <- c(roots, uniroot(f, lower = x[i], upper = x[i + 1L], ...)$root)
  }
  return(sort(roots))
}
