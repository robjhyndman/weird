#' @title Table of Highest Density Regions
#' @description
#' Compute the highest density regions (HDR) for a kernel density estimate. The HDRs
#' are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `density` (the value of the density at the boundary of the HDR),
#' `mode` (the local mode equal to the highest density value within the corresponding HDR).
#' For one dimensional density functions, the tibble also has columns
#' `lower` (the lower ends of the intervals), and
#' `upper` (the upper ends of the interval).
#'
#' @param y Numerical vector or matrix of data
#' @param density Probablity density function, either estimated by `ks::kde()` or
#' a list with components `x` and `y` defining the density function.
#' @param prob Probability of the HDR
#' @param ... If `y` is supplied, other arguments are passed to \code{\link[ks]{kde}}.
#' Otherwise, additional arguments are ignored.
#' @return A tibble
#' @author Rob J Hyndman
#' @examples
#' hdr_table(c(rnorm(100), rnorm(100, 3, 1)))
#' x <- seq(-4,4, l=501)
#' hdr_table(density = list(x=x, y=dnorm(x)), prob = 0.95)
#' @export
hdr_table <- function(y = NULL, density = NULL,
      prob = c(0.50, 0.99), ...) {
  if(min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  # Data supplied
  if(!is.null(y)) {
    if(!is.null(density)) {
      warning("Ignoring density")
    }
    hdr <- hdr(y = y, prob = prob, ...)
  } else {
    # density supplied
    hdr <- hdr(prob = prob, den = density, ...)
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
    arrange(prob, ends) |>
    mutate(n = 1 + trunc((row_number() / 2.01))) |>
    pivot_wider(id_cols = c("n", "prob", "density"), names_from = type, values_from = ends)
  hdr_df <- hdr_df

  hdr_df |>
    select(prob, lower, upper, density)
}

# Compute HDRs

hdr <- function(y = NULL, prob, den = NULL, ...) {
  if (!is.null(y)) {
    r <- diff(range(y))
    if (r == 0) {
      stop("Insufficient data")
    }
    den <- ks::kde(y, ...)
  } else if(!inherits(den, "kde")) {
    # Convert den to class kde
    den2 <- den
    x <- sample(den2$x, size=2e5, replace = TRUE, prob = den2$y)
    den <- ks::kde(x)
    den$eval.points = den2$x
    den$estimate = den2$y
  }
  alpha <- sort(1 - prob)
  falpha <- approx(seq(99), den$cont, xout = 100*(1-alpha))$y
  hdr.store <- tibble(
    prob = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    mode = numeric(0)
  )
  for (i in 1:length(alpha)){
    junk <- hdr.ends(den, falpha[i])$hdr
    n <- length(junk)/2
    for(j in seq(n)) {
      within <- den$eval.points >= junk[2*j-1] & den$eval.points <= junk[2*j]
      subden <- cbind(x = den$eval.points[within], y=den$estimate[within])
      hdr.store <- bind_rows(hdr.store,
        tibble(prob=alpha[i], lower=junk[2*j-1], upper=junk[2*j],
                             mode = subden$x[which.max(subden$y)])
      )
    }
  }
  return(list(hdr = hdr.store, falpha = falpha))
}

# Remaining functions adapted from hdrcde package

hdr.ends <- function(den, falpha) {
  miss <- is.na(den$eval.points) # | is.na(den$estimate)
  den$eval.points <- den$eval.points[!miss]
  den$estimate <- den$estimate[!miss]
  n <- length(den$eval.points)
  # falpha is above the density, so the HDR does not exist
  if (falpha > max(den$estimate)) {
    return(list(falpha = falpha, hdr = NA))
  }
  f <- function(x, den, falpha) {
    approx(den$eval.points, den$estimate - falpha, xout = x)$y
  }
  intercept <- all_roots(f, interval = range(den$eval.points), den = den, falpha = falpha)
  ni <- length(intercept)
  # No roots -- use the whole line
  if (ni == 0L) {
    intercept <- c(den$eval.points[1], den$eval.points[n])
  } else {
    # Check behaviour outside the smallest and largest intercepts
    if (f(0.5 * (head(intercept, 1) + den$eval.points[1]), den, falpha) > 0) {
      intercept <- c(den$eval.points[1], intercept)
    }
    if (f(0.5 * (tail(intercept, 1) + den$eval.points[n]), den, falpha) > 0) {
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
