#' Convert distributional object to a data frame
#'
#' Make a long-form data frame containing densities from a distributional
#' object on a regular grid for plotting.
#'
#' @param object A distributional object
#' @return Data frame with columns `x`, `y` (if bivariate), `density`, and `distribution`.
#' @examples
#' dist_kde(oldfaithful$duration) |> density_df()
#'
#' @export
density_df <- function(object) {
  d <- dimension_dist(object)
  if (d > 2) {
    stop("Only univariate and bivariate densities are supported")
  }
  if (length(object) > 1) {
    purrr::map_dfr(object, density_df)
  } else if (d == 1) {
    make_density_df_1d(object)
  } else {
    make_density_df_2d(object)
  }
}

# ---- 1D ------------------------------------------------------------------

make_density_df_1d <- function(object, ngrid = 501L) {
  rand <- unlist(distributional::generate(object, times = 1))

  if (is.logical(rand)) {
    grid_x <- c(FALSE, TRUE)
  } else if (is.integer(rand)) {
    qq <- quantile(object, p = c(0.002, 0.998)) |>
      unlist() |>
      round() |>
      as.integer()
    grid_x <- seq(qq[1], qq[2], by = 1L)
  } else {
    qq <- unlist(quantile(object, p = c(0, 0.002, 0.998, 1)))
    range_x <- range(qq[is.finite(qq)])
    if ("kde" %in% stats::family(object)) {
      kde_x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
      range_x <- range(range_x, unlist(kde_x))
    }
    pad <- 0.0001 * diff(range_x)
    grid_x <- c(
      range_x[1] - pad,
      seq(range_x[1], range_x[2], length.out = ngrid - 2),
      range_x[2] + pad
    )
  }

  if (stats::family(object) == "mvnorm") {
    # 1d multivariate normal
    grid_x <- as.matrix(grid_x)
  }
  dens <- density(object, at = grid_x)
  dist_names <- names_dist(object, unique = TRUE)

  if (length(object) == 1L) {
    return(dplyr::as_tibble(data.frame(
      x = grid_x,
      distribution = dist_names,
      density = dens[[1]],
      stringsAsFactors = FALSE
    )))
  }

  out <- data.frame(
    x = rep(grid_x, times = length(object)),
    distribution = rep(dist_names, each = length(grid_x)),
    density = unlist(dens, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  dplyr::as_tibble(out)
}

# ---- 2D ------------------------------------------------------------------

make_density_df_2d <- function(object, ngrid = 101L, range_n = 1000L) {
  if (length(object) > 1) {
    stop("Currently only supporting one bivariate density")
  }
  fam <- stats::family(object)
  if ("kde" %in% fam) {
    kde <- vctrs::vec_data(object)[[1]]$kde
    grid <- expand.grid(
      x = kde$eval.points[[1]],
      y = kde$eval.points[[2]]
    )
    grid$density <- as.vector(kde$estimate)
  } else {
    range_xy <- bivariate_range(object, fam = fam, range_n = range_n)
    support <- range_xy[2, ] - range_xy[1, ]
    pad <- 0.0001 * support

    x <- c(
      range_xy[1, 1] - pad[1],
      seq(range_xy[1, 1], range_xy[2, 1], length.out = ngrid - 2),
      range_xy[2, 1] + pad[1]
    )
    y <- c(
      range_xy[1, 2] - pad[2],
      seq(range_xy[1, 2], range_xy[2, 2], length.out = ngrid - 2),
      range_xy[2, 2] + pad[2]
    )

    grid <- expand.grid(x = x, y = y)
    grid$density <- density(object, at = as.matrix(grid))[[1]]
  }

  grid$distribution <- names_dist(object, unique = TRUE)[1]
  dplyr::as_tibble(grid[, c("x", "y", "distribution", "density")])
}

# Bounding box for the plotting grid of a bivariate distribution.
#
# Uses analytic parameters when the distribution belongs to a family we know
# how to bound directly (currently: multivariate normal). Otherwise falls back
# to a sample-based bound.
#
# Returns a 2x2 matrix:
#   row 1 (mins): lower x, lower y
#   row 2 (maxs): upper x, upper y
bivariate_range <- function(
  object,
  fam = stats::family(object),
  range_n = 1000L
) {
  # ---- Analytic fast paths --------------------------------------------
  # `mvnorm` is the family string returned by distributional for objects
  # constructed via dist_multivariate_normal() (its class is `dist_mvnorm`,
  # and the package's default family() method strips the `dist_` prefix).
  if ("mvnorm" %in% fam) {
    fast <- tryCatch(
      mvnorm_range(object),
      error = function(e) NULL
    )
    if (!is.null(fast)) return(fast)
    # If extraction failed for any reason, fall through to sampling.
  }

  # ---- Generic fallback: empirical range from a small sample ----------
  rand <- distributional::generate(object, times = range_n)[[1]]
  apply(rand, 2, range, na.rm = TRUE)
}

# Analytic bounding box for a (bivariate) multivariate normal. Returns NULL if
# the parameter extraction does not produce the expected shapes; the caller
# treats NULL as "please fall back to sampling".
#
# Bounds are mu +/- 4 * sd on each marginal. 4 sigma covers ~99.994% of
# marginal mass, which is comfortably outside the widest HDR coverage we
# typically plot (default max prob = 0.9 corresponds to roughly 2.15 sigma in
# Mahalanobis distance, or up to ~2.15 marginal sigma when a contour grazes
# an axis). Slightly more generous than strictly necessary, matching what the
# previous 1e5-sample empirical bound delivered.
mvnorm_range <- function(object) {
  d_data <- vctrs::vec_data(object)[[1]]
  mu <- d_data$mu
  sigma <- d_data$sigma

  if (!is.numeric(mu) || length(mu) != 2L) {
    return(NULL)
  }
  if (!is.matrix(sigma) || any(dim(sigma) != 2L)) {
    return(NULL)
  }

  sds <- sqrt(diag(sigma))
  if (!all(is.finite(sds)) || any(sds <= 0)) {
    return(NULL)
  }

  rbind(
    mins = mu - 4 * sds,
    maxs = mu + 4 * sds
  )
}

# ---- Distribution introspection helpers ----------------------------------

dimension_dist <- function(object) {
  length(unlist(distributional::generate(object[1], times = 1)))
}

names_dist <- function(object, unique = FALSE) {
  dist_names <- format(object)
  object_names <- names(object)
  idx <- which(object_names != "")
  dist_names[idx] <- object_names[idx]
  if (unique) {
    make.unique(dist_names)
  } else {
    dist_names
  }
}
