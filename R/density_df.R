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
  info <- lapply(object, dist_info)
  dims <- vapply(info, \(x) x[["dim"]], integer(1L))
  if (any(dims > 2)) {
    stop("Only univariate and bivariate densities are supported")
  }
  d <- dims[1]
  dist_names <- names_dist(object, unique = TRUE)
  if (length(object) > 1) {
    # Multiple distributions
    if (all(dims == d) && d == 1) {
      # We have only 1d distributions
      types <- vapply(info, \(x) x[["type"]], character(1L))
      if (all(types == types[1])) {
        # All distributions of the same type (logical, integer, real)
        # Use a common grid
        grid_x <- find_grid_1d(object, type = types[1])
        out <- lapply(object, \(o) make_density_df_1d(o, grid_x = grid_x))
      } else {
        out <- lapply(object, density_df)
      }
    } else if (all(dims == d) && d == 2) {
      # We have only 2d distributions
      # Use a common grid (as no integer or logical bivariate distributions defined)
      grid <- find_grid_2d(object)
      out <- lapply(object, \(o) make_density_df_2d(o, grid = grid))
    } else {
      out <- lapply(object, density_df)
    }
    out <- mapply(
      function(x, name) {
        x$distribution <- name
        x
      },
      out,
      as.list(dist_names),
      SIMPLIFY = FALSE
    )
    dplyr::bind_rows(out)
  } else if (d == 1) {
    make_density_df_1d(object)
  } else {
    make_density_df_2d(object)
  }
}

# ---- 1D ------------------------------------------------------------------

find_grid_1d <- function(object, ngrid = 501L, type = NULL) {
  if (is.null(type)) {
    type <- dist_info(object[1])[["type"]]
  }
  # Logical variables
  if (type == "logical") {
    return(c(FALSE, TRUE))
  }
  # Integer variables
  if (type == "integer") {
    ranges <- lapply(object, function(o) {
      quantile(o, p = c(0.002, 0.998)) |>
        unlist() |>
        round() |>
        as.integer()
    })
    all_range <- range(unlist(ranges))
    return(seq(all_range[1], all_range[2], by = 1L))
  }
  # Real variables
  ranges <- lapply(object, function(o) {
    qq <- unlist(quantile(o, p = c(0, 0.002, 0.998, 1)))
    range(qq[is.finite(qq)])
  })
  range_x <- range(unlist(ranges))
  # If KDE, make sure range covers the sample used to estimate it
  kde_x <- unlist(lapply(object, function(o) {
    if ("kde" %in% stats::family(o)) {
      unlist(lapply(vctrs::vec_data(o), function(u) u$kde$x))
    }
  }))
  if (length(kde_x) > 0) {
    range_x <- range(range_x, kde_x)
  }
  # Pad result to ensure boundaries are properly captured
  pad <- 0.0001 * diff(range_x)
  c(
    range_x[1] - pad,
    seq(range_x[1], range_x[2], length.out = ngrid - 2),
    range_x[2] + pad
  )
}

make_density_df_1d <- function(object, grid_x = NULL, ngrid = 501L) {
  stopifnot(length(object) == 1)
  if (is.null(grid_x)) {
    grid_x <- find_grid_1d(object, ngrid = ngrid)
  }
  if (stats::family(object) == "mvnorm") {
    # 1d multivariate normal
    grid_x <- as.matrix(grid_x)
  }
  dens <- density(object, at = grid_x)
  dplyr::as_tibble(data.frame(
    x = grid_x,
    distribution = names_dist(object),
    density = dens[[1]]
  ))
}

# ---- 2D ------------------------------------------------------------------

find_grid_2d <- function(object, ngrid = 101L, range_n = 1000L) {
  ranges <- lapply(object, function(o) {
    bivariate_range(o, fam = stats::family(o), range_n = range_n)
  })
  if (length(ranges) == 1L) {
    range_xy <- ranges[[1]]
  } else {
    mins <- do.call(rbind, lapply(ranges, function(r) r[1, ]))
    maxs <- do.call(rbind, lapply(ranges, function(r) r[2, ]))
    range_xy <- rbind(
      mins = apply(mins, 2, min),
      maxs = apply(maxs, 2, max)
    )
  }
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
  expand.grid(x = x, y = y)
}

make_density_df_2d <- function(
  object,
  grid = NULL,
  ngrid = 101L,
  range_n = 1000L
) {
  stopifnot(length(object) == 1)
  fam <- stats::family(object)
  if (is.null(grid)) {
    if ("kde" %in% fam) {
      kde <- vctrs::vec_data(object)[[1]]$kde
      grid <- expand.grid(
        x = kde$eval.points[[1]],
        y = kde$eval.points[[2]]
      )
      grid$density <- as.vector(kde$estimate)
      grid$distribution <- names_dist(object)
      return(dplyr::as_tibble(grid[, c("x", "y", "distribution", "density")]))
    }
    grid <- find_grid_2d(object, ngrid = ngrid, range_n = range_n)
  }
  grid$density <- density(object, at = as.matrix(grid))[[1]]
  grid$distribution <- names_dist(object)
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
  # constructed via dist_multivariate_normal()
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
# marginal mass, well outside the widest HDR coverage typically plotted.
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
  rbind(mins = mu - 4 * sds, maxs = mu + 4 * sds)
}

# ---- Distribution introspection helpers ----------------------------------
# dist_info returns dimension and type for a single distributional object
dist_info <- function(object) {
  rand <- unlist(distributional::generate(object[1], times = 1))
  list(
    dim = length(rand),
    type = if (is.logical(rand)) {
      "logical"
    } else if (is.integer(rand)) {
      "integer"
    } else {
      "continuous"
    }
  )
}

# dimension_dist returns vector of dimensions from a vector of distributions
# If same, collapses to the unique value
dimension_dist <- function(object, same = TRUE) {
  d <- unlist(lapply(object, function(x) dist_info(x)$dim))
  if (same) {
    if (any(d != d[1])) {
      stop("All distributions must have the same dimension")
    }
    d <- d[1]
  }
  d
}

# names_dist returns a vector of names from a vector of distributions,
# optionally forced to be unique
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
