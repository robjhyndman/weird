#' Add ggplot layer of densities from distributional objects in 1 dimension
#'
#' @details
#' This function adds a ggplot layer of a density from a distributional object.
#' For univariate densities, it adds a line plot of the density function.
#' For bivariate densities, it adds a contour plot of the density function.
#'
#' @param object distribution object from the distributional package or
#' \code{\link{dist_kde}}()
#' @param ngrid Number of points at which to evaluate the density function.
#' @param scale Scaling factor for the density function.
#' @param ... Additional arguments are passed to \code{\link[ggplot2]{geom_line}}.
#' @return A ggplot layer
#' @author Rob J Hyndman
#' @examples
#' library(distributional)
#' dist_mixture(
#'   dist_normal(-2, 1),
#'   dist_normal(2, 1),
#'   weights = c(1 / 3, 2 / 3)
#' ) |>
#'   gg_density() +
#'   gg_density_layer(dist_normal(-2, 1), linetype = "dashed", scale = 1 / 3) +
#'   gg_density_layer(dist_normal(2, 1), linetype = "dashed", scale = 2 / 3)
#' @export

gg_density_layer <- function(object, ngrid = 501, scale = 1, ...) {
  df <- make_density_df(object, ngrid)
  if (length(object) == 1L) {
    geom_line(data = df, aes(x = x, y = scale * Density), ...)
  } else {
    geom_line(data = df, aes(x = x, y = scale * Density, color = Distribution), ...)
  }
}

# Make data frame containing densities from distributional object
make_density_df <- function(object, ngrid = 501) {
  # Find dimension of distribution
  d <- dimension_dist(object)
  if(d > 2) {
    stop("Only univariate and bivariate densities are supported")
  }
  if(d == 1) {
    # Find range of x values to use
    qq <- quantile(object, p = c(0, 0.002, 0.998, 1)) |> unlist()
    range_x <- range(qq[is.finite(qq)])
    # Expand to include all data points if a kde
    if ("kde" %in% stats::family(object)) {
      x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
      range_x <- range(range_x, unlist(x))
    }
    # Create grid
    support <- diff(range_x)
    y <- c(
      min(range_x) - 0.0001 * support,
      seq(min(range_x), max(range_x), length = ngrid - 2),
      max(range_x) + 0.0001 * support
    )
    # Density on grid
    df <- c(x = list(y), density(object, at = y))
  } else {
    if(length(object) > 1) {
      stop("Currently only supporting one bivariate density")
    }
    # For kde, use saved grid
    if("kde" %in% stats::family(object)) {
      kde <- vctrs::vec_data(object)[[1]]$kde
      df <- expand.grid(x = kde$eval.points[[1]], y = kde$eval.points[[2]])
      df$Density <- as.vector(kde$estimate)
    } else {
      # Find range of x values to use
      rand <- distributional::generate(object, times = 1e6)
      support <- lapply(rand, function(u) { apply(u, 2, range, na.rm=TRUE) })
      range_xy <- apply(do.call(rbind, support), 2, range)
      # Expand to include all data points if a kde
      if ("kde" %in% stats::family(object)) {
        x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
        support <- lapply(x, function(u) { apply(u, 2, range, na.rm=TRUE) })
        range_xy <- apply(do.call(rbind, c(list(range_xy), support)), 2, range)
      }
      support <- apply(range_xy, 2, diff)
      #ngrid <- round(sqrt(ngrid))
      x <- c(
        min(range_xy[,1]) - 0.0001 * support[1],
        seq(min(range_xy[,1]), max(range_xy[,1]), length = ngrid - 2),
        max(range_xy[,1]) + 0.0001 * support
      )
      y <- c(
        min(range_xy[,2]) - 0.0001 * support[2],
        seq(min(range_xy[,2]), max(range_xy[,2]), length = ngrid - 2),
        max(range_xy[,2]) + 0.0001 * support
      )
      df <- as.data.frame(expand.grid(x=x,y=y))
      if("mvnorm" %in% stats::family(object)) {
        # Use faster mvtnorm package
        params <- vctrs::vec_data(object)[[1]]
        mu <- vctrs::field(params, "mu")
        sigma <- vctrs::field(params, "sigma")
        df$Density <- mvtnorm::dmvnorm(df, mean = mu, sigma = sigma)
      } else {
        # Use slower distributional package
        df$Density <- density(object, at = as.matrix(df))
      }
    }
  }
  names(df)[-seq(d)] <- names_dist(object, unique = TRUE)
  tibble::as_tibble(df) |>
    tidyr::pivot_longer(
      cols = -seq(d), names_to = "Distribution",
      values_to = "Density"
    ) |>
    dplyr::distinct()
}

# Find dimension of distribution
dimension_dist <- function(object) {
  dist_names <- format(object)
  if(any(grepl("MVN", dist_names))) {
    mvn <- dist_names[grep("MVN", dist_names)[1]]
    d <- as.numeric(sub("MVN\\[(\\d+)\\]", "\\1", mvn))
  } else if("kde" %in% stats::family(object)) {
    kde <- which(grepl("kde", dist_names))
    d <- lapply(vctrs::vec_data(object[kde]), function(u) NCOL(u$kde$x)) |> unlist()
  } else {
    d <- 1
  }
  d[1]
}

# Get names of distributions
names_dist <- function(object, unique = FALSE) {
  dist_names <- format(object)
  object_names <- names(object)
  idx <- which(object_names != "")
  dist_names[idx] <- object_names[idx]
  if(unique) {
    make.unique(dist_names)
  } else {
    dist_names
  }
}
