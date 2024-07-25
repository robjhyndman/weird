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
#' @param ... Additional arguments are passed to \code{\link[ggplot2]{geom_line}}.
#' @return A ggplot layer
#' @author Rob J Hyndman
#' @examples
#' # Univariate density
#' dist_kde(c(rnorm(500), rnorm(500, 4, 1.5))) |>
#'   autoplot(show_hdr = TRUE, prob = c(0.5, 0.95), color = "#c14b14") +
#'   autolayer(distributional::dist_normal(), linetype = "dashed")
#' @exportS3Method ggplot2::autolayer

autolayer.distribution <- function(object, ngrid = 501, ...) {
  df <- make_density_df(object, ngrid)
  if(length(object) == 1L) {
    geom_line(data = df, aes(x = y, y = Density), ...)
  } else {
    geom_line(data = df, aes(x = y, y = Density, color = Distribution), ...)
  }
}

# Make data frame containing densities from distributional object
make_density_df <- function(object, ngrid) {
  # Find range of x values to use
  range_x <- range(unlist(quantile(object, p = c(0.002, 0.998))))
  # Expand to edge of range if support is finite
  if(is.finite(minx <- min(quantile(object, p=0)))) {
    range_x <- range(minx, range_x)
  }
  if(is.finite(maxx <- max(quantile(object, p=1)))) {
    range_x <- range(range_x, maxx)
  }
  # Expand to include all data points if a kde
  if("kde" %in% stats::family(object)) {
    x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
    range_x <- range(range_x, unlist(x))
  }
  support <- diff(range_x)
  y <- c(
    min(range_x) - 0.0001*support,
    seq(min(range_x), max(range_x), length = ngrid-2),
    max(range_x) + 0.0001*support
  )
  df <- c(list(y), density(object, at = y))
  names(df) <- c("y", make.unique(format(object)))
  tibble::as_tibble(df) |>
    tidyr::pivot_longer(
      cols = -y, names_to = "Distribution",
      values_to = "Density"
    )
}

