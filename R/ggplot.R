#' Produce ggplot of densities from kde() in 1 or 2 dimensions
#' @param object Probability density function as estimated by `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param filled If `TRUE`, the bivariate contours are shown as shaded regions rather than lines.
#' @param show_hdr If `TRUE`, the HDR regions specified by `prob` are shown as a ribbon below the density.
#' @param palette Color palette to use for HDR regions.
#' @param ... Additional arguments are ignored.
#' @examples
#' # Univariate density
#' c(rnorm(100), rnorm(100, 3, 1)) |>
#'   ks::kde() |>
#'   autoplot(show_hdr = TRUE, prob= c(0.5, 0.99)) +
#'   scale_fill_brewer(palette = "OrRd", direction = -1)
#' tibble(y1 = rnorm(100), y2 = y1 + rnorm(100)) |>
#'   ks::kde(H = 0.4*diag(2)) |>
#'   autoplot(filled = TRUE)
#' @export

autoplot.kde <- function(object, prob = seq(9)/10, filled = FALSE,
    show_hdr = FALSE, palette = viridisLite::viridis, ...) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob must be between 0 and 1")
  }
  if(inherits(object$eval.points, "list")) {
    d <- length(object$eval.points)
  } else {
    d <- 1L
  }
  if(d > 2) {
    stop("Only univariate and bivariate densities are supported")
  }
  if(d == 1L) {
    density <- tibble(
      y = object$eval.points,
      density = object$estimate
    )
    if(show_hdr) {
      hdr <- hdr_table(density = object, prob = prob)
    }
  } else {
    hdr <- hdr_table(density = object, prob = prob)
    density <- expand.grid(
      y1 = object$eval.points[[1]],
      y2 = object$eval.points[[2]]
    ) |>
      mutate(density = c(object$estimate))
  }

  if(d == 1L) {
    # Plot univariate density
    p <- density |>
      ggplot() +
      geom_line(aes(x=y, y=density)) +
      labs(x = object$names[1])
    if(show_hdr) {
      maxden <- max(density$density)
      nint <- length(unique(hdr$prob))
      p <- p +
        geom_rect(data = hdr,
          aes(xmin = lower, xmax=upper, ymin = -maxden/20, ymax=0,
              fill = factor(prob))) +
        scale_fill_manual(
          breaks = hdr$prob,
          values = rev(palette(nint)),
          labels = paste0(hdr$prob, "%")
        ) +
        guides(fill = "none")
    }
  } else {
    # Plot the contours
    p <- density |>
      ggplot(aes(x = y1, y = y2, z = density)) +
      labs(x = object$names[1], y = object$names[2])
    if(filled) {
      p <- p +
        geom_contour_filled(breaks = rev(c(hdr$density, 100))) +
        scale_fill_manual(
          values = rev(palette(length(hdr$prob))),
          labels = rev(paste0(100 * hdr$prob, "%"))
        )
    } else {
      p <- p + geom_contour(breaks = hdr$density)
    }
    p <- p + guides(fill = guide_legend(title = "HDR coverage"))
  }
  return(p)
}

calc_density <- function(y, h, H, ...) {
  y <- as.matrix(y)
  d <- NCOL(y)
  r <- apply(apply(y, 2, range), 2, diff)
  if (any(r == 0)) {
    stop("Insufficient data")
  }
  if (missing(h) & NCOL(y) == 1) {
    h <- ks::hns(y[, 1])
  } else if (missing(H)) {
    H <- ks::Hns(y)
  }
  ks::kde(y, h = h, H = H, binned = length(y) > 1000, ...)
}

density_on_grid <- function(y, fy, ngrid) {
  y <- as.matrix(y)
  if(NCOL(y) != 2L)
    stop("y must be a matrix with 2 columns")
  # Create grid of points
  density <- list(eval.points = list(
    seq(min(y[,1]), max(y[,1]), length=ngrid),
    seq(min(y[,2]), max(y[,2]), length=ngrid)
  ))
  # Bivariate interpolation
  grid <- expand.grid(density$eval.points[[1]], density$eval.points[[2]])
  density$estimate <- interp::interpp(x = y[,1], y = y[,2], z = fy,
                                      xo =grid[,1], yo = grid[,2])$z |>
    suppressWarnings() |>
    matrix(nrow=50)
  return(density)
}
