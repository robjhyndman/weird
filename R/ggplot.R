#' Produce ggplot of densities in 1 or 2 dimensions
#' @param object Probability density function as estimated by `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param fill If `TRUE`, and the density is bivariate, the bivariate contours
#' are shown as filled regions rather than lines.
#' @param show_hdr If `TRUE`, and the density is univariate, then the HDR regions
#' specified by `prob` are shown as a ribbon below the density.
#' @param show_points Numerical scalar giving probability threshold for points
#' to be shown on the density plot. e.g., if `show_points` is 0.99,
#' then the points outside the 99% highest density region are shown.
#' No points are shown if set to `NULL`.
#' @param color Color to use for HDR contours (if `fill` is `FALSE`), or the central color in HDR regions
#' @param palette Color palette function to use for HDR filled regions (if `fill` is `TRUE`).
#' @param ... Additional arguments are currently ignored.
#' @examples
#' # Univariate density
#' c(rnorm(500), rnorm(500, 4, 1.5)) |>
#'   kde() |>
#'   autoplot(show_hdr = TRUE, prob= c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' ymat |>
#'   kde(H = Hns(ymat)) |>
#'   autoplot(fill = TRUE, prob = c(0.5, 0.95), show_points = 0.95)
#' @export

autoplot.kde <- function(object, prob = seq(9)/10, fill = FALSE,
    show_hdr = FALSE, show_points = NULL, color = "#0072B2",
    palette = hdr_palette, ...) {
  if (min(prob) <= 0 | max(prob) >= 1) {
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
    )
    density$density <- c(object$estimate)
  }

  if(d == 1L) {
    # Plot univariate density
    p <- density |>
      ggplot() +
      geom_line(aes(x=y, y=density)) +
      labs(x = object$names[1])
    maxden <- max(density$density)
    if(!is.null(show_points)) {
      fi <- exp(-kde_scores(object$x, h = object$h))
      threshold <- quantile(fi, prob = 1 - show_points)
      show_x <- tibble::tibble(x = object$x[fi < threshold])
      p <- p + ggplot2::geom_point(
        data = show_x,
        mapping = aes(x = x, y = -maxden/40)
      )
    }
    if(show_hdr) {
      prob <- unique(hdr$prob)
      nhdr <- length(prob)
      p <- p +
        ggplot2::geom_rect(data = hdr,
          aes(xmin = lower, xmax=upper, ymin = -maxden/20, ymax=0,
              fill = factor(prob))) +
        scale_fill_manual(
          breaks = rev(prob),
          values = rev(palette(nhdr, color)),
          labels = paste0(100*rev(prob), "%")
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
    }
  } else {
    # Plot the contours
    p <- density |>
      ggplot() +
      labs(x = object$names[1], y = object$names[2])
    if(!is.null(show_points)) {
      ks <- kde_scores(object$x, H = object$H)
      threshold <- quantile(ks, 1 - show_points)
      show_x <- as.data.frame(x = object$x[ks > threshold,])
      colnames(show_x)[1:2] <- c("x","y")
      p <- p + ggplot2::geom_point(
        data = show_x,
        mapping = aes(x = x, y = y)
      )
    }
    if(fill) {
      p <- p +
        geom_contour_filled(aes(x = y1, y = y2, z = density),
                            breaks = rev(c(hdr$density, 100))) +
        scale_fill_manual(
          values = rev(palette(length(hdr$prob), color)),
          labels = rev(paste0(100 * hdr$prob, "%"))
        )
    } else {
      p <- p + geom_contour(aes(x = y1, y = y2, z = density),
                            breaks = hdr$density, color = color)
    }
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
  }
  return(p)
}

#' Color palette designed for plotting Highest Density Regions
#'
#' A sequential color palette is returned, with the first color being `color`, and the rest of the colors
#' being a mix of `color` with increasing amounts of white.
#'
#' @param n Number of colors to generate
#' @param color First color of vector.
#' @return A function that takes a number `n` and returns a vector of `n` colors
#' @export
hdr_palette <- function(n, color = "#0072B2") {
  rev(grDevices::colorRampPalette(c(color, "white"))(n+2)[seq(n)])
}

utils::globalVariables(c("x","y","y1","y2"))
