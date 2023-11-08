#' Produce ggplot of densities in 1 or 2 dimensions
#' @param object Probability density function as estimated by `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param fill If `TRUE`, and the density is bivariate, the bivariate contours
#' are shown as filled regions rather than lines.
#' @param show_hdr If `TRUE`, and the density is univariate, then the HDR regions
#' specified by `prob` are shown as a ribbon below the density.
#' @param show_points If `TRUE`, then individual points are plotted.
#' @param show_mode If `TRUE`, then the mode of the distribution is shown.
#' @param color Color to use for HDR contours (if `fill` is `FALSE`), or the central color in HDR regions
#' @param palette Color palette function to use for HDR filled regions (if `fill` is `TRUE`).
#' @param alpha Transparency of points. Defaults to min(1, 1000/n), where n is the number of observations.
#' @param ... Additional arguments are currently ignored.
#' @examples
#' # Univariate density
#' c(rnorm(500), rnorm(500, 4, 1.5)) |>
#'   kde() |>
#'   autoplot(show_hdr = TRUE, prob= c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' ymat |>
#'   kde(H = Hns(ymat)) |>
#'   autoplot(fill = TRUE, prob = c(0.5, 0.95), show_points = TRUE)
#' @export

autoplot.kde <- function(object, prob = seq(9)/10, fill = FALSE,
    show_hdr = FALSE, show_points = FALSE, show_mode = FALSE, color = "#00659e",
    palette = hdr_palette, alpha = min(1, 1000/NROW(object$x)), ...) {
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
  if(show_points) {
    if(is.null(object$x)) {
      warning("No observations found")
    }
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
    if(show_points) {
      # Only show points outside largest HDR
      if(show_hdr) {
        fi <- exp(-kde_scores(object$x, h = object$h))
        threshold <- quantile(fi, prob = 1 - max(hdr$prob))
        show_x <- tibble::tibble(x = object$x[fi < threshold])
      } else {
        show_x <- tibble::tibble(x = object$x)
      }
      p <- p + ggplot2::geom_point(
        data = show_x,
        mapping = aes(x = x, y = -maxden/40),
        alpha = alpha
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
    if(show_mode) {
      p <- p +
        ggplot2::geom_line(
          data = expand.grid(mode = unique(hdr$mode), ends = c(0, -maxden/20)),
          mapping = aes(x = mode, y = ends, group = mode),
          color = grDevices::colorRampPalette(c(color, "black"))(3)[2],
          size = 1
        )
    }
  } else {
    # Plot the contours
    p <- density |>
      ggplot() +
      labs(x = object$names[1], y = object$names[2])
    if(show_points) {
      # If fill, only show points outside largest HDR
      if(fill) {
        ks <- kde_scores(object$x, H = object$H)
        threshold <- quantile(ks, 1 - max(hdr$prob))
        show_x <- as.data.frame(x = object$x[ks > threshold,])
        colnames(show_x)[1:2] <- c("x","y")
      } else {
        show_x <- as.data.frame(x = object$x)
        colnames(show_x)[1:2] <- c("x","y")
      }
      p <- p + ggplot2::geom_point(
        data = show_x,
        mapping = aes(x = x, y = y),
        alpha = alpha
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
    if(show_mode) {
      p <- p +
        ggplot2::geom_point(
          data = density |> filter(density == max(density)),
          mapping = aes(x = y1, y = y2),
          color = grDevices::colorRampPalette(c(color, "black"))(3)[2]
        )
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
hdr_palette <- function(n, color = "#00659e") {
  rev(grDevices::colorRampPalette(c(color, "white"))(n+2)[seq(n)])
}

utils::globalVariables(c("x","y","y1","y2"))
