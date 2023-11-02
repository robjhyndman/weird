#' Produce ggplot of densities in 1 or 2 dimensions
#' @param object Probability density function as estimated by `density()` or `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param filled If `TRUE`, the bivariate contours are shown as shaded regions rather than lines.
#' @param show_hdr If `TRUE`, the HDR regions specified by `prob` are shown as a ribbon below the density.
#' @param palette Color palette to use for HDR regions.
#' @param ... Additional arguments are ignored.
#' @examples
#' # Univariate density
#' c(rnorm(100), rnorm(100, 3, 1)) |>
#'   density() |>
#'   autoplot(show_hdr = TRUE, prob= c(0.5, 0.99)) +
#'   scale_fill_brewer(palette = "OrRd", direction = -1)
#' tibble(y1 = rnorm(100), y2 = y1 + rnorm(100)) |>
#'   density(H = 0.4*diag(2)) |>
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
    )
    density$density <- c(object$estimate)
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
        ggplot2::geom_rect(data = hdr,
          aes(xmin = lower, xmax=upper, ymin = -maxden/20, ymax=0,
              fill = factor(prob))) +
        scale_fill_manual(
          breaks = hdr$prob,
          values = rev(palette(nint)),
          labels = paste0(hdr$prob, "%")
        ) +
        ggplot2::guides(fill = "none")
    }
  } else {
    # Plot the contours
    p <- density |>
      ggplot() +
      labs(x = object$names[1], y = object$names[2])
    if(filled) {
      p <- p +
        geom_contour_filled(aes(x = y1, y = y2, z = density),
                            breaks = rev(c(hdr$density, 100))) +
        scale_fill_manual(
          values = rev(palette(length(hdr$prob))),
          labels = rev(paste0(100 * hdr$prob, "%"))
        )
    } else {
      p <- p + geom_contour(aes(x = y1, y = y2, z = density),
                            breaks = hdr$density)
    }
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
  }
  return(p)
}

utils::globalVariables(c("y","y1","y2"))
