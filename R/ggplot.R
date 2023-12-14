#' Produce ggplot of densities in 1 or 2 dimensions
#'
#' @details
#' This function produces a ggplot of the density estimate produced by `ks::kde()`.
#' For univariate densities, it produces a line plot of the density function, with
#' an optional ribbon showing some highest density regions (HDRs) and/or the observations.
#' For bivariate densities, it produces a contour plot of the density function, with
#' the observations optionally shown as points.
#' The mode can also be drawn as a point with the HDRs.
#' For bivariate densities, the combination of `fill = TRUE`, `show_points = TRUE`,
#' `show_mode = TRUE`, and `prob = c(0.5, 0.99)` is equivalent to an HDR boxplot.
#' For univariate densities,  the combination of `show_hdr = TRUE`, `show_points = TRUE`,
#' `show_mode = TRUE`, and `prob = c(0.5, 0.99)` is equivalent to an HDR boxplot.
#'
#' @param object Probability density function as estimated by `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param fill If `TRUE`, and the density is bivariate, the bivariate contours
#' are shown as filled regions rather than lines.
#' @param show_hdr If `TRUE`, and the density is univariate, then the HDR regions
#' specified by `prob` are shown as a ribbon below the density.
#' @param show_points If `TRUE`, then individual points are plotted.
#' @param show_mode If `TRUE`, then the mode of the distribution is shown.
#' @param show_lookout If `TRUE`, then the observations with lookout probabilities less than 0.05 are shown in red.
#' @param color Color used for mode and HDR contours. If `palette = hdr_palette`,
#' it is also used as the basis for HDR regions.
#' @param palette Color palette function to use for HDR filled regions
#' (if `fill` is `TRUE` or `show_hdr` is `TRUE`).
#' @param alpha Transparency of points. When `fill` is `FALSE`, defaults to
#' min(1, 1000/n), where n is the number of observations. Otherwise, set to 1.
#' @param ... Additional arguments are currently ignored.
#' @return A ggplot object.
#' @author Rob J Hyndman
#' @examples
#' # Univariate density
#' c(rnorm(500), rnorm(500, 4, 1.5)) |>
#'   kde() |>
#'   autoplot(show_hdr = TRUE, prob= c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' ymat |>
#'   kde(H = kde_bandwidth(ymat)) |>
#'   autoplot(show_points = TRUE, alpha = 0.1, fill = TRUE)
#' @export

autoplot.kde <- function(object, prob = seq(9)/10, fill = FALSE,
    show_hdr = FALSE, show_points = FALSE, show_mode = FALSE, show_lookout = FALSE,
    color = "#00659e", palette = hdr_palette, alpha = ifelse(fill, 1, min(1, 1000/NROW(object$x))),
    ...) {
  if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  if(identical(palette, hdr_palette)) {
    colors <- hdr_palette(color = color, prob=prob)
  } else {
    colors <- palette(n = length(prob)+1)
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
        kscores <- calc_kde_scores(object$x, h = object$h,...)
        fi <- exp(-kscores$scores)
        threshold <- quantile(fi, prob = 1 - max(hdr$prob), type = 8)
        show_x <- tibble::tibble(x = object$x[fi < threshold])
      } else {
        show_x <- tibble::tibble(x = object$x)
      }
      p <- p + ggplot2::geom_point(
        data = show_x,
        mapping = aes(x = x, y = -maxden/40),
        alpha = alpha
      )
      if(show_lookout) {
        if(!show_hdr) {
          kscores <- calc_kde_scores(object$x, h = object$h,...)
        }
        lookout_highlight <- lookout(density_scores = kscores$scores, loo_scores = kscores$loo) < 0.05
        lookout <- tibble(x = object$x[lookout_highlight])
        p <- p + ggplot2::geom_point(
          data = lookout, mapping = aes(x = x, y = -maxden/40),
          color = "#ff0000"
        )
      }
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
          values = colors[-1],
          labels = paste0(100*rev(prob), "%")
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
    }
    if(show_mode) {
      p <- p +
        ggplot2::geom_line(
          data = expand.grid(mode = unique(hdr$mode), ends = c(0, -maxden/20)),
          mapping = aes(x = mode, y = ends, group = mode),
          color = color,
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
        kscores <- calc_kde_scores(object$x, H = object$H,...)
        fi <- exp(-kscores$scores)
        threshold <- quantile(fi, prob = 1 - max(hdr$prob), type = 8)
        show_x <- as.data.frame(x = object$x[fi < threshold,])
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
      if(show_lookout) {
        if(!fill) {
          kscores <- calc_kde_scores(object$x, H = object$H,...)
        }
        lookout_highlight <- lookout(density_scores = kscores$scores, loo_scores = kscores$loo) < 0.05
        lookout <- as.data.frame(x = object$x[lookout_highlight,])
        colnames(lookout)[1:2] <- c("x","y")
        p <- p + ggplot2::geom_point(
          data = lookout, mapping = aes(x = x, y = y),
          color = "#ff0000"
        )
      }
    }
    if(fill) {
      p <- p +
        geom_contour_filled(aes(x = y1, y = y2, z = density),
                            breaks = rev(c(hdr$density, 100))) +
        scale_fill_manual(
          values = colors[-1],
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
          color = color
        )
    }
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
  }
  return(p)
}

#' Color palette designed for plotting Highest Density Regions
#'
#' A sequential color palette is returned, with the first color being `color`,
#' and the rest of the colors being a mix of `color` with increasing amounts of white.
#' If `prob` is provided, then the mixing proportions are determined by `prob` (and
#' n is ignored). Otherwise the mixing proportions are equally spaced between 0 and 1.
#'
#' @param n Number of colors in palette.
#' @param color First color of vector.
#' @param prob Vector of probabilities between 0 and 1.
#' @return A function that returns a vector of colors of length `length(prob) + 1`.
#' @examples
#' hdr_palette(prob = c(0.5, 0.99))
#' @export
hdr_palette <- function(n, color = "#00659e", prob = NULL) {
  if(missing(prob)) {
    prob <- seq(n-1)/n
  } else if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  pc_colors <- grDevices::colorRampPalette(c(color, "white"))(150)[2:100]
  idx <- approx(seq(99)/100, seq(99), prob, rule=2)$y
  c(color, pc_colors[idx])
}

utils::globalVariables(c("x","y","y1","y2"))
