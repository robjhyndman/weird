#' Produce ggplot of densities from distributional objects in 1 or 2 dimensions
#'
#' @details
#' This function produces a ggplot of a density from a distributional object.
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
#' @param object distribution object from the distributional package or
#' \code{\link{dist_kde}}()
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
#' dist_kde(c(rnorm(500), rnorm(500, 4, 1.5))) |>
#'   autoplot(show_hdr = TRUE, prob = c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' ymat |>
#'   dist_kde(ymat) |>
#'   autoplot(show_points = TRUE, alpha = 0.1, fill = TRUE)
#' @exportS3Method ggplot2::autoplot

autoplot.distribution <- function(
    object, prob = seq(9) / 10, fill = FALSE,
    show_hdr = FALSE, show_points = FALSE, show_mode = FALSE, show_lookout = FALSE,
    color = "#00659e", palette = hdr_palette, alpha = ifelse(fill, 1, min(1, 1000 / NROW(object$x))),
    ...) {
  if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  if (identical(palette, hdr_palette)) {
    colors <- hdr_palette(color = color, prob = prob)
  } else {
    colors <- palette(n = length(prob) + 1)
  }
  dist <- family(object)
  no_groups <- length(dist) == 1L
  # Names of distributions
  dist_names <- format(object)
  if(length(unique(dist_names)) != length(dist_names)) {
    # Find duplicates
    dup <- duplicated(dist_names)
    dist_names[dup] <- paste0(dist_names[dup], "a")
  }

  # Extract data
  x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
  names(x) <- dist_names
  # Check if multivariate
  d <- unlist(lapply(x, NCOL))
  d[dist != "kde"] <- 1
  if (any(d > 2)) {
    stop("Only univariate and bivariate densities are supported")
  } else if (any(d == 2)) {
    stop("Bivariate plotting not yet implemented")
  }
  if (show_points) {
    if (all(lengths(x) == 0)) {
      warning("No observations found")
      show_points <- FALSE
    }
  }
  # Set up data frame for densities
  range_x <- range(unlist(quantile(object, p = c(0.002, 0.998))))
  if (show_points) {
    range_x <- range(range_x, unlist(x))
  }
  y <- seq(min(range_x), max(range_x), length = 501)
  df <- c(list(y), density(object, at = y))
  names(df) <- c("y", dist_names)
  df <- tibble::as_tibble(df) |>
    tidyr::pivot_longer(
      cols = -y, names_to = "Distribution",
      values_to = "Density"
    )
  maxden <- max(df$Density)

  # Add density lines to plot
  p <- ggplot(df)
  if (no_groups) {
    p <- p + geom_line(aes(x = y, y = Density))
  } else {
    p <- p + geom_line(aes(x = y, y = Density, color = Distribution))
  }

  # Set up HDRs if needed
  if (show_hdr) {
    prob <- sort(unique(prob), decreasing = TRUE)
    hdr <- purrr::map_dfr(prob, function(u) {
      hdri <- hdr(object, size = u * 100)
      tibble(
        level = u*100,
        Distribution = dist_names,
        lower = vctrs::field(hdri, "lower"),
        upper = vctrs::field(hdri, "upper")
      ) |>
        tidyr::unnest(c(lower, upper))
    })
  }

  # Show observations in bottom margin
  if (show_points) {
    if(show_hdr) {
      # Only show points outside largest HDR
      fi <- purrr::map2(object, x, function(u,x) {
        if(is.null(x)) {
          return(NULL)
        } else {
          density(u, at = x)[[1]]
        }
      })
      threshold <- lapply(fi, quantile, prob = 1 - max(prob), type = 8)
      idx <- purrr::map2(fi, threshold, function(f,t){which(f < t)})
      x <- purrr::map2(x, idx, function(x,i) x[i])
    }
    # Drop distributions with no data
    some_data <- names(x)[lengths(x) > 0]
    x <- x[some_data]
    show_x <- tibble::as_tibble(x) |>
      tidyr::pivot_longer(
        cols = everything(), names_to = "Distribution",
        values_to = "x"
      )
    if (no_groups) {
      a <- aes(x = x, y = -maxden * as.numeric(factor(Distribution)) / 40)
    } else {
      a <- aes(x = x, y = -maxden * (as.numeric(factor(Distribution))- 0.5) / 20,
               color = Distribution)
    }
    p <- p + ggplot2::geom_point(data = show_x, mapping = a, alpha = alpha)
  }
  if (show_lookout) {
    stop("Not yet implemented")
    if (!show_hdr) {
      kscores <- calc_kde_scores(object$x, h = object$h, ...)
    }
    lookout_highlight <- lookout(density_scores = kscores$scores, loo_scores = kscores$loo) < 0.05
    lookout <- tibble(x = object$x[lookout_highlight])
    p <- p + ggplot2::geom_point(
      data = lookout, mapping = aes(x = x, y = -maxden / 40),
      color = "#ff0000"
    )
  }

  if (show_hdr) {
    p <- p +
      ggplot2::geom_rect(
        data = hdr |> mutate(id = row_number()),
        aes(
          xmin = lower, xmax = upper,
          ymin = -maxden * as.numeric(factor(Distribution)) / 20,
          ymax = -maxden * (as.numeric(factor(Distribution))-1) / 20,
          fill = factor(level, levels = sort(prob * 100)),
          color = Distribution,
        )
      ) +
      scale_fill_manual(
        breaks = rev(prob * 100),
        values = colors[-1],
        labels = paste0(100 * rev(prob), "%")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "HDR coverage"))
  }
  if (show_mode) {
    modes <- df |>
      dplyr::group_by(Distribution) |>
      dplyr::filter(Density == max(Density)) |>
      select(mode = y, Distribution)
    if (no_groups) {
      a <- aes(x = mode, y = -maxden / 20)
    } else {
      a <- aes(x = mode, y = -maxden * (as.numeric(factor(Distribution))- 0.5) / 20, color = Distribution)
    }
    p <- p +
      ggplot2::geom_point(data = modes, mapping = a, shape = 17, size = 3)
  }

  return(p)
}
