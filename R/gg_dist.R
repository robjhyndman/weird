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
#' @param show_points If `TRUE`, then individual observations are plotted.
#' @param show_mode If `TRUE`, then the mode of the distribution is shown.
#' @param show_lookout If `TRUE`, then the observations with lookout probabilities less than 0.05 are shown in red.
#' @param ngrid Number of points at which to evaluate the density function.
#' @param color Color used for mode and HDR contours. If `palette = hdr_palette`,
#' it is also used as the basis for HDR regions.
#' @param palette Color palette function to use for HDR filled regions
#' (if `fill` is `TRUE` or `show_hdr` is `TRUE`).
#' @param alpha Transparency of points. When `fill` is `FALSE`, defaults to
#' min(1, 1000/n), where n is the number of observations. Otherwise, set to 1.
#' @param jitter When TRUE and `show_points` is TRUE, a small amount of vertical
#' jittering is applied to the observations.
#' @param ... Additional arguments are currently ignored.
#' @return A ggplot object.
#' @author Rob J Hyndman
#' @examples
#' # Univariate density
#' dist_kde(c(rnorm(500), rnorm(500, 4, 1.5))) |>
#'   gg_dist(show_hdr = TRUE, prob = c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' # ymat |>
#' #  dist_kde(ymat) |>
#' #  gg_dist(show_points = TRUE, alpha = 0.1, fill = TRUE)
#' @export

gg_dist <- function(
    object, prob = seq(9) / 10, fill = FALSE,
    show_hdr = FALSE, show_points = FALSE, show_mode = FALSE, show_lookout = FALSE,
    ngrid = 501, color = "#00659e", palette = hdr_palette, alpha = NULL,
    jitter = FALSE, ...) {
  if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  if (identical(palette, hdr_palette)) {
    colors <- hdr_palette(color = color, prob = prob)
  } else {
    colors <- palette(n = length(prob) + 1)
  }
  # Names of distributions
  object_names <- names(object)
  dist_names <- make.unique(format(object))
  idx <- which(object_names != "")
  dist_names[idx] <- object_names[idx]
  dist <- stats::family(object)
  no_groups <- length(dist) == 1L

  # Set up data frame for densities
  df <- make_density_df(object, ngrid)
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
      hdri <- distributional::hdr(object, size = u * 100)
      tibble(
        level = u * 100,
        Distribution = dist_names,
        lower = vctrs::field(hdri, "lower"),
        upper = vctrs::field(hdri, "upper")
      ) |>
        tidyr::unnest(c(lower, upper))
    })
    hdr$id <- seq(NROW(hdr))
  }

  # Show observations in bottom margin
  if (show_points) {
    # Extract data
    x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
    names(x) <- dist_names
    if (all(lengths(x) == 0)) {
      stop("No observations found. Set show_points to FALSE")
    }
    if (show_hdr) {
      # Drop points obscured by largest HDR
      thresh <- tibble(object = object, Distribution = dist_names) |>
        dplyr::left_join(hdr |> filter(level == max(level)), by = "Distribution") |>
        dplyr::rowwise() |>
        dplyr::mutate(fi = density(object, at = lower)) |>
        dplyr::select(Distribution, fi)
      threshold <- as.list(thresh$fi)
      names(threshold) <- thresh$Distribution
      fi <- purrr::map2(object, x, function(u, x) {
        if (is.null(x)) {
          return(NULL)
        } else {
          density(u, at = x)[[1]]
        }
      })
      idx <- purrr::map2(fi, threshold, function(f, t) {
        which(f < t)
      })
      x <- purrr::map2(x, idx, function(x, i) x[i])
    }
    # Drop distributions with no data
    some_data <- names(x)[lengths(x) > 0]
    x <- x[some_data]
    show_x <- tibble(
      Distribution = rep(names(x), lengths(x)),
      x = unlist(x)
    )
    if (no_groups) {
      a <- aes(x = x, y = -maxden * as.numeric(factor(Distribution)) / 40)
    } else {
      a <- aes(
        x = x, y = -maxden * (as.numeric(factor(Distribution)) - 0.5) / 20,
        color = Distribution
      )
    }
    if (is.null(alpha)) {
      alpha <- ifelse(fill, 1, min(1, 1000 / max(lengths(x))))
    }
    if (jitter) {
      p <- p + ggplot2::geom_jitter(
        data = show_x, mapping = a, alpha = alpha,
        width = 0, height = maxden / 100
      )
    } else {
      p <- p + ggplot2::geom_point(data = show_x, mapping = a, alpha = alpha)
    }
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
    if (no_groups) {
      p <- p +
        ggplot2::geom_rect(
          data = hdr,
          aes(
            xmin = lower, xmax = upper,
            ymin = -maxden * as.numeric(factor(Distribution)) / 20,
            ymax = -maxden * (as.numeric(factor(Distribution)) - 1) / 20,
            alpha = -level
          ),
          fill = color
        )
    } else {
      p <- p +
        ggplot2::geom_rect(
          data = hdr,
          aes(
            xmin = lower, xmax = upper,
            ymin = -maxden * as.numeric(factor(Distribution)) / 20,
            ymax = -maxden * (as.numeric(factor(Distribution)) - 1) / 20,
            alpha = -level,
            fill = Distribution
          )
        )
    }
    p <- p +
      ggplot2::scale_alpha(
        name = "HDR coverage",
        breaks = -100 * prob,
        labels = paste0(100 * prob, "%"),
        range = c(0.2, 1)
      )
  }
  if (show_mode) {
    modes <- df |>
      dplyr::group_by(Distribution) |>
      dplyr::filter(Density == max(Density)) |>
      select(mode = y, Distribution)
    if (no_groups) {
      a <- aes(x = mode, y = -maxden / 20)
    } else {
      a <- aes(x = mode, y = -maxden * (as.numeric(factor(Distribution)) - 0.5) / 20, color = Distribution)
    }
    p <- p +
      ggplot2::geom_point(data = modes, mapping = a, shape = 17, size = 3)
  }

  return(p)
}

utils::globalVariables(c("Density", "Distribution", "level"))
