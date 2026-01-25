#' Produce ggplot of densities from distributional objects in 1 or 2 dimensions
#'
#' @details
#' This function produces a ggplot of a density from a distributional object.
#' For univariate densities, it produces a line plot of the density function, with
#' an optional ribbon showing some highest density regions (HDRs) and/or the observations.
#' For bivariate densities, it produces ah HDR contour plot of the density function, with
#' the observations optionally shown as points.
#' The mode can also be drawn as a point.
#' The combination of `hdr = "fill"`, `show_points = TRUE`,
#' `show_mode = TRUE`, and `prob = c(0.5, 0.99)` is equivalent to showing
#' HDR boxplots.
#'
#' @param object distribution object from the distributional package or
#' \code{\link{dist_kde}}()
#' @param prob Probability of the HDRs to be drawn.
#' @param hdr Character string describing how the HDRs are to be shown. Options
#' are "none", "fill", "points" and "contours" (the latter only for bivariate plots).
#' If `NULL`, then "none" is used for univariate distributions and "contours" for bivariate.
#' @param show_points If `TRUE`, then individual observations are plotted.
#' @param show_mode If `TRUE`, then the mode of the distribution is shown as a
#' point.
#' @param show_anomalies If `TRUE`, then the observations with surprisal
#' probabilities less than 0.005 (using a GPD approximation) are shown in black.
#' @param colors Color palette to use. If there are more than
#' `length(colors)` distributions, they are recycled. Default is the
#' Okabe-Ito color palette.
#' @param alpha Transparency of points. Ignored if `show_points` is `FALSE`.
#' Defaults to min(1, 500/n), where n is the number of observations plotted.
#' @param jitter For univariate distributions, when `jitter` is `TRUE` and
#' `show_points` is TRUE, a small amount of vertical jittering is applied to the
#' observations. Ignored for bivariate distributions.
#' @param ngrid Number of grid points to use for the density function.
#' @return A ggplot object.
#' @author Rob J Hyndman
#' @examples
#' # Univariate densities
#' kde <- dist_kde(c(rnorm(500), rnorm(500, 4, .5)))
#' gg_density(kde,
#'   hdr = "fill", prob = c(0.5, 0.95), color = "#c14b14",
#'   show_mode = TRUE, show_points = TRUE, jitter = TRUE
#' )
#' c(dist_normal(), kde) |>
#'   gg_density(hdr = "fill", prob = c(0.5, 0.95))
#' # Bivariate density
#' tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000)) |>
#'   dist_kde() |>
#'   gg_density(show_points = TRUE, alpha = 0.1, hdr = "fill")
#' @export

gg_density <- function(
  object,
  prob = seq(9) / 10,
  hdr = NULL,
  show_points = FALSE,
  show_mode = FALSE,
  show_anomalies = FALSE,
  colors = c(
    "#0072b2",
    "#D55E00",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442",
    "#333333"
  ),
  alpha = NULL,
  jitter = FALSE,
  ngrid = 501
) {
  if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  prob <- sort(prob)
  d <- dimension_dist(object)
  if (is.null(hdr)) {
    if (d == 1) {
      hdr <- "none"
    } else {
      hdr <- "contours"
    }
  }
  hdr <- match.arg(hdr, c("none", "fill", "points", "contours"))

  # Set up data frame containing densities
  df <- make_density_df(object, ngrid = ngrid)
  # Repeat colors
  if (length(object) > length(colors)) {
    warning(
      "Insufficient colors. Some densities will be plotted in the same color."
    )
    colors <- rep(colors, 1 + round(length(object) / length(colors)))[seq_along(
      object
    )]
  }

  # HDR thresholds if needed
  if (hdr != "none") {
    # HDR thresholds
    threshold <- hdr_table(object, prob) |>
      dplyr::transmute(
        level = 100 * prob,
        Distribution = distribution,
        threshold = density
      ) |>
      dplyr::distinct()
    # HDR color palette
    hdr_colors <- lapply(
      colors,
      function(u) {
        hdr_palette(color = u, prob = c(prob, 1))
      }
    )
    names(hdr_colors) <- names_dist(object, unique = TRUE)
  } else {
    threshold <- NULL
    hdr_colors <- as.list(colors)
  }
  # Set up data frame containing observations
  if (
    any(
      stats::family(object) == "kde" &
        (show_points | show_anomalies | hdr == "points")
    )
  ) {
    show_x <- show_data(object, prob, threshold, anomalies = show_anomalies)
  } else {
    show_x <- NULL
  }

  if (d == 1) {
    if (hdr == "contours") {
      stop("Contours not possible for univariate densities")
    }
    gg_density1(
      object,
      df,
      show_x,
      threshold,
      prob,
      hdr,
      TRUE,
      show_points,
      show_anomalies,
      show_mode,
      hdr_colors,
      alpha,
      jitter
    )
  } else if (d == 2) {
    gg_density2(
      object,
      df,
      show_x,
      threshold,
      prob,
      hdr,
      show_points,
      show_anomalies,
      show_mode,
      hdr_colors,
      alpha
    )
  } else {
    stop("Only univariate and bivariate densities are supported")
  }
}

gg_density1 <- function(
  object,
  df,
  show_x,
  threshold,
  prob,
  hdr,
  show_density,
  show_points,
  show_anomalies,
  show_mode,
  hdr_colors,
  alpha,
  jitter
) {
  dist_names <- names_dist(object, unique = TRUE)
  maxden <- max(df$Density)
  discrete <- is.logical(df$x) | is.integer(df$x)

  # Start plot
  p <- ggplot(df)
  # Add density lines to plot
  if (show_density) {
    if (discrete) {
      p <- p +
        geom_segment(aes(
          x = x,
          xend = x,
          y = 0,
          yend = Density,
          color = Distribution
        ))
    } else {
      p <- p + geom_line(aes(x = x, y = Density, color = Distribution))
    }
  }
  # Add HDRs to plot
  if (hdr == "fill") {
    prob <- sort(unique(prob), decreasing = TRUE)
    hdrdf <- purrr::map_dfr(prob, function(u) {
      hdri <- distributional::hdr(object, size = u * 100)
      tibble(
        level = u * 100,
        Distribution = dist_names,
        lower = vctrs::field(hdri, "lower"),
        upper = vctrs::field(hdri, "upper")
      ) |>
        tidyr::unnest(c(lower, upper))
    })
    hdrdf$id <- seq(NROW(hdrdf))
    hdrdf$ymin <- -maxden *
      as.numeric(factor(hdrdf$Distribution, levels = dist_names)) /
      20
    hdrdf$ymax <- hdrdf$ymin + maxden / 20
    # Add one interval at a time because we can't use multiple ggplot fill scales
    levels <- sort(unique(hdrdf$level), decreasing = TRUE)
    for (dist in unique(hdrdf$Distribution)) {
      for (i in seq_along(levels)) {
        p <- p +
          ggplot2::geom_rect(
            data = hdrdf[
              hdrdf$Distribution == dist & hdrdf$level == levels[i],
            ],
            aes(xmin = lower, xmax = upper, ymin = ymin, ymax = ymax),
            fill = rev(hdr_colors[[dist]])[i + 1]
          )
      }
    }
  }
  # Show observations
  if (!is.null(show_x)) {
    if (is.null(alpha)) {
      alpha <- min(1, 500 / NROW(show_x))
    }
    # Add y plotting position for observations
    show_x$y <- -maxden *
      (as.numeric(factor(show_x$Distribution, levels = dist_names)) - 0.5) /
      20
    if (jitter) {
      show_x$y <- show_x$y +
        stats::runif(NROW(show_x), -maxden / 45, maxden / 45)
    }
    if (hdr == "fill") {
      # Drop observations obscured by largest HDR
      include <- paste0(prob * 100, "%")
      show_x <- show_x |>
        dplyr::filter(!(group %in% include))
    }
  }
  if (NROW(show_x) > 0) {
    if (show_anomalies) {
      # Split data set into anomalies and the rest
      outliers <- show_x[show_x$anomaly, ]
      show_x <- show_x[!show_x$anomaly, ]
    }
  } else {
    outliers <- NULL
  }
  if (NROW(show_x) > 0) {
    if (hdr == "points") {
      # Add one interval at a time because we can't use multiple ggplot color scales
      levels <- sort(unique(show_x$level))
      for (dist in unique(show_x$Distribution)) {
        for (i in seq_along(levels)) {
          p <- p +
            geom_point(
              data = show_x[
                show_x$Distribution == dist & show_x$level == levels[i],
              ],
              aes(x = x, y = y),
              color = hdr_colors[[dist]][i + 1]
            )
        }
      }
    } else if (show_points) {
      p <- p +
        ggplot2::geom_point(
          data = show_x,
          mapping = aes(x = x, y = y, color = Distribution),
          alpha = alpha
        )
    }
  }
  if (show_anomalies) {
    if (NROW(outliers) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = outliers,
          mapping = aes(x = x, y = y),
          color = "#000"
        )
    }
  }

  # Add mode to plot
  if (show_mode) {
    modes <- df |>
      dplyr::group_by(Distribution) |>
      dplyr::filter(Density == max(Density)) |>
      dplyr::ungroup() |>
      dplyr::select(mode = x, Distribution) |>
      dplyr::mutate(
        i = as.numeric(factor(Distribution, levels = dist_names)),
        lower = -maxden * i / 20,
        upper = -maxden * (i - 1) / 20
      ) |>
      tidyr::pivot_longer(lower:upper, values_to = "y", names_to = "ypos")
    p <- p +
      ggplot2::geom_line(
        data = modes,
        mapping = aes(
          x = mode,
          y = y,
          group = Distribution,
          color = Distribution
        ),
        linewidth = 1
      )
  }

  # Color scale and legend
  colors <- unlist(lapply(hdr_colors, function(u) {
    u[1]
  }))
  p <- p +
    ggplot2::scale_color_manual(
      breaks = dist_names,
      values = colors,
      labels = dist_names
    )

  # Don't show color legend if only one density
  if (length(object) == 1L) {
    p <- p + ggplot2::guides(color = "none")
  }

  return(p)
}

gg_density2 <- function(
  object,
  df,
  show_x,
  threshold,
  prob,
  hdr,
  show_points,
  show_anomalies,
  show_mode,
  hdr_colors,
  alpha
) {
  if (length(object) > 1) {
    stop("I can only handle one bivariate density in a plot")
  }
  dist_names <- names_dist(object, unique = TRUE)
  hdr_colors <- hdr_colors[[1]]
  # Start plot
  p <- ggplot(df)
  # Show filled regions
  if (hdr == "fill") {
    p <- p +
      geom_contour_filled(
        aes(x = x, y = y, z = Density),
        breaks = c(Inf, threshold$threshold)
      ) +
      scale_fill_manual(
        values = hdr_colors[-1],
        labels = paste0(100 * prob, "%"),
        name = "HDR coverage"
      )
  }
  # Plot individual observations
  # Show observations
  if (!is.null(show_x)) {
    if (is.null(alpha)) {
      alpha <- min(1, 500 / NROW(show_x))
    }
    if (hdr == "fill") {
      # Drop observations obscured by largest HDR
      include <- paste0(prob * 100, "%")
      show_x <- show_x |>
        dplyr::filter(!(group %in% include))
    }
    if (show_anomalies) {
      # Split data set into anomalies and the rest
      outliers <- show_x[show_x$anomaly, ]
      show_x <- show_x[!show_x$anomaly, ]
    }
    if (hdr == "points") {
      outsideprob <- 1 - 0.01 * show_anomalies
      p <- p +
        ggplot2::geom_point(
          data = as.data.frame(show_x) |>
            filter(),
          mapping = aes(x = x, y = y, col = group)
        ) +
        ggplot2::scale_color_manual(
          values = hdr_colors[-1],
          labels = paste0(100 * c(prob, outsideprob), "%"),
          name = "HDR coverage"
        )
    } else if (show_points) {
      p <- p +
        ggplot2::geom_point(
          data = show_x,
          mapping = aes(x = x, y = y),
          color = head(hdr_colors, 1), # dplyr::if_else(show_anomalies, tail(hdr_colors, 1), head(hdr_colors, 1)),
          alpha = alpha
        )
    }
    if (show_anomalies) {
      p <- p +
        ggplot2::geom_point(
          data = outliers,
          mapping = aes(x = x, y = y),
          color = "#000"
        )
    }
  }
  # Show contours
  if (hdr == "contours") {
    p <- p +
      geom_contour(
        aes(x = x, y = y, z = Density),
        breaks = threshold$threshold,
        color = hdr_colors[1]
      )
  }
  if (show_mode) {
    modes <- df |>
      dplyr::filter(Density == max(Density))
    p <- p +
      ggplot2::geom_point(
        data = modes,
        mapping = aes(x = x, y = y),
        color = hdr_colors[1]
      )
  }
  return(p)
}

utils::globalVariables(c(
  "dist",
  "Density",
  "Distribution",
  "level",
  "i",
  "den",
  "ymin",
  "ymax"
))
