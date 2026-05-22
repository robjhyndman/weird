#' Produce ggplot of densities from distributional objects in 1 or 2 dimensions
#'
#' @details
#' This function produces a ggplot of a density from a distributional object.
#' For univariate densities, it produces a line plot of the density function, with
#' an optional ribbon showing some highest density regions (HDRs) and/or the observations.
#' For bivariate densities, it produces an HDR contour plot of the density function, with
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
#' @return A ggplot object.
#' @author Rob J Hyndman
#' @examples
#' # Univariate densities
#' kde <- dist_kde(c(rnorm(500), rnorm(500, 4, 0.5)))
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
  jitter = FALSE
) {
  if (min(prob) <= 0 || max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  prob <- sort(prob)
  d <- dimension_dist(object)
  if (is.null(hdr)) {
    hdr <- if (d == 1) "none" else "contours"
  }
  hdr <- match.arg(hdr, c("none", "fill", "points", "contours"))

  if (length(object) > length(colors)) {
    warning(
      "Insufficient colors. Some densities will be plotted in the same color."
    )
    colors <- rep(colors, ceiling(length(object) / length(colors)))[seq_along(
      object
    )]
  }

  # Pre-compute the density grid once and pass it through to gg_density1 /
  # gg_density2
  df <- density_df(object)

  if (hdr != "none") {
    # Compute HDR threshold densities
    threshold <- hdr_table_with_data(object, prob, df) |>
      dplyr::transmute(
        level = 100 * prob,
        distribution = distribution,
        threshold = density
      ) |>
      dplyr::distinct()
    # HDR color palette
    hdr_colors <- lapply(colors, function(u) {
      hdr_palette(color = u, prob = c(prob, 1))
    })
    names(hdr_colors) <- names_dist(object, unique = TRUE)
  } else {
    threshold <- NULL
    hdr_colors <- as.list(colors)
  }

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
      show_x,
      threshold,
      prob,
      hdr,
      show_points,
      show_anomalies,
      show_mode,
      hdr_colors,
      alpha,
      jitter,
      df = df
    )
  } else if (d == 2) {
    gg_density2(
      object,
      show_x,
      threshold,
      prob,
      hdr,
      show_points,
      show_anomalies,
      show_mode,
      hdr_colors,
      alpha,
      df = df
    )
  } else {
    stop("Only univariate and bivariate densities are supported")
  }
}

# `df` and `show_density` are retained as trailing optional arguments for the
# benefit of internal callers (e.g. gg_hdrboxplot()) that want to suppress the
# density curve while still using the rest of the layered composition.
gg_density1 <- function(
  object,
  show_x,
  threshold,
  prob,
  hdr,
  show_points,
  show_anomalies,
  show_mode,
  hdr_colors,
  alpha,
  jitter,
  df = NULL,
  show_density = TRUE
) {
  dist_names <- names_dist(object, unique = TRUE)

  if (is.null(df)) {
    df <- density_df(object)
  }
  maxden <- max(df$density)
  discrete <- is.logical(df$x) | is.integer(df$x)

  p <- ggplot(data = df)

  # ----- Density representation -----
  if (show_density) {
    if (discrete) {
      p <- p +
        ggplot2::geom_segment(
          mapping = aes(
            x = x,
            xend = x,
            y = 0,
            yend = density,
            color = distribution
          )
        )
    } else {
      p <- p +
        geom_line(aes(x = x, y = density, colour = distribution))
    }
  }

  # ----- HDR fill rectangles below the axis -----
  if (hdr == "fill") {
    prob_desc <- sort(unique(prob), decreasing = TRUE)
    hdrdf <- purrr::map_dfr(prob_desc, function(u) {
      hdri <- distributional::hdr(object, size = u * 100, n = 4096)
      tibble(
        level = u * 100,
        distribution = dist_names,
        lower = vctrs::field(hdri, "lower"),
        upper = vctrs::field(hdri, "upper")
      ) |>
        tidyr::unnest(c(lower, upper))
    })
    hdrdf$id <- seq_len(NROW(hdrdf))
    hdrdf$ymin <- -maxden *
      as.numeric(factor(hdrdf$distribution, levels = dist_names)) /
      20
    hdrdf$ymax <- hdrdf$ymin + maxden / 20
    levels <- sort(unique(hdrdf$level), decreasing = TRUE)
    for (dist in unique(hdrdf$distribution)) {
      for (i in seq_along(levels)) {
        p <- p +
          ggplot2::geom_rect(
            data = hdrdf[
              hdrdf$distribution == dist & hdrdf$level == levels[i],
            ],
            aes(xmin = lower, xmax = upper, ymin = ymin, ymax = ymax),
            fill = rev(hdr_colors[[dist]])[i + 1]
          )
      }
    }
  }

  # ----- Observation rug -----
  if (!is.null(show_x)) {
    if (is.null(alpha)) {
      alpha <- min(1, 500 / NROW(show_x))
    }
    # Add y plotting position for observations
    show_x$y <- -maxden *
      (as.numeric(factor(show_x$distribution, levels = dist_names)) - 0.5) /
      20
    if (jitter) {
      show_x$y <- show_x$y +
        stats::runif(NROW(show_x), -maxden / 45, maxden / 45)
    }
    if (hdr == "fill") {
      # Drop points inside any HDR region (they'd be hidden under the fill)
      include <- paste0(prob * 100, "%")
      show_x <- show_x |> dplyr::filter(!(group %in% include))
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
      for (dist in unique(show_x$distribution)) {
        for (i in seq_along(levels)) {
          p <- p +
            ggplot2::geom_point(
              data = show_x[
                show_x$distribution == dist & show_x$level == levels[i],
              ],
              mapping = aes(x = x, y = y),
              color = hdr_colors[[dist]][i + 1]
            )
        }
      }
    } else if (show_points) {
      p <- p +
        ggplot2::geom_point(
          data = show_x,
          mapping = aes(x = x, y = y, color = distribution),
          alpha = alpha
        )
    }
  }
  if (show_anomalies && NROW(outliers) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = outliers,
        mapping = aes(x = x, y = y),
        color = "#000"
      )
  }

  # ----- Mode markers -----
  if (show_mode) {
    modes <- df |>
      dplyr::group_by(distribution) |>
      dplyr::filter(density == max(density)) |>
      dplyr::ungroup() |>
      dplyr::select(mode = x, distribution) |>
      dplyr::mutate(
        i = as.numeric(factor(distribution, levels = dist_names)),
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
          group = distribution,
          color = distribution
        ),
        linewidth = 1
      )
  }

  # ----- Colour scale & legend -----
  pal <- unlist(lapply(hdr_colors, function(u) u[1]))
  p <- p +
    ggplot2::scale_color_manual(
      breaks = dist_names,
      values = pal,
      labels = dist_names
    )

  # Don't show color legend if only one density
  if (length(object) == 1L) {
    p <- p + ggplot2::guides(color = "none")
  }

  p
}

gg_density2 <- function(
  object,
  show_x,
  threshold,
  prob,
  hdr,
  show_points,
  show_anomalies,
  show_mode,
  hdr_colors,
  alpha,
  df = NULL
) {
  if (length(object) > 1) {
    stop("I can only handle one bivariate density in a plot")
  }
  hdr_colors <- hdr_colors[[1]]

  if (is.null(df)) {
    df <- density_df(object)
  }
  # Reuse the threshold densities computed by gg_density(); fall back to a
  # fresh hdr_table() call only if the caller did not pre-compute them.
  if (!is.null(threshold)) {
    thresholds <- threshold$threshold
  } else {
    thresholds <- hdr_table_with_data(object, prob = prob, df)$density
  }

  p <- ggplot(data = df)

  if (hdr == "fill") {
    p <- p +
      geom_contour_filled(
        aes(x = x, y = y, z = density),
        breaks = c(Inf, thresholds)
      ) +
      ggplot2::scale_fill_manual(
        values = hdr_colors[-1],
        labels = paste0(100 * prob, "%"),
        name = "HDR coverage"
      )
  } else if (hdr == "contours") {
    p <- p +
      geom_contour(
        aes(x = x, y = y, z = density),
        breaks = thresholds,
        colour = hdr_colors[1]
      )
  }

  if (!is.null(show_x)) {
    if (is.null(alpha)) {
      alpha <- min(1, 500 / NROW(show_x))
    }
    if (hdr == "fill") {
      # Drop points inside any HDR region (they'd be hidden under the fill)
      include <- paste0(prob * 100, "%")
      show_x <- show_x |> dplyr::filter(!(group %in% include))
    }
    if (show_anomalies) {
      # Split data set into anomalies and the rest
      outliers <- show_x[show_x$anomaly, ]
      show_x <- show_x[!show_x$anomaly, ]
    } else {
      outliers <- NULL
    }
    if (hdr == "points") {
      outsideprob <- 1 - 0.01 * show_anomalies
      p <- p +
        ggplot2::geom_point(
          data = as.data.frame(show_x),
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
          color = head(hdr_colors, 1),
          alpha = alpha
        )
    }
    if (show_anomalies && !is.null(outliers) && NROW(outliers) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = outliers,
          mapping = aes(x = x, y = y),
          color = "#000"
        )
    }
  }

  if (show_mode) {
    modes <- df |> dplyr::filter(density == max(density))
    p <- p +
      ggplot2::geom_point(
        data = modes,
        mapping = aes(x = x, y = y),
        color = hdr_colors[1]
      )
  }

  p
}

utils::globalVariables(c(
  "dist",
  "density",
  "distribution",
  "level",
  "i",
  "den",
  "ymin",
  "ymax"
))
