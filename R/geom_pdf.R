#' Probability density function geom
#'
#' @description
#' A ggplot2 layer that displays the probability density function (pdf) of a
#' [distributional] object. For univariate distributions it draws a line plot
#' of the density; for bivariate distributions, use [geom_pdf_2d()].
#'
#' @details
#' Unlike most geoms, `geom_pdf()` does not consume data through the standard
#' data/aesthetic pipeline. Instead, the distribution is supplied through the
#' `dist` parameter and the underlying [StatPdf] generates the plotting data
#' from it. The computed columns `x`, `density`, and `distribution` are
#' available via [ggplot2::after_stat()].
#'
#' @param mapping Set of aesthetic mappings. Usually `NULL`; when `length(dist) > 1`
#'   and `mapping` is `NULL`, `colour` is automatically mapped to the computed
#'   `distribution` variable.
#' @param data A data frame. Usually `NULL`; the layer does not need user data.
#' @param dist A distribution object from the distributional package, or from
#'   [dist_kde()].
#' @param scale Scaling factor applied to the density.
#' @param ngrid Number of grid points at which the density is evaluated.
#'   Ignored when `density_df` is supplied.
#' @param density_df This argument is for internal package use to provide pre-computed
#' data frame containing density values when it is available.
#' @param geom The geometric object used to display the layer. Defaults to
#'   `"line"`; alternatives such as `"area"` or `"path"` also work.
#' @param position Position adjustment, defaults to `"identity"`.
#' @param ... Other arguments passed to the underlying geom (e.g. `linetype`,
#'   `linewidth`, `colour`, `alpha`).
#' @param na.rm Passed through to the underlying geom.
#' @param show.legend Logical; should this layer be included in legends?
#' @param inherit.aes Logical; if `FALSE`, overrides the default aesthetics.
#'
#' @return A ggplot2 layer.
#' @author Rob J Hyndman
#'
#' @examples
#' # Univariate example
#' mix <- dist_mixture(
#'   dist_normal(-2, 1),
#'   dist_normal(2, 1),
#'   weights = c(1 / 3, 2 / 3)
#' )
#' ggplot() +
#'   geom_pdf(dist = mix) +
#'   geom_pdf(dist = dist_normal(-2, 1), scale = 1 / 3, linetype = "dashed") +
#'   geom_pdf(dist = dist_normal(2, 1),  scale = 2 / 3, linetype = "dashed")
#'
#' # Multiple distributions are auto-coloured
#' ggplot() +
#'   geom_pdf(dist = c(dist_normal(0, 1), dist_student_t(df = 3)))
#'
#' # Bivariate example
#' biv <- dist_multivariate_normal(
#'   mu    = list(c(0, 0)),
#'   sigma = list(matrix(c(1, 0.6, 0.6, 1), nrow = 2))
#' )
#' ggplot() +
#'   geom_pdf_2d(dist = biv)
#' @export
geom_pdf <- function(
  mapping = NULL,
  data = NULL,
  dist,
  scale = 1,
  ngrid = 501,
  density_df = NULL,
  geom = "line",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (missing(dist)) {
    stop("`dist` must be supplied (a distributional object).", call. = FALSE)
  }

  if (is.null(mapping) && length(dist) > 1L) {
    mapping <- ggplot2::aes(colour = ggplot2::after_stat(distribution))
  }

  ggplot2::layer(
    data = data %||% data.frame(.pdf_dummy = NA_real_),
    mapping = mapping,
    stat = StatPdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      dist = dist,
      density_df = density_df,
      scale = scale,
      ngrid = ngrid,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_pdf
#' @format NULL
#' @usage NULL
#' @export
StatPdf <- ggplot2::ggproto(
  "StatPdf",
  ggplot2::Stat,

  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(x),
    y = ggplot2::after_stat(density)
  ),

  required_aes = character(),

  extra_params = c("na.rm", "dist", "density_df", "scale", "ngrid"),

  compute_layer = function(self, data, params, layout) {
    object <- params$dist
    ngrid <- params$ngrid %||% 501
    scale <- params$scale %||% 1
    density_df <- params$density_df

    if (is.null(object)) {
      stop(
        "`dist` is NULL inside StatPdf$compute_layer(); this should not happen.",
        call. = FALSE
      )
    }

    d <- dimension_dist(object)
    if (any(d != 1)) {
      stop(
        "`geom_pdf()` only supports univariate distributions. ",
        "Use `geom_pdf_2d()` for bivariate distributions.",
        call. = FALSE
      )
    }

    df <- density_df %||% make_density_df(object, ngrid = ngrid)

    base <- data.frame(
      x = df$x,
      density = scale * df$Density,
      distribution = as.character(df$Distribution),
      stringsAsFactors = FALSE
    )
    base$group <- as.integer(factor(base$distribution))

    panels <- unique(layout$layout$PANEL)
    if (length(panels) == 0L) {
      panels <- 1L
    }
    do.call(
      rbind,
      lapply(panels, function(p) {
        out <- base
        out$PANEL <- p
        out
      })
    )
  }
)


# ---------------------------------------------------------------------------
# Bivariate variant
# ---------------------------------------------------------------------------

#' @rdname geom_pdf
#' @param prob Coverage probabilities for the highest density regions used as
#'   contour breaks. Defaults to `seq(0.1, 0.9, by = 0.1)`. Ignored when
#'   `thresholds` is supplied.
#' @param filled Logical. If `TRUE`, draw filled HDR bands using
#'   [ggplot2::geom_contour_filled()]. If `FALSE` (default), draw unfilled contour lines
#'   using [ggplot2::geom_contour()].
#' @param thresholds Optional numeric vector of HDR density thresholds (the
#'   `density` column of an `hdr_table()` result). When supplied, the internal
#'   `hdr_table()` call is skipped. The conversion from raw thresholds to
#'   contour `breaks` (with or without the leading `Inf`) is handled
#'   internally based on `filled`.
#' @export
geom_pdf_2d <- function(
  mapping = NULL,
  data = NULL,
  dist,
  ngrid = 101,
  prob = seq(0.1, 0.9, by = 0.1),
  filled = FALSE,
  density_df = NULL,
  thresholds = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (missing(dist)) {
    stop("`dist` must be supplied (a distributional object).", call. = FALSE)
  }
  if (!is.logical(filled) || length(filled) != 1L || is.na(filled)) {
    stop("`filled` must be a single TRUE or FALSE.", call. = FALSE)
  }

  d <- dimension_dist(dist)
  if (any(d != 2)) {
    stop(
      "`geom_pdf_2d()` only supports bivariate distributions.",
      call. = FALSE
    )
  }

  df <- density_df %||% make_density_df(dist, ngrid = ngrid)

  if (is.null(thresholds)) {
    thresholds <- hdr_table_with_data(dist, prob = prob, df)$density
  }

  default_mapping <- ggplot2::aes(x = x, y = y, z = Density)

  if (filled) {
    # Descending order matters: see comment in v6 for the explanation. With
    # breaks = c(Inf, t_high, ..., t_low) the adjacent-pair bands cover the
    # peak (Inf, t_high] and then each successive HDR ring inward to outward.
    breaks_filled <- c(Inf, sort(unique(thresholds), decreasing = TRUE))
    ggplot2::geom_contour_filled(
      data = df,
      mapping = mapping %||% default_mapping,
      breaks = breaks_filled,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  } else {
    breaks_lines <- sort(unique(thresholds))
    ggplot2::geom_contour(
      data = df,
      mapping = mapping %||% default_mapping,
      breaks = breaks_lines,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  }
}
