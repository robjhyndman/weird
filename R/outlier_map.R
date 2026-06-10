#' Outlier map from a projection or principal component analysis
#'
#' @description Draw an outlier map showing the score distance and orthogonal
#' distance of each observation from a projection or principal component
#' analysis. The score distance measures how far an observation lies from the
#' centre *within* the projection subspace, while the orthogonal distance
#' measures how far it lies *from* the subspace. Pass `object` (the output of
#' [stats::prcomp()] or an rrcov `Pca*` function); otherwise supply `scores` and
#' `loadings` together with the original `data`. For a `prcomp` object, use its
#' `rank.` argument to set the number of retained components.
#'
#' When `object` is a PCA-like object and `show_thresholds = TRUE`, the
#' score-distance and orthogonal-distance cutoffs are drawn as dashed lines and
#' observations are coloured by type:
#' \describe{
#'   \item{Regular observation}{small score and orthogonal distance.}
#'   \item{Good leverage point}{large score distance, small orthogonal distance.}
#'   \item{Orthogonal outlier}{small score distance, large orthogonal distance.}
#'   \item{Bad leverage point}{large score and orthogonal distance.}
#' }
#' The cutoffs are only defined for PCA-like objects, so `show_thresholds` is
#' ignored when `scores` and `loadings` are passed directly.
#'
#' @param object Optionally, the output of [stats::prcomp()] or an rrcov `Pca*`
#' function. For a `prcomp` object, set the number of retained components with
#' its `rank.` argument. If supplied, the `scores` and `loadings` arguments are
#' ignored.
#' @param data The original data matrix or data frame used to compute the
#' projection, scaled if the projection was computed on scaled data. This is
#' required to compute the orthogonal distances, except when `object` is a
#' rrcov `Pca*` object (which stores them).
#' @param scores A matrix or data frame of scores, with one column per retained
#' component. Ignored if `object` is supplied.
#' @param loadings A matrix or data frame of loadings, with one column per
#' retained component. Ignored if `object` is supplied.
#' @param show_thresholds If `TRUE` (the default) and `object` is a PCA-like
#' object, the score-distance and orthogonal-distance cutoffs are drawn as
#' dashed lines and observations are coloured by type. Ignored when `scores`
#' and `loadings` are passed directly.
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#' @return A `ggplot` object.
#' @author Rob J Hyndman
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R",
#' Chapter 9, \url{https://OTexts.com/weird/}.
#' @examples
#' Y <- oldfaithful[, c("duration", "waiting")]
#' prcomp(Y, scale = TRUE, rank. = 1) |>
#'   outlier_map(data = Y)
#' @importFrom ggplot2 geom_vline geom_hline scale_colour_manual guides guide_legend theme
#' @importFrom stats qchisq qnorm mad median var
#' @export
outlier_map <- function(
  object = NULL,
  data = NULL,
  scores = NULL,
  loadings = NULL,
  show_thresholds = TRUE,
  ...
) {
  pca_like <- !is.null(object)
  cutoff_sd <- cutoff_od <- NULL
  if (pca_like) {
    if (inherits(object, "prcomp")) {
      k <- ncol(object$rotation)
      d <- prcomp_distances(object, data, k)
      cutoff_sd <- sqrt(qchisq(0.975, df = k))
      cutoff_od <- od_cutoff(d$od)
    } else if (inherits(object, "Pca")) {
      k <- object$k
      aug <- augment(object)
      d <- list(sd = aug$.sd, od = aug$.od)
      cutoff_sd <- object$cutoff.sd
      cutoff_od <- object$cutoff.od
    } else {
      stop("`object` must be the output of prcomp() or an rrcov Pca* function")
    }
  } else {
    if (is.null(scores) || is.null(loadings)) {
      stop("Supply `object`, or both `scores` and `loadings`.")
    }
    scores <- as.matrix(scores)
    k <- ncol(scores)
    d <- projection_distances(scores, as.matrix(loadings), data)
  }

  df <- tibble(sd = d$sd, od = d$od)
  show <- isTRUE(show_thresholds) && pca_like
  title <- if (pca_like) {
    paste0("PCA with ", k, " component", if (k == 1) "" else "s")
  } else {
    NULL
  }

  if (show) {
    sd_hi <- df$sd > cutoff_sd
    od_hi <- df$od > cutoff_od
    df$type <- factor(
      dplyr::case_when(
        !sd_hi & !od_hi ~ "Regular",
        sd_hi & !od_hi ~ "Good leverage point",
        !sd_hi & od_hi ~ "Orthogonal outlier",
        TRUE ~ "Bad leverage point"
      ),
      levels = c(
        "Orthogonal outlier",
        "Regular",
        "Bad leverage point",
        "Good leverage point"
      )
    )
    p <- ggplot(df, aes(x = sd, y = od, colour = type)) +
      geom_point(...) +
      geom_vline(
        xintercept = cutoff_sd,
        linetype = "dashed",
        colour = "grey50"
      ) +
      geom_hline(
        yintercept = cutoff_od,
        linetype = "dashed",
        colour = "grey50"
      ) +
      scale_colour_manual(
        values = c(
          "Regular" = "#0072B2",
          "Good leverage point" = "#009E73",
          "Orthogonal outlier" = "#D55E00",
          "Bad leverage point" = "black"
        )
      ) +
      guides(colour = guide_legend(nrow = 2)) +
      theme(legend.position = "bottom")
  } else {
    dots <- list(...)
    if (is.null(dots$colour) && is.null(dots$color)) {
      dots$colour <- "#0072B2"
    }
    p <- ggplot(df, aes(x = sd, y = od)) +
      do.call(geom_point, dots)
  }
  p +
    labs(x = "Score distance", y = "Orthogonal distance", title = title)
}

# Score and orthogonal distances from a prcomp object. The orthogonal distance
# is the residual length after reconstructing the (centred and scaled) data from
# the k retained components, so the original data is required.
prcomp_distances <- function(object, data, k) {
  if (is.null(data)) {
    stop(
      "`data` is required to compute orthogonal distances for a prcomp object."
    )
  }
  cols <- seq_len(k)
  scores <- object$x[, cols, drop = FALSE]
  sdev <- object$sdev[cols]
  sd <- sqrt(rowSums(sweep(scores^2, 2, sdev^2, "/")))
  z <- as.matrix(data)
  if (!isFALSE(object$center)) {
    z <- sweep(z, 2, object$center)
  }
  if (!isFALSE(object$scale)) {
    z <- sweep(z, 2, object$scale, "/")
  }
  resid <- z - scores %*% t(object$rotation[, cols, drop = FALSE])
  list(sd = sd, od = sqrt(rowSums(resid^2)))
}

# Score and orthogonal distances from scores and loadings supplied directly. The
# score variances act as the eigenvalues, and the orthogonal distance is the
# residual length of the centred data after reconstruction.
projection_distances <- function(scores, loadings, data) {
  if (is.null(data)) {
    stop("`data` is required to compute orthogonal distances.")
  }
  eig <- apply(scores, 2, var)
  sd <- sqrt(rowSums(sweep(scores^2, 2, eig, "/")))
  z <- sweep(as.matrix(data), 2, colMeans(data))
  resid <- z - scores %*% t(loadings)
  list(sd = sd, od = sqrt(rowSums(resid^2)))
}

# Orthogonal-distance cutoff using the normal approximation of Hubert, Rousseeuw
# and Vanden Branden (2005): od^(2/3) is approximately normal.
od_cutoff <- function(od) {
  z <- od^(2 / 3)
  (median(z) + mad(z) * qnorm(0.975))^(3 / 2)
}

utils::globalVariables(c("sd", "od"))
