#' @title Density scores
#' @description Compute density scores or leave-one-out density scores from a
#' model or a kernel density estimate of a data set.
#' The density scores are defined as minus the log of the conditional density,
#' or kernel density estimate, at each observation.
#' The leave-one-out density scores (or LOO density scores) are obtained by
#' estimating the conditional density or kernel density estimate using all
#' other observations.
#' @details If the first argument is a numerical vector or matrix, then
#' a kernel density estimate is computed, using a Gaussian kernel,
#' with default bandwidth given by a robust normal reference rule.
#' Otherwise the model is used to compute the conditional
#' density function at each observation, from which the density scores (or
#' possibly the LOO density scores) are obtained.
#' @param object A model object or a numerical data set.
#' @param loo Should leave-one-out density scores be computed?
#' @author Rob J Hyndman
#' @return A numerical vector containing either the density scores, or the LOO
#' density scores.
#' @seealso
#'  \code{\link{kde_bandwidth}}
#'  \code{\link[ks]{kde}}
#' @export

density_scores <- function(object, loo = FALSE, ...) {
  UseMethod("density_scores")
}

#' @rdname density_scores
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @examples
#' # Density scores computed from bivariate data set
#' of <- oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   mutate(
#'     fscores = density_scores(cbind(duration, waiting)),
#'     loo_fscores = density_scores(cbind(duration, waiting), loo = TRUE),
#'     lookout_prob = lookout(density_scores = fscores, loo_scores = loo_fscores)
#'   )
#' of |>
#'   ggplot(aes(x = duration, y = waiting, color = lookout_prob < 0.01)) +
#'   geom_point()
#' @export
density_scores.default <- function(
    object, loo = FALSE,
    h = kde_bandwidth(object, method = "double"),
    H = kde_bandwidth(object, method = "double"), ...) {
  object <- as.matrix(object)
  tmp <- calc_kde_scores(object, h, H, ...)
  if (loo) {
    return(tmp$loo_scores)
  } else {
    return(tmp$scores)
  }
}

#' @rdname density_scores
#' @param ... Other arguments are ignored.
#' @examples
#' # Density scores computed from bivariate KDE
#' f_kde <- kde(of[, 2:3], H = kde_bandwidth(of[, 2:3]))
#' of |>
#'   mutate(
#'     fscores = density_scores(f_kde),
#'     loo_fscores = density_scores(f_kde, loo = TRUE)
#'   )
#' @export
density_scores.kde <- function(object, loo = FALSE, ...) {
  n <- NROW(object$x)
  d <- NCOL(object$x)
  # kde on a grid, but we need it at observations, so we will re-estimate
  # interpolation is probably quicker, but less accurate and
  # this works ok.
  output <- calc_kde_scores(object$x, object$h, object$H, ...)
  if (loo) {
    return(output$loo_scores)
  } else {
    return(output$scores)
  }
}

#' @rdname density_scores
#' @examples
#' # Density scores computed from linear model
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' lm_wine <- lm(log(price) ~ points, data = shiraz)
#' shiraz |>
#'   mutate(
#'     fscore = density_scores(lm_wine),
#'     loo_fscore = density_scores(lm_wine, loo = TRUE),
#'     lookout_prob = lookout(density_scores = fscore, loo_scores = loo_fscore)
#'   ) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @export
density_scores.lm <- function(object, loo = FALSE, ...) {
  e <- stats::residuals(object, type = "response")
  h <- stats::hatvalues(object)
  sigma2 <- sum(e^2, na.rm = TRUE) / object$df.residual
  if (loo) {
    resdf <- object$df.residual
    sigma2 <- (sigma2 * resdf - e^2 / (1 - h)) / (resdf - 1)
  }
  r2 <- e^2 / ((1 - h) * sigma2)
  return(0.5 * (log(2 * pi) + r2))
}


#' @rdname density_scores
#' @examples
#' # Density scores computed from GAM
#' gam_wine <- mgcv::gam(log(price) ~ s(points), data = shiraz)
#' shiraz |>
#'   mutate(
#'     fscore = density_scores(gam_wine),
#'     lookout_prob = lookout(density_scores = fscore)
#'   ) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @importFrom stats approx dbinom density dnorm dpois na.omit
#' @export
density_scores.gam <- function(object, loo = FALSE, ...) {
  if (loo) {
    warning("Leave-one-out log scores unavailable for GAM models. Returning log scores.")
  }
  fit_aug <- broom::augment(object, type.predict = "response")
  if (object$family$family == "gaussian") {
    std.resid <- c(scale(fit_aug$.resid / fit_aug$.se.fit))
    density_scores <- -dnorm(std.resid, log = TRUE)
  } else if (object$family$family == "binomial") {
    density_scores <- -dbinom(
      x = object$y * object$prior.weights,
      size = object$prior.weights, prob = fit_aug$.fitted, log = TRUE
    )
  } else if (object$family$family == "poisson") {
    density_scores <- -dpois(object$y, lambda = fit_aug$.fitted, log = TRUE)
  } else {
    stop("Unsupported family")
  }
  return(density_scores)
}

# Compute value of density at each observation using kde
calc_kde_scores <- function(
    y,
    h = kde_bandwidth(y, method = "double"),
    H = kde_bandwidth(y, method = "double"), ...) {
  n <- NROW(y)
  d <- NCOL(y)
  # Estimate density at each observation
  if (d == 1L) {
    gridsize <- 10001
    K0 <- 1 / (h * sqrt(2 * pi))
    fi <- ks::kde(y,
      h = h, gridsize = gridsize, binned = n > 2000,
      eval.points = y, compute.cont = FALSE, ...
    )$estimate
  } else {
    gridsize <- 101
    K0 <- det(H)^(-1 / 2) * (2 * pi)^(-d / 2)
    fi <- ks::kde(y,
      H = H, gridsize = gridsize, binned = n > 2000,
      eval.points = y, compute.cont = FALSE, ...
    )$estimate
  }
  loo_scores <- -log(pmax(0, (n * fi - K0) / (n - 1)))
  scores <- -log(pmax(0, fi))
  return(list(scores = scores, loo_scores = loo_scores))
}

utils::globalVariables(c(
  ".resid", ".se.fit", ".std.resid", ".resid", ".sigma", ".hat",
  "studentized_residuals"
))
