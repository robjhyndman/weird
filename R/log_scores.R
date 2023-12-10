#' @title Log scores
#' @description Compute log scores or leave-one-out log scores from a model or
#'  a kernel density estimate of a data set.
#'  The log scores are defined as minus the log of the conditional density,
#'  or kernel density estimate, at each observation.
#'  The leave-one-out log scores (or log LOO scores) are obtained by estimating the conditional density
#'  or kernel density estimate using all other observations.
#' @details If the first argument is a numerical vector or matrix, then
#' a kernel density estimate is computed, using a Gaussian kernel,
#' with default bandwidth given by a robust normal reference rule.
#' Otherwise the model is used to compute the conditional
#' density function at each observation, from which the log scores (or
#' possibly the log LOO scores) are obtained.
#' @param object A model object or a numerical data set.
#' @param loo Should leave-one-out log scores be computed?
#' @author Rob J Hyndman
#' @return A numerical vector containing either the log scores, or the log LOO scores.
#' @export

log_scores <- function(object, loo = FALSE, ...) {
  UseMethod("log_scores")
}

#' @rdname log_scores
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @examples
#' of <- oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   mutate(
#'     logscores = log_scores(cbind(duration, waiting)),
#'     loglooscores = log_scores(cbind(duration, waiting), loo = TRUE),
#'     lookout_prob = lookout(logscores, loglooscores)
#'   )
#' of |>
#'   ggplot(aes(x = duration, y = waiting, color = lookout_prob < 0.01)) +
#'   geom_point()
#' @seealso
#'  \code{\link{kde_bandwidth}}
#'  \code{\link[ks]{kde}}
#' @export
log_scores.default <- function(object, loo = FALSE,
                               h = kde_bandwidth(object), H = kde_bandwidth(object),
                               ...) {
  object <- as.matrix(object)
  tmp <- calc_kde_scores(object, h, H, ...)
  if (loo) {
    return(tmp$loo_scores)
  } else {
    return(tmp$scores)
  }
}

#' @rdname log_scores
#' @param ... Other arguments are ignored.
#' @examples
#' of <- oldfaithful |>
#'   filter(duration < 7000, waiting < 7000)
#' f_kde <- kde(of[,2:3], H = kde_bandwidth(of[,2:3]))
#' of |>
#'   mutate(
#'     logscores = log_scores(f_kde),
#'     loglooscores = log_scores(f_kde, loo = TRUE),
#'     lookout_prob = lookout(logscores, loglooscores)
#'   ) |>
#'   ggplot(aes(x = duration, y = waiting, color = lookout_prob < 0.01)) +
#'   geom_point()
#' @seealso
#'  \code{\link{kde_bandwidth}}
#'  \code{\link[ks]{kde}}
#' @export
log_scores.kde <- function(object, loo = FALSE, ...) {
  n <- NROW(object$x)
  d <- NCOL(object$x)
  # Estimate density at each observation using interpolation
  if(d == 1L) {
    if(loo) {
      K0 = 1/(object$h * sqrt(2 * pi))
    }
    fi <- approx(object$eval.points, object$estimate, object$x)$y
  } else {
    if(loo) {
      K0 = det(object$H)^(-1/2) * (2*pi)^(-d/2)
    }
    xy <- expand.grid(object$eval.points[[1]], object$eval.points[[2]])
    fi <- interp::interp(
      x = xy[,1], y = xy[,2], z = object$estimate,
      xo = object$x[[1]], yo = object$x[[2]],
      output = "points"
    )$z
  }
  if(loo) {
    return(-log(pmax(0, (n * fi - K0) / (n - 1))))
  } else {
    return(-log(pmax(0, fi)))
  }
}

#' @rdname log_scores
#' @examples
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- lm(log(price) ~ points, data = shiraz)
#' shiraz |>
#'   mutate(
#'     logscore = log_scores(fit_wine),
#'     loglooscore = log_scores(fit_wine, loo = TRUE),
#'     lookout_prob = lookout(logscore, loglooscore)
#'   ) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @export
log_scores.lm <- function(object, loo = FALSE, ...) {
  fit_aug <- broom::augment(object)
  if(loo) {
    fit_aug$.std.resid <- fit_aug$.resid / (fit_aug$.sigma * sqrt(1 - fit_aug$.hat))
  }
  return(-dnorm(fit_aug$.std.resid, log = TRUE))
}

#' @rdname log_scores
#' @examples
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- mgcv::gam(log(price) ~ s(points), data = shiraz)
#' shiraz |>
#'   mutate(
#'     logscore = log_scores(fit_wine),
#'     lookout_prob = lookout(logscore)
#'   ) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @importFrom stats approx dbinom density dnorm dpois na.omit
#' @export
log_scores.gam <- function(object, loo = FALSE, ...) {
  if(loo) {
    warning("Leave-one-out scores unavailable for GAM models")
  }
  fit_aug <- broom::augment(object, type.predict = "response")
  if(object$family$family == "gaussian") {
    std.resid <- c(scale(fit_aug$.resid / fit_aug$.se.fit))
    log_scores <- -dnorm(std.resid, log = TRUE)
  } else if(object$family$family == "binomial") {
    log_scores <- -dbinom(x = object$y*object$prior.weights,
                          size = object$prior.weights, prob = fit_aug$.fitted, log = TRUE)
  } else if(object$family$family == "poisson") {
    log_scores <- -dpois(object$y, lambda = fit_aug$.fitted, log = TRUE)
  } else {
    stop("Unsupported family")
  }
  return(log_scores)
}

utils::globalVariables(c(".resid",".se.fit",".std.resid",".resid",".sigma",".hat",
                         "studentized_residuals"))

