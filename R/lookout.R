#' @title Lookout probabilities
#' @description Compute leave-one-out log score probabilities using a
#' Generalized Pareto distribution. These give the probability of each observation
#' being an anomaly.
#' @details If the first argument is a numerical vector or matrix, then
#' a kernel density estimate is computed and the probabilities are computed
#' from the leave-one-out kde scores. Otherwise the model is used to compute
#' residuals, and the probabilities are computed from the leave-one-out residuals.
#' @param object A model object or a numerical data set.
#' @param threshold_probability Probability threshold when computing the POT model for the log scores.
#' When `object` is a numerical data set, this value is only used if there are more than 100 observations.
#' If there are fewer than 25 observations, no POT model is computed and a warning is given.
#' For between 25 and 100 observations, the threshold probability is set to
#' \code{1 - 5/NROW(y)}.
#' @return A numerical vector containing the lookout probabilities
#' @export

lookout <- function(object, threshold_probability = 0.95, ...) {
  UseMethod("lookout")
}

#' @rdname lookout
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @examples
#' of <- oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   mutate(lookout_prob = lookout(cbind(duration, waiting)))
#' of |>
#'   ggplot(aes(x = duration, y = waiting, color = lookout_prob < 0.01)) +
#'   geom_point()
#' @export
lookout.default <- function(object,
    threshold_probability = min(0.95, 1 - 5/NROW(object)),
    h = kde_bandwidth(object), H = kde_bandwidth(object),
    ...) {
  tmp <- calc_kde_scores(object, h, H, ...)
  loo_scores <- tmp$loo_scores
  n <- NROW(object)
  if(n < 25) {
    warning("Insufficient data to compute lookout probabilities")
    lookout_probabilities <- rep(1, n)
  } else {
    if(n < 100) {
      threshold_probability <- 1 - 5/n
    }
    threshold <- stats::quantile(tmp$scores, prob = threshold_probability, type = 8)
    gpd <- evd::fpot(tmp$scores, threshold = threshold, std.err = FALSE)$estimate
    lookout_probabilities <- evd::pgpd(
      loo_scores, loc = threshold,
      scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
    )
  }
  return(lookout_probabilities)
}


#' @rdname lookout
#' @examples
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- lm(log(price) ~ points, data = shiraz)
#' shiraz |>
#'   mutate(lookout_prob = lookout(fit_wine)) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @export
lookout.lm <- function(object, threshold_probability = 0.95, ...) {
  fit_aug <- broom::augment(object) |>
    dplyr::mutate(
      studentized_residuals = .resid / (.sigma * sqrt(1 - .hat)),
      log_scores = -dnorm(.std.resid, log = TRUE),
      log_loo_scores = -dnorm(studentized_residuals, log = TRUE)
    )
  threshold <- quantile(fit_aug$log_scores, prob = 0.95, type = 8)
  gpd <- evd::fpot(fit_aug$log_scores, threshold = threshold)$estimate
  pval <- evd::pgpd(fit_aug$log_loo_scores, loc = threshold,
     scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE)
  return(pval)
}

#' @rdname lookout
#' @examples
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- mgcv::gam(log(price) ~ s(points), data = shiraz)
#' shiraz |>
#'   mutate(lookout_prob = lookout(fit_wine)) |>
#'   ggplot(aes(x = points, y = price, color = lookout_prob < 0.02)) +
#'   geom_jitter(height = 0, width = 0.2) +
#'   scale_y_log10()
#' @export
lookout.gam <- function(object, threshold_probability = 0.95, ...) {
  fit_aug <- broom::augment(object) |>
    dplyr::mutate(
      .std.resid = .resid / .se.fit,
      log_scores = -dnorm(.std.resid, log = TRUE),
    )
  threshold <- quantile(fit_aug$log_scores, prob = 0.95, type = 8)
  gpd <- evd::fpot(fit_aug$log_scores, threshold = threshold)$estimate
  pval <- evd::pgpd(fit_aug$log_scores, loc = threshold,
                    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE)
  return(pval)
}

utils::globalVariables(c(".resid",".se.fit",".std.resid",".resid",".sigma",".hat",
                         "studentized_residuals"))

