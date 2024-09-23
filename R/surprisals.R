#' @title Surprisals

#' @description Compute surprisals or surprisal probabilities from a model or a
#' data set. A surprisal is given by \eqn{s = -\log f(y)} where \eqn{f} is the
#' density or probability mass function of the estimated or assumed distribution,
#' and \eqn{y} is an observation. A surprisal probability is the probability of
#' a surprisal at least as extreme as \eqn{s}.
#'
#' The surprisal probabilities may be computed in three different ways.
#' 1. Given the same distribution that was used to compute the surprisal values.
#' Under this option, surprisal probabilities are equal to 1 minus the
#' coverage probability of the largest HDR that contains each value. Surprisal
#' probabilities smaller than 1e-6 are returned as 1e-6.
#' 2. Using a Generalized Pareto Distribution fitted to the most extreme
#' surprisal values (those with probability less than `threshold_probability`).
#' This option is used if `approximation = "gpd"`. For surprisal values with
#' probability less than `threshold_probability`, the value of
#' `threshold_probability` is returned. Under this option, the distribution is
#' used for computing the surprisal values but not for determining their
#' probabilities. Due to extreme value theory, the resulting probabilities should
#' be relatively insensitive to the distribution used in computing the surprisal
#' values.
#' 3. Empirically as the proportion of observations with greater surprisal values.
#' This option is used when `approxiation = "empirical"`. This is also
#' insensitive to the distribution used in computing the surprisal values.
#' @param object A model or numerical data set
#' @param probability Should surprisal probabilities be computed, or the
#' surprisal values?
#' @param approximation Character string specifying the approximation to use in
#' computing the surprisal probabilities. Ignored if `probability = FALSE`. :
#' `none` specifies that no approximation is to be used;
#' `gpd` specifies that  the Generalized Pareto distribution should be used;
#' while `empirical` specifies that the probabilities should be estimated empirically.
#' @param threshold_probability Probability threshold when computing the GPD
#' approximation. This is the probability below which the GPD is fitted. Only
#' used if `approximation = "gpd"`).
#' @param ... Other arguments are passed to the appropriate method.
#' @author Rob J Hyndman
#' @return A numerical vector containing the surprisals or surprisal probabilities.
#' @examples
#' # surprisals computed from bivariate data set
#' oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   mutate(
#'     loo_fscores = surprisals(cbind(duration, waiting), loo = TRUE)
#'   )
#' @export

surprisals <- function(
    object,
    probability = TRUE,
    approximation = c("none", "gpd", "empirical"),
    threshold_probability = 0.10,
    ...) {
  UseMethod("surprisals")
}


#' @rdname surprisals
#' @details If no distribution is provided, a kernel density estimate is
#' computed. The leave-one-out surprisals (or LOO surprisals) are obtained by
#' estimating the kernel density estimate using all other observations.
#'
#' @param distribution A distribution object. If not provided, a kernel density
#' estimate is computed from the data `object`.
#' @param loo Should leave-one-out surprisals be computed?
#' @seealso \code{\link{dist_kde}}
#' @examples
#' # Univariate data
#' tibble(
#'   y = c(5, rnorm(49)),
#'   p = surprisals(y)
#' )
#' tibble(
#'   y = n01$v1,
#'   prob1 = surprisals(y),
#'   prob2 = surprisals(y, approximation = "gpd"),
#'   prob3 = surprisals(y, distribution = dist_normal()),
#'   prob4 = surprisals(y, distribution = dist_normal(), approximation = "gpd")
#' ) |>
#'   arrange(prob1)
#' # Bivariate data
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   prob = surprisals(cbind(x, y)),
#'   lookout = lookout_prob(cbind(x, y))
#' )
#' @export
surprisals.default <- function(
    object,
    probability = TRUE,
    approximation = c("none", "gpd", "empirical"),
    threshold_probability = 0.10,
    distribution = dist_kde(object, multiplier = 2, ...),
    loo = FALSE,
    ...) {
  object <- as.matrix(object)
  if (NCOL(object) == 1L) {
    object <- c(object)
  }
  den <- density(distribution, at = object, log = TRUE)
  if (is.list(den)) {
    den <- den[[1]]
  }
  scores <- -den
  if (loo & stats::family(distribution) == "kde") {
    n <- NROW(object)
    d <- NCOL(object)
    if (d == 1L) {
      h <- vctrs::vec_data(distribution)[[1]]$kde$h
      K0 <- 1 / (h * sqrt(2 * pi))
    } else {
      H <- vctrs::vec_data(distribution)[[1]]$kde$H
      K0 <- det(H)^(-1 / 2) * (2 * pi)^(-d / 2)
    }
    scores <- -log(pmax(0, (n * exp(-scores) - K0) / (n - 1)))
  }
  if (probability) {
    surprisal_prob(scores, distribution, approximation, threshold_probability) |>
      suppressWarnings()
  } else {
    scores
  }
}

#' @rdname surprisals_model
#' @inheritParams surprisals
#' @title Surprisals computed from a model
#' @param object A model object such as returned by \code{\link[stats]{lm}},
#' or \code{\link[mgcv]{gam}}.
#' @param loo Should leave-one-out surprisals be computed?
#' @param ... Other arguments are ignored.
#' @examples
#' # surprisals computed from linear model
#' of <- oldfaithful |>
#'   filter(duration < 7200, waiting < 7200)
#' lm_of <- lm(waiting ~ duration, data = of)
#' of |>
#'   mutate(
#'     fscore = surprisals(lm_of),
#'     loo_fscore = surprisals(lm_of, loo = TRUE),
#'     # lookout_prob = lookout(surprisals = fscore, loo_scores = loo_fscore)
#'   ) |>
#'   ggplot(aes(
#'     x = duration, y = waiting,
#'     color = loo_fscore > quantile(loo_fscore, 0.99)
#'   )) +
#'   geom_point()
#' @export
surprisals.lm <- function(
    object,
    probability = TRUE,
    approximation = c("none", "gpd", "empirical"),
    threshold_probability = 0.10,
    loo = FALSE, ...) {
  e <- stats::residuals(object, type = "response")
  h <- stats::hatvalues(object)
  sigma2 <- sum(e^2, na.rm = TRUE) / object$df.residual
  if (loo) {
    resdf <- object$df.residual
    sigma2 <- (sigma2 * resdf - e^2 / (1 - h)) / (resdf - 1)
  }
  r2 <- e^2 / ((1 - h) * sigma2)
  s <- 0.5 * (log(2 * pi) + r2)
  if (probability) {
    surprisal_prob(s, distributional::dist_normal(), approximation, threshold_probability)
  } else {
    s
  }
}

#' @rdname surprisals_model
#' @examples
#' # surprisals computed from GAM
#' of <- oldfaithful |>
#'   filter(duration > 1, duration < 7200, waiting < 7200)
#' gam_of <- mgcv::gam(waiting ~ s(duration), data = of)
#' of |>
#'   mutate(fscore = surprisals(gam_of))
#' @importFrom stats approx dbinom density dnorm dpois na.omit
#' @export
surprisals.gam <- function(
    object,
    probability = TRUE,
    approximation = c("none", "gpd", "empirical"),
    threshold_probability = 0.10,
    loo = FALSE, ...) {
  if (loo) {
    warning("Leave-one-out log scores unavailable for GAM models.")
  }
  fit_aug <- broom::augment(object, type.predict = "response")
  if (object$family$family == "gaussian") {
    std.resid <- c(scale(fit_aug$.resid / fit_aug$.se.fit))
    surprisals <- -dnorm(std.resid, log = TRUE)
    dist <- distributional::dist_normal()
  } else if (object$family$family == "binomial") {
    surprisals <- -dbinom(
      x = object$y * object$prior.weights,
      size = object$prior.weights, prob = fit_aug$.fitted, log = TRUE
    )
    dist <- distributional::dist_binomial(object$prior.weights, fit_aug$.fitted)
  } else if (object$family$family == "poisson") {
    surprisals <- -dpois(object$y, lambda = fit_aug$.fitted, log = TRUE)
    dist <- distributional::dist_poisson(lambda = fit_aug$.fitted)
  } else {
    stop("Unsupported family")
  }
  if (probability) {
    surprisal_prob(surprisals, dist, approximation, threshold_probability)
  } else {
    surprisals
  }
}

utils::globalVariables(c(
  ".resid", ".se.fit", ".std.resid", ".resid", ".sigma", ".hat",
  "studentized_residuals"
))
