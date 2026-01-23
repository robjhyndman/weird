#' @rdname surprisals_model
#' @inherit surprisals
#' @title Surprisals and surprisal probabilities computed from a model
#' @param object A model object such as returned by \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, or \code{\link[mgcv]{gam}}.
#' This includes a specified conditional probability
#' distribution which is used to compute surprisal values.
#' @param loo Should leave-one-out surprisals be computed? For computational
#' reasons, this is only available for `lm` objects.
#' @param ... Other arguments are ignored.
#' @examples
#' # A linear model (i.e., a conditional Gaussian distribution)
#' lm_of <- lm(waiting ~ duration, data = oldfaithful)
#' oldfaithful |>
#'   mutate(
#'     fscore = surprisals_prob(lm_of),
#'     prob = surprisals_prob(lm_of, loo = TRUE),
#'   ) |>
#'   ggplot(aes(
#'     x = duration, y = waiting,
#'     color = prob < 0.01
#'   )) +
#'   geom_point()
#' # A Poisson GLM
#' glm_breaks <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)
#' warpbreaks |>
#'   mutate(prob = surprisals_prob(glm_breaks)) |>
#'   filter(prob < 0.05)
#' @export
surprisals.lm <- function(object, loo = FALSE, ...) {
  e <- stats::residuals(object, type = "response")
  h <- stats::hatvalues(object)
  sigma2 <- sum(e^2, na.rm = TRUE) / object$df.residual
  if (loo) {
    if (inherits(object, "glm")) {
      stop("LOO surprisals are not implemented for glm objects.")
    } else {
      resdf <- object$df.residual
      sigma2 <- (sigma2 * resdf - e^2 / (1 - h)) / (resdf - 1)
    }
  }
  r2 <- e^2 / ((1 - h) * sigma2)
  0.5 * (log(2 * pi) + r2)
}

#' @rdname surprisals_model
#' @export
surprisals_prob.lm <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  loo = FALSE,
  ...
) {
  approximation <- match.arg(approximation)
  s <- surprisals.lm(object, loo = loo)
  surprisal_prob_from_s(
    s,
    distribution = distributional::dist_normal(),
    approximation = approximation,
    threshold_probability = threshold_probability
  )
}

#' @rdname surprisals_model
#' @importFrom stats approx dbinom density dnorm dpois na.omit
#' @export
surprisals.gam <- function(object, ...) {
  fit_aug <- broom::augment(object, type.predict = "response")
  if (object$family$family == "gaussian") {
    std.resid <- c(scale(fit_aug$.resid / fit_aug$.se.fit))
    surprisals <- -dnorm(std.resid, log = TRUE)
  } else if (object$family$family == "binomial") {
    surprisals <- -dbinom(
      x = object$y * object$prior.weights,
      size = object$prior.weights,
      prob = fit_aug$.fitted,
      log = TRUE
    )
  } else if (object$family$family == "poisson") {
    surprisals <- -dpois(object$y, lambda = fit_aug$.fitted, log = TRUE)
  } else {
    stop("Unsupported family")
  }
  return(surprisals)
}

#' @rdname surprisals_model
#' @export
surprisals_prob.gam <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  ...
) {
  approximation <- match.arg(approximation)
  if (object$family$family == "gaussian") {
    dist <- distributional::dist_normal()
  } else {
    fit_aug <- broom::augment(object, type.predict = "response")
    if (object$family$family == "binomial") {
      dist <- distributional::dist_binomial(
        object$prior.weights,
        fit_aug$.fitted
      )
    } else if (object$family$family == "poisson") {
      dist <- distributional::dist_poisson(lambda = fit_aug$.fitted)
    } else {
      stop("Unsupported family")
    }
  }
  s <- surprisals.gam(object, loo = FALSE)
  surprisal_prob_from_s(
    s,
    distribution = dist,
    approximation = approximation,
    threshold_probability = threshold_probability,
    y = object$y * object$prior.weights
  )
}

utils::globalVariables(
  c(
    ".resid",
    ".se.fit",
    ".std.resid",
    ".resid",
    ".sigma",
    ".hat",
    "studentized_residuals"
  )
)
