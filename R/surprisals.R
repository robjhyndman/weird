#' @title Surprisals

#' @description Compute surprisals from a model or a data set given a
#' probability distribution. The surprisals are defined as minus the log of the
#' density at each observation.
#' @param object A model or numerical data set
#' @param ... Other arguments are passed to the appropriate method.
#' @export

surprisals <- function(object, ...) {
  UseMethod("surprisals")
}

#' @title Surprisals computed from a data set
#'
#' @description Compute surprisals from a data set given a probability
#' distribution. The surprisals are defined as minus the log of the density at
#' each observation.
#'
#' @details If no distribution is provided, a kernel density estimate is
#' computed. The leave-one-out surprisals (or LOO surprisals) are obtained by
#' estimating the kernel density estimate using all other observations.
#'
#' @param object A numerical data  set, either a vector, matrix or data frame.
#' @param distribution A probability distribution stored as a distributional
#' object.
#' @param loo Should leave-one-out surprisals be computed?
#' @param ... Other arguments are passed to \code{\link{dist_kde}}.
#' @author Rob J Hyndman
#' @return A numerical vector containing the surprisals.
#' @seealso \code{\link{dist_kde}}
#' @examples
#' # surprisals computed from bivariate data set
#' oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   mutate(
#'     loo_fscores = surprisals(cbind(duration, waiting), loo = TRUE)
#'   )
#' @export
surprisals.default <- function(
    object,
    distribution = dist_kde(object, multiplier = 2, ...),
    loo = FALSE,
    ...) {
  object <- as.matrix(object)
  if(NCOL(object) == 1L)
    object <- c(object)
  scores <- -density(distribution, at = object, log = TRUE)[[1]]
  if(loo & stats::family(distribution) == "kde") {
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
  return(scores)
}

#' @rdname surprisals_model
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
#'     #lookout_prob = lookout(surprisals = fscore, loo_scores = loo_fscore)
#'   ) |>
#'   ggplot(aes(x = duration, y = waiting,
#'     color = loo_fscore > quantile(loo_fscore, 0.99))) +
#'   geom_point()
#' @export
surprisals.lm <- function(object, loo = FALSE, ...) {
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
surprisals.gam <- function(object, loo = FALSE, ...) {
  if (loo) {
    warning("Leave-one-out log scores unavailable for GAM models. Returning log scores.")
  }
  fit_aug <- broom::augment(object, type.predict = "response")
  if (object$family$family == "gaussian") {
    std.resid <- c(scale(fit_aug$.resid / fit_aug$.se.fit))
    surprisals <- -dnorm(std.resid, log = TRUE)
  } else if (object$family$family == "binomial") {
    surprisals <- -dbinom(
      x = object$y * object$prior.weights,
      size = object$prior.weights, prob = fit_aug$.fitted, log = TRUE
    )
  } else if (object$family$family == "poisson") {
    surprisals <- -dpois(object$y, lambda = fit_aug$.fitted, log = TRUE)
  } else {
    stop("Unsupported family")
  }
  return(surprisals)
}

utils::globalVariables(c(
  ".resid", ".se.fit", ".std.resid", ".resid", ".sigma", ".hat",
  "studentized_residuals"
))
