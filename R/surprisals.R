#' @title Surprisals and surprisal probabilities

#' @description A surprisal is given by \eqn{s = -\log f(y)} where \eqn{f} is the
#' density or probability mass function of the estimated or assumed distribution,
#' and \eqn{y} is an observation. This is returned by `surprisals()`.
#' A surprisal probability is the probability of a surprisal at least as extreme
#' as \eqn{s}. This is returned by `surprisals_prob()`
#'
#' The surprisal probabilities may be computed in three different ways.
#' 1. Given the same distribution that was used to compute the surprisal values.
#' Under this option, surprisal probabilities are equal to 1 minus the
#' coverage probability of the largest HDR that contains each value. Surprisal
#' probabilities smaller than 1e-6 are returned as 1e-6.
#' 2. Using a Generalized Pareto Distribution fitted to the most extreme
#' surprisal values (those with probability less than `threshold_probability`).
#' This option is used if `approximation = "gpd"`. For surprisal probabilities
#' greater than `threshold_probability`, the value of
#' `threshold_probability` is returned. Under this option, the distribution is
#' used for computing the surprisal values but not for determining their
#' probabilities. Due to extreme value theory, the resulting probabilities should
#' be relatively insensitive to the distribution used in computing the surprisal
#' values.
#' 3. Empirically as the proportion of observations with greater surprisal values.
#' This option is used when `approxiation = "empirical"`. This is also
#' insensitive to the distribution used in computing the surprisal values.
#' @param object A model or numerical data set
#' @param approximation Character string specifying the approximation to use in
#' computing the surprisal probabilities. Ignored if `probability = FALSE`.
#' `approximation = "none"` specifies that no approximation is to be used;
#' `approximation = "gpd"` specifies that  the Generalized Pareto distribution should be used;
#' while `approximation = "empirical"` specifies that the probabilities should be estimated empirically.
#' @param threshold_probability Probability threshold when computing the GPD
#' approximation. This is the probability below which the GPD is fitted. Only
#' used if `approximation = "gpd"` and `probability = TRUE`).
#' @param ... Other arguments are passed to the appropriate method.
#' @author Rob J Hyndman
#' @return A numerical vector containing the surprisals or surprisal probabilities.
#' @examples
#' # surprisals computed from bivariate data set
#' oldfaithful |>
#'   mutate(
#'     loo_fscores = surprisals_prob(cbind(duration, waiting), loo = TRUE)
#'   )
#' @export
surprisals <- function(
  object,
  ...
) {
  UseMethod("surprisals")
}

#' @rdname surprisals
#' @export
surprisals_prob <- function(
  object,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.10,
  ...
) {
  UseMethod("surprisals_prob")
}

#' @inherit surprisals
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
#'   p_kde = surprisals_prob(y, loo = TRUE),
#'   p_normal = surprisals_prob(y, distribution = dist_normal()),
#'   p_zscore = 2 * (1 - pnorm(abs(y)))
#' )
#' tibble(
#'   y = n01$v1,
#'   prob1 = surprisals_prob(y, loo = TRUE),
#'   prob2 = surprisals_prob(y, approximation = "gpd"),
#'   prob3 = surprisals_prob(y, distribution = dist_normal()),
#'   prob4 = surprisals_prob(y, distribution = dist_normal(), approximation = "gpd")
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
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  object <- as.matrix(object)
  if (NCOL(object) == 1L) {
    object <- c(object)
  }
  if (length(distribution) > 1 & length(object) > 1) {
    if (length(distribution) != length(object)) {
      stop("Length of distribution and object must be the same or equal to 1")
    }
  }
  if (length(distribution) == NROW(object)) {
    den <- mapply(density, distribution, object, log = TRUE)
  } else {
    den <- density(distribution, at = object, log = TRUE)
    if (is.list(den)) {
      if (length(den) > 1) {
        stop("What's going on?")
      }
      den <- den[[1]]
    }
  }
  surprisals_from_den(
    object,
    den,
    distribution,
    loo
  )
}

#' @rdname surprisals.default
#' @export
surprisals_prob.default <- function(
  object,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.10,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  approximation <- match.arg(approximation)
  s <- surprisals.default(object, distribution = distribution, loo = loo)
  if (loo & all(stats::family(distribution) == "kde")) {
    y <- object
  } else {
    y <- NULL
  }
  surprisal_prob_from_s(
    s,
    distribution = distribution,
    approximation = approximation,
    threshold_probability = threshold_probability,
    y = y
  ) |>
    suppressWarnings()
}

# Surprisals function that uses pre-calculated densities
# Same arguments as surprisals.default except den is numerical vector of log densities
surprisals_from_den <- function(
  object,
  den,
  distribution,
  loo
) {
  object <- as.matrix(object)
  if (NCOL(object) == 1L) {
    object <- c(object)
  }
  if (length(distribution) > 1 & length(object) > 1) {
    if (length(distribution) != length(object)) {
      stop("Length of distribution and object must be the same or equal to 1")
    }
  }
  scores <- -den
  if (loo & all(stats::family(distribution) == "kde")) {
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
#' @inherit surprisals
#' @title Surprisals and surprisal probabilities computed from a model
#' @param object A model object such as returned by \code{\link[stats]{lm}},
#' or \code{\link[mgcv]{gam}}.
#' @param loo Should leave-one-out surprisals be computed?
#' @param ... Other arguments are ignored.
#' @examples
#' # surprisals computed from linear model
#' lm_of <- lm(waiting ~ duration, data = oldfaithful)
#' oldfaithful |>
#'   mutate(
#'     fscore = surprisals_prob(lm_of),
#'     loo_fscore = surprisals_prob(lm_of, loo = TRUE),
#'   ) |>
#'   ggplot(aes(
#'     x = duration, y = waiting,
#'     color = loo_fscore > quantile(loo_fscore, 0.99)
#'   )) +
#'   geom_point()
#' @export
surprisals.lm <- function(
  object,
  loo = FALSE,
  ...
) {
  e <- stats::residuals(object, type = "response")
  h <- stats::hatvalues(object)
  sigma2 <- sum(e^2, na.rm = TRUE) / object$df.residual
  if (loo) {
    resdf <- object$df.residual
    sigma2 <- (sigma2 * resdf - e^2 / (1 - h)) / (resdf - 1)
  }
  r2 <- e^2 / ((1 - h) * sigma2)
  0.5 * (log(2 * pi) + r2)
}

#' @rdname surprisals_model
#' @export
surprisals_prob.lm <- function(
  object,
  approximation = c("none", "gpd", "empirical"),
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
#' @examples
#' # surprisals computed from GAM
#' gam_of <- mgcv::gam(waiting ~ s(duration), data = oldfaithful)
#' oldfaithful |>
#'   mutate(fscore = surprisals(gam_of))
#' @importFrom stats approx dbinom density dnorm dpois na.omit
#' @export
surprisals.gam <- function(
  object,
  ...
) {
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
  approximation = c("none", "gpd", "empirical"),
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

utils::globalVariables(c(
  ".resid",
  ".se.fit",
  ".std.resid",
  ".resid",
  ".sigma",
  ".hat",
  "studentized_residuals"
))
