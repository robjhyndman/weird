#' @title Surprisals and surprisal probabilities

#' @description A surprisal is given by \eqn{s = -\log f(y)} where \eqn{f} is the
#' density or probability mass function of the estimated or assumed distribution,
#' and \eqn{y} is an observation. This is returned by `surprisals()`.
#' A surprisal probability is the probability of a surprisal at least as extreme
#' as \eqn{s}. This is returned by `surprisals_prob()`
#'
#' @details The surprisal probabilities may be computed in three different ways.
#' 1. When `approximation = "none"` (the default), the surprisal probabilities are computed
#' using the same distribution that was used to compute the surprisal values.
#' Under this option, surprisal probabilities are equal to 1 minus the
#' coverage probability of the largest HDR that contains each value. Surprisal
#' probabilities smaller than 1e-6 are returned as 1e-6.
#' 2. When `approximation = "gdp"`, the surprisal probabilities are
#' computed using a Generalized Pareto Distribution fitted to the most extreme
#' surprisal values (those with probability less than `threshold_probability`).
#' For surprisal probabilities greater than `threshold_probability`, the value of
#' `threshold_probability` is returned. Under this option, the distribution is
#' used for computing the surprisal values but not for determining their
#' probabilities. Due to extreme value theory, the resulting probabilities should
#' be relatively insensitive to the distribution used in computing the surprisal
#' values.
#' 3. When `approximation = "rank"`, the surprisal probability of each
#' observation is estimated using the proportion of observations with
#' greater surprisal values; i.e., 1 - rank(s)/n where `rank(s)` is the rank
#' of the surprisal value `s` among all surprisal values, and `n` is the
#' number of observations. This is a nonparametric approach that is also
#' insensitive to the distribution used in computing the surprisal values.
#' @param object A model or numerical data set
#' @param approximation Character string specifying the method to use in
#' computing the surprisal probabilities. See Details below.
#' @param threshold_probability Probability threshold when computing the GPD
#' approximation. This is the probability below which the GPD is fitted. Only
#' used if `approximation = "gpd"`.
#' @param ... Other arguments are passed to the appropriate method.
#' @author Rob J Hyndman
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Chapter 6,
#' \url{https://OTexts.com/weird/}.
#' @return A numerical vector containing the surprisals or surprisal probabilities.
#' @seealso For specific methods, see [surprisals.numeric()] and [surprisals.lm()],
#' @export
surprisals <- function(object, ...) {
  UseMethod("surprisals")
}

#' @rdname surprisals
#' @export
surprisals_prob <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  ...
) {
  UseMethod("surprisals_prob")
}
