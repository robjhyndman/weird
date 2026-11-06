#' @rdname surprisals_factor
#' @inherit surprisals return author references
#' @title Surprisals and surprisal probabilities of categorical data
#' @description For a categorical variable taking one of the categories
#' \eqn{c_1,\dots,c_K}, the surprisal of an observation \eqn{y} is
#' \eqn{s = -\log f(y)}, where \eqn{f(c_k)} is the probability of category
#' \eqn{c_k}. The surprisal probability is the total probability of all
#' categories that are no more probable than \eqn{y}:
#' \deqn{p = \sum_{k=1}^K f(c_k) 1\big(f(c_k) \le f(y)\big).}
#' Tied categories (with the same probability) have the same surprisal
#' probability, which includes the probabilities of all of them.
#' @details By default, the probabilities are estimated from the data as
#' \eqn{\hat{f}(c_k) = (n_k + a)/(n + Ka)}, where \eqn{n_k} is the number of
#' observations in category \eqn{c_k}, \eqn{n} is the number of
#' non-missing observations, \eqn{K} is the number of levels of `object`
#' (including levels with no observations), and \eqn{a} is `prior`. This is the
#' posterior mean of \eqn{f(c_k)} under a symmetric Dirichlet prior with
#' parameter \eqn{a}. With `loo = TRUE`, each observation is omitted when
#' estimating the probabilities used for its own surprisal and surprisal
#' probability, so that \eqn{n_k} and \eqn{n} are each reduced by one for an
#' observation in category \eqn{c_k}.
#'
#' To compute surprisals of several categorical variables jointly, combine
#' them into a single factor with [interaction()].
#' @param object A factor.
#' @param prob A numeric vector of probabilities for the levels of `object`,
#' either named by level or in the order of `levels(object)`. If `NULL` (the
#' default), the probabilities are estimated from `object`.
#' @param prior A non-negative constant \eqn{a} added to the count of each
#' level when estimating the probabilities. Only used when `prob` is `NULL`.
#' @param loo Should leave-one-out surprisals be computed? Only used when
#' `prob` is `NULL`.
#' @param approximation Character string specifying the method to use in
#' computing the surprisal probabilities. With `"none"` (the default), the
#' exact probabilities are computed from the category probabilities. With
#' `"empirical"` (or `"rank"`), the surprisal probability of each observation
#' is the proportion of observations with greater or equal surprisals.
#' @param threshold_probability Not used for categorical data.
#' @param ... Other arguments are ignored.
#' @examples
#' # Road types of Australian road deaths since 2014
#' road_deaths |>
#'   filter(!is.na(road_type)) |>
#'   mutate(
#'     s = surprisals(road_type),
#'     p = surprisals_prob(road_type)
#'   ) |>
#'   count(road_type, s, p) |>
#'   arrange(p)
#' # Joint surprisal probabilities of road user and sex
#' road_deaths |>
#'   filter(!is.na(road_user), !is.na(gender)) |>
#'   mutate(p = surprisals_prob(interaction(road_user, gender))) |>
#'   count(road_user, gender, p) |>
#'   arrange(p)
#' @export
surprisals.factor <- function(
  object,
  prob = NULL,
  prior = 0,
  loo = FALSE,
  ...
) {
  fy <- factor_probs(object, prob, prior, loo)$fy
  -log(fy)
}

#' @rdname surprisals_factor
#' @export
surprisals_prob.factor <- function(
  object,
  approximation = c("none", "empirical", "rank"),
  threshold_probability = 0.10,
  prob = NULL,
  prior = 0,
  loo = FALSE,
  ...
) {
  approximation <- match.arg(approximation)
  fp <- factor_probs(object, prob, prior, loo)
  if (approximation != "none") {
    s <- -log(fp$fy)
    return(rank(-s, ties.method = "max", na.last = "keep") / sum(!is.na(s)))
  }
  # Surprisal probability of each level, given the level probabilities f.
  # Allow for rounding error so that tied levels are counted together.
  level_prob <- function(f, j) {
    min(1, sum(f[f <= f[j] * (1 + 1e-9)]))
  }
  K <- length(fp$f)
  if (is.null(fp$f_loo)) {
    p_level <- vapply(seq_len(K), \(j) level_prob(fp$f, j), numeric(1))
  } else {
    p_level <- vapply(seq_len(K), \(j) level_prob(fp$f_loo[, j], j), numeric(1))
  }
  p_level[as.integer(object)]
}

# Probabilities of the levels of a factor, and of each observation.
# Returns a list with
#   f:     probabilities of the levels
#   f_loo: K x K matrix whose jth column gives the probabilities of the levels
#          when one observation in level j is omitted (NULL unless loo = TRUE)
#   fy:    probability of each observation (leave-one-out if loo = TRUE)
factor_probs <- function(object, prob = NULL, prior = 0, loo = FALSE) {
  if (!is.factor(object)) {
    stop("object must be a factor")
  }
  lev <- levels(object)
  K <- length(lev)
  idx <- as.integer(object)
  if (!is.null(prob)) {
    if (loo || prior != 0) {
      stop("loo and prior can only be used when prob is estimated from the data")
    }
    if (!is.numeric(prob) || length(prob) != K) {
      stop("prob must be a numeric vector with one value for each level of object")
    }
    if (!is.null(names(prob))) {
      if (!setequal(names(prob), lev)) {
        stop("The names of prob must be the levels of object")
      }
      prob <- prob[lev]
    }
    if (anyNA(prob) || any(prob < 0) || abs(sum(prob) - 1) > 1e-8) {
      stop("prob must be non-negative and sum to 1")
    }
    f <- unname(prob)
    return(list(f = f, f_loo = NULL, fy = f[idx]))
  }
  if (!is.numeric(prior) || length(prior) != 1L || is.na(prior) || prior < 0) {
    stop("prior must be a single non-negative number")
  }
  nk <- tabulate(idx, nbins = K)
  n <- sum(nk)
  if (n == 0L) {
    stop("object has no non-missing values")
  }
  f <- (nk + prior) / (n + K * prior)
  if (!loo) {
    return(list(f = f, f_loo = NULL, fy = f[idx]))
  }
  if (n == 1L && prior == 0) {
    stop("At least two observations are needed for leave-one-out probabilities")
  }
  # Column j: counts with one observation removed from level j
  f_loo <- (matrix(nk + prior, K, K) - diag(K)) / (n - 1 + K * prior)
  list(f = f, f_loo = f_loo, fy = f_loo[cbind(idx, idx)])
}
