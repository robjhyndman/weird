#' @title Lookout probabilities
#' @description Compute leave-one-out log score probabilities using a
#' Generalized Pareto distribution. These give the probability of each observation
#' being from the same distribution as the majority of observations. A low probability
#' indicates a likely anomaly.
#' @details This function can work with several object types.
#' If `object` is not `NULL`, then the object is passed to \code{\link{surprisals}}
#' to compute surprisals (and possibly LOO surprisals). Otherwise,
#' the surprisals are taken from the `surprisals` argument, and the
#' LOO surprisals are taken from the `loo_scores` argument. Then the Generalized
#' Pareto distribution is fitted to the scores, to obtain the probability of each observation.
#' @param object A model object or a numerical data set.
#' @param surprisals Numerical vector of log scores
#' @param loo_scores Optional numerical vector of leave-one-out log scores
#' @param threshold_probability Probability threshold when computing the POT model for the log scores.
#' @references Sevvandi Kandanaarachchi & Rob J Hyndman (2022) "Leave-one-out
#' kernel density estimates for outlier detection", *J Computational & Graphical
#' Statistics*, **31**(2), 586-599. \url{https://robjhyndman.com/publications/lookout/}
#' @return A numerical vector containing the lookout probabilities
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' tibble(
#'   y = c(5, rnorm(49)),
#'   lookout = lookout(y)
#' )
#' # Bivariate data with score calculation done outside the function
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   fscores = surprisals(y),
#'   loo_fscores = surprisals(y, loo = TRUE),
#'   lookout = lookout(surprisals = fscores, loo_scores = loo_fscores)
#' )
#' # Using a regression model
#' of <- oldfaithful |> filter(duration < 7200, waiting < 7200)
#' fit_of <- lm(waiting ~ duration, data = of)
#' of |>
#'   mutate(lookout_prob = lookout(fit_of)) |>
#'   arrange(lookout_prob)
#' @importFrom stats quantile
#' @importFrom evd fpot pgpd

#' @export

lookout <- function(
    object = NULL,
    surprisals = NULL, loo_scores = surprisals,
    threshold_probability = 0.95) {
  if (!is.null(object)) {
    if (!is.null(surprisals) | !is.null(loo_scores)) {
      warning("Ignoring surprisals and loo_scores arguments and using object.")
    }
    if (is.data.frame(object) | inherits(object, "matrix") | inherits(object, "numeric")) {
      tmp <- calc_kde_scores(as.matrix(object))
      surprisals <- tmp$scores
      loo_scores <- tmp$loo_scores
    } else {
      surprisals <- surprisals(object)
      loo_scores <- surprisals(object, loo = TRUE) |> suppressWarnings()
    }
  }
  threshold <- stats::quantile(surprisals,
    prob = threshold_probability,
    type = 8, na.rm = TRUE
  )
  if (sum(surprisals > threshold, na.rm = TRUE) == 0L) {
    warning("No scores above threshold.")
    return(rep(1, length(surprisals)))
  }
  finite <- surprisals < Inf
  if (any(!finite, na.rm = TRUE)) {
    warning("Infinite surprisals will be ignored in GPD.")
  }
  gpd <- evd::fpot(surprisals[finite], threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(
    loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
}
