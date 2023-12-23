#' @title Lookout probabilities
#' @description Compute leave-one-out log score probabilities using a
#' Generalized Pareto distribution. These give the probability of each observation
#' being an anomaly.
#' @details This function can work with several object types.
#' If `object` is not `NULL`, then the object is passed to \code{\link{density_scores}}
#' to compute density scores (and possibly LOO density scores). Otherwise,
#' the density scores are taken from the `density_scores` argument, and the
#' LOO density scores are taken from the `loo_scores` argument. Then the Generalized
#' Pareto distribution is fitted to the scores, to obtain the probability of each observation.
#' @param object A model object or a numerical data set.
#' @param density_scores Numerical vector of log scores
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
#'   fscores = density_scores(y),
#'   loo_fscores = density_scores(y, loo = TRUE),
#'   lookout = lookout(density_scores = fscores, loo_scores = loo_fscores)
#' )
#' # Using a regression model
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- lm(log(price) ~ points, data = shiraz)
#' shiraz |>
#'   mutate(lookout_prob = lookout(fit_wine)) |>
#'   arrange(lookout_prob)
#' @importFrom stats quantile
#' @importFrom evd fpot pgpd

#' @export

lookout <- function(
    object = NULL,
    density_scores = NULL, loo_scores = density_scores,
    threshold_probability = 0.95) {
  if (!is.null(object)) {
    if (!is.null(density_scores) | !is.null(loo_scores)) {
      warning("Ignoring density_scores and loo_scores arguments and using object.")
    }
    if (is.data.frame(object) | inherits(object, "matrix") | inherits(object, "numeric")) {
      tmp <- calc_kde_scores(as.matrix(object))
      density_scores <- tmp$scores
      loo_scores <- tmp$loo_scores
    } else {
      density_scores <- density_scores(object)
      loo_scores <- density_scores(object, loo = TRUE) |> suppressWarnings()
    }
  }
  threshold <- stats::quantile(density_scores, prob = threshold_probability, type = 8)
  if (sum(density_scores > threshold) == 0L) {
    warning("No scores above threshold.")
    return(rep(1, length(density_scores)))
  }
  gpd <- evd::fpot(density_scores, threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(
    loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
}
