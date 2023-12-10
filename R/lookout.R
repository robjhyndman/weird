#' @title Lookout probabilities
#' @description Compute leave-one-out log score probabilities using a
#' Generalized Pareto distribution. These give the probability of each observation
#' being an anomaly.
#' @details If the log_loo_scores are not available, the log scores are used
#' to compute the anomaly probabilities.
#' @param density_scores Numerical vector of log scores
#' @param log_loo_scores Optional numerical vector of leave-one-out log scores
#' @param threshold_probability Probability threshold when computing the POT model for the log scores.
#' @references Sevvandi Kandanaarachchi & Rob J Hyndman (2022) "Leave-one-out
#' kernel density estimates for outlier detection", *J Computational & Graphical
#' Statistics*, **31**(2), 586-599. \url{http://robjhyndman.com/publications/lookout}
#' @return A numerical vector containing the lookout probabilities
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' tibble(
#'   y = c(5, rnorm(49)),
#'   fscores = density_scores(y),
#'   loo_fscores = density_scores(y, loo = TRUE),
#'   lookout = lookout(fscores, loo_fscores)
#' )
#' # Bivariate data
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   fscores = density_scores(y),
#'   loo_fscores = density_scores(y, loo = TRUE),
#'   lookout = lookout(fscores, loo_fscores)
#' )
#' # Using a regression model
#' shiraz <- wine_reviews |> filter(variety %in% c("Shiraz", "Syrah"))
#' fit_wine <- lm(log(price) ~ points, data = shiraz)
#' shiraz |>
#'   mutate(
#'     fscore = density_scores(fit_wine),
#'     loo_fscore = density_scores(fit_wine, loo = TRUE),
#'     lookout_prob = lookout(fscore, loo_fscore)
#'   ) |>
#'   arrange(lookout_prob)
#' @importFrom stats quantile
#' @importFrom evd fpot pgpd

#' @export

lookout <- function(density_scores, log_loo_scores = density_scores, threshold_probability = 0.95) {
  threshold <- stats::quantile(density_scores, prob = threshold_probability, type = 8)
  gpd <- evd::fpot(density_scores, threshold = threshold, std.err = FALSE)$estimate
  lookout_probabilities <- evd::pgpd(
    log_loo_scores, loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
}

