#' @title Regression scores from a linear model
#' @description Compute log scores and log-loo scores from a linear Gaussian regression model,
#'  defined as minus the log of the conditional density computed at each observation.
#'  Leave-one-out scores are optionally computed.
#' @param object A fitted model, usually produced by \code{\link[stats]{lm}}.
#' @param loo Should leave-one-scores be returned? Default: FALSE.
#' @return Numerical vector containing log scores.
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' library(tidyr)
#' fit <- lm(log(price) ~ points, data = wine_reviews)
#' wine_reviews |>
#'   mutate(loo_scores = reg_scores(fit, loo = TRUE)) |>
#'   ggplot(aes(x = points, y = loo_scores)) +
#'   geom_jitter(width = 0.2, height = 0, alpha = 0.2)
#' @export
#' @rdname reg_scores
reg_scores <- function(object, loo = FALSE) {
  if (!inherits(object, "lm")) {
    stop("object must be a linear model")
  }
  e <- stats::residuals(object, type = "response")
  h <- stats::hatvalues(object)
  sigma2 <- sum(e^2, na.rm = TRUE)/object$df.residual

  if (loo) {
    n <- length(e)
    p <- length(object$coefficients)
    sigma2 <- (sigma2 * (n-p-1) - e^2/(1-h)) / (n-p-2)
  }
  return(log(2*pi)/2 + e^2/(1-h)/sigma2)
}
