#' @title Lookout probabilities
#' @description The lookout algorithm (Kandanaarachchi & Hyndman, 2022) computes
#' leave-one-out surprisal probabilities from a kernel density estimate using a
#' Generalized Pareto distribution. The kernel density estimate uses a
#' bandwidth based on topological data analysis and a quadratic kernel. So it is
#' similar but not identical to using \link{surprisal_prob} with `loo = TRUE`
#' and `GPD = TRUE`. A low probability indicates a likely anomaly.
#' @param object A numerical data set.
#' @param ... Other arguments are passed to \code{\link[lookout]{lookout}}.
#' @references Sevvandi Kandanaarachchi & Rob J Hyndman (2022) "Leave-one-out
#' kernel density estimates for outlier detection", *J Computational & Graphical
#' Statistics*, **31**(2), 586-599. \url{https://robjhyndman.com/publications/lookout/}
#' @return A numerical vector containing the lookout probabilities
#' @seealso \code{\link[lookout]{lookout}}
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' tibble(
#'   y = c(5, rnorm(49)),
#'   lookout = lookout_prob(y)
#' )
#' # Bivariate data
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   lookout = lookout_prob(cbind(x, y))
#' )
#' # Using a regression model
#' of <- oldfaithful |> filter(duration < 7200, waiting < 7200)
#' fit_of <- lm(waiting ~ duration, data = of)
#' broom::augment(fit_of) |>
#'   mutate(lookout = lookout_prob(.std.resid)) |>
#'   arrange(lookout)
#' @export

lookout_prob <- function(object, ...) {
  return(lookout::lookout(object, ...)$outlier_probability)
}
