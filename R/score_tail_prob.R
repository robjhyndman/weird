#' Density score tail probabilities
#'
#' Compute the probability of a density score at least as extreme as `g` given
#' a specified distribution. A density score is given by $g = -\log(f)$ where
#' $f$ is the density or probability mass function of the distribution. The
#' argument `distribution` specifies which distribution to use. The
#' probabilities are estimate by simulation; `n` random values are generated
#' from the specified `distribution`. Consequently, slightly different values
#' will be returned each time unless a random seed is set using
#' \link{\code[base]{set.seed}}.
#'
#' @param g vector of density scores.
#' @param distribution A distributional object specifying the distribution to
#' use.
#' @param n Number of samples to generate for estimating the probabilities.
#' @examples
#' library(distributional)
#' score_tail_prob(-log(dnorm(1:4)), dist_normal())
#' @export

score_tail_prob <- function(g, distribution = NULL, n = 1e6) {
  y <- distributional::generate(distribution, times=n)[[1]]
  scores <- -log(density(distribution, y)[[1]])
  1-cdf(distributional::dist_sample(list(scores)), q = g)[[1]]
}

