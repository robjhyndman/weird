#' @rdname surprisals_data
#' @inherit surprisals
#' @title Surprisals and surprisal probabilities computed from data
#' @param object A numerical data set (either a vector, matrix, or a data.frame
#' containing only numerical columns).
#' @param distribution A distribution object. By default, a kernel density
#' estimate is computed from the data `object`.
#' @param approximation Character string specifying the method to use in
#' computing the surprisal probabilities. See Details below. For a multivariate
#' data set, it needs to be set to either "gpd" or "rank".
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
#'   prob1 = surprisals_prob(y),
#'   prob2 = surprisals_prob(y, loo = TRUE),
#'   prob3 = surprisals_prob(y, distribution = dist_normal()),
#'   prob4 = surprisals_prob(y, distribution = dist_normal(), approximation = "gpd")
#' ) |>
#'   arrange(prob1)
#' # Bivariate data
#' tibble(
#'   x = rnorm(50),
#'   y = c(5, rnorm(49)),
#'   prob = surprisals_prob(cbind(x, y), approximation = "gpd")
#' )
#' oldfaithful |>
#'   mutate(
#'     s = surprisals(cbind(duration, waiting), loo = TRUE),
#'     p = surprisals_prob(cbind(duration, waiting), loo = TRUE, approximation = "gpd")
#'   ) |>
#'   arrange(p)
#' @export
surprisals.numeric <- function(
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
  surprisals_from_den(object, den, distribution, loo)
}

#' @rdname surprisals_data
#' @export
surprisals.matrix <- function(
  object,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  if (!is.numeric(object)) {
    stop("matrix must be numeric")
  }
  surprisals.numeric(object, distribution = distribution, loo = loo, ...)
}

#' @rdname surprisals_data
#' @export
surprisals.data.frame <- function(
  object,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  object <- as.matrix(object)
  surprisals.matrix(
    object,
    distribution = distribution,
    loo = loo,
    ...
  )
}

#' @rdname surprisals_data
#' @export
surprisals_prob.numeric <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  approximation <- match.arg(approximation)
  s <- surprisals.numeric(object, distribution = distribution, loo = loo)
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
  )
}

#' @rdname surprisals_data
#' @export
surprisals_prob.matrix <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  if (!is.numeric(object)) {
    stop("matrix must be numeric")
  }
  surprisals_prob.numeric(
    object,
    approximation = approximation,
    threshold_probability = threshold_probability,
    distribution = distribution,
    loo = loo,
    ...
  )
}

#' @rdname surprisals_data
#' @export
surprisals_prob.data.frame <- function(
  object,
  approximation = c("none", "gpd", "rank"),
  threshold_probability = 0.10,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
) {
  object <- as.matrix(object)
  surprisals_prob.matrix(
    object,
    approximation = approximation,
    threshold_probability = threshold_probability,
    distribution = distribution,
    loo = loo,
    ...
  )
}
