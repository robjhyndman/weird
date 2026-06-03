#' Compute robust multivariate scaled data
#'
#' @description A multivariate version of [base::scale()], that takes account
#' of the covariance matrix of the data. By default, robust estimates are used:
#' the centers are removed using medians,
#' the scale function for univariate data is `s_Qn`,
#' and the covariance matrix for multivariate data is estimated using a robust MCD estimate.
#' The data are scaled using the Cholesky decomposition of
#' the inverse (co)variance. Then the scaled data are returned.
#' Details of the methods are provided by Hyndman (2026).
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 2.6, 3.6 and 3.7.
#'
#' @details Optionally, the centering and scaling can be done for each variable
#' separately, by setting `cov = NULL`, so there is no rotation of the data,
#' Also optionally, non-robust methods can be used by specifying `center = mean`,
#' `scale = stats::sd()`, and `cov = stats::cov()`. Any non-numeric columns are retained
#' with a warning.
#' Missing values are removed before the centers, scale and cov are estimated.
#'
#' @param object A vector, matrix, or data frame containing some numerical data.
#' @param center A function to compute the center of each numerical variable. Set
#' to NULL if no centering is required.
#' @param scale	 A function to scale each numerical variable. When
#' `cov = robustbase::covOGK()`, `scale` is passed as the `sigmamu` argument. When
#'  `cov = robustbase::covMcd()`, `scale` is passed as the `scalefn` argument.
#' @param cov A function to compute the covariance matrix. Set to NULL if no rotation required. `cov()` must either return the matrix directly, or a list containing a matrix named `cov`.
#' @param alpha When `cov = robustbase::covMcd()`, `alpha` controls the size of the subsets over which the determinant is minimized. Otherwise it is ignored. Set to 0.9 by default.
#' @param warning Should a warning be issued if non-numeric columns are ignored?
#' @param what What to return? Default `"object"` returns the scaled data.
#' Alternatively, `"terms"` returns the location and scale/covariance estimates,
#' while `"all"` gives both the scaled data and estimated quantities.
#' @param ... Other arguments are passed to `cov()`.
#' @return A vector, matrix or data frame of the same size and class as `object`,
#' but with numerical variables replaced by scaled versions (renamed if they have been rotated).
#' @seealso [base::scale()], [stats::sd()], [stats::cov()], [robustbase::covOGK()], [robustbase::s_Qn()]
#' @author Rob J Hyndman
#' @examples
#' # Univariate z-scores
#' z <- mvscale(oldfaithful$duration, center = mean, scale = sd)
#' # Non-robust scaling with no rotation
#' oldfaithful |>
#'   mvscale(center = mean, scale = sd, cov = NULL, warning = FALSE)
#' # Non-robust scaling with rotation
#' oldfaithful |>
#'   mvscale(center = mean, scale = sd, cov = stats::cov, warning = FALSE)
#' # Robust scaling and rotation
#' oldfaithful |>
#'   mvscale(warning = FALSE)
#' @export
mvscale <- function(
  object,
  center = stats::median,
  scale = robustbase::s_Qn,
  cov = robustbase::covMcd,
  alpha = 0.9,
  warning = TRUE,
  what = c("object", "terms", "all"),
  ...
) {
  what <- match.arg(what)
  what_terms <- FALSE
  if(what %in% c("terms", "all")){
    terms_list <- list()
    what_terms <- TRUE
  }
  d <- NCOL(object)
  # Check if input is a vector
  is_vec <- d == 1L &&
    !inherits(object, "matrix") &&
    !inherits(object, "data.frame")
  if (is_vec) {
    numeric_col <- is.numeric(object)
    if (!numeric_col) {
      stop("Input must be numeric")
    }
    mat <- as.matrix(object)
  } else if (inherits(object, "matrix")) {
    # It is already a matrix
    if (!is.numeric(object)) {
      stop("Input must be numeric")
    }
    numeric_col <- rep(TRUE, NCOL(object))
    mat <- object
  } else if (inherits(object, "data.frame")) {
    # Find the numeric columns
    numeric_col <- unlist(lapply(object, is.numeric))
    if (!all(numeric_col) && warning) {
      warning(
        "Ignoring non-numeric columns: ",
        paste(names(object)[!numeric_col], collapse = ", ")
      )
    }
    mat <- as.matrix(object[, numeric_col])
  } else {
    stop("object must be a numeric vector, matrix or data frame")
  }
  if (any(mat == Inf & !is.na(mat))) {
    stop("object contains infinite values")
  }
  # Remove centers
  if (!is.null(center)) {
    med <- apply(mat, 2, \(x) center(na.omit(x)))
    mat <- sweep(mat, 2L, med)
    if(what_terms) terms_list <- c(terms_list, list("center" = med))
  }
  # scale function that ignores missing values
  if (!is.null(scale)) {
    my_scale <- function(x, ...) scale(na.omit(x), ...)
  } else {
    my_scale <- function(x, ...) 1
  }
  # Scale
  if (d == 1L) {
    z <- mat / my_scale(mat)
    if (is_vec) {
      if(what_terms){
        terms_list <- c(terms_list, list("scale" = my_scale(mat)))
        if(what == "all") return(c(list("object" = as.vector(z)), terms_list))
        return(terms_list)
      }
      else return(as.vector(z))
    }
  } else if (!is.null(cov)) {
    # Compute covariance matrix from non-missing values
    mat_nomissing <- mat[stats::complete.cases(mat), , drop = FALSE]
    if (identical(cov, robustbase::covOGK)) {
      S <- cov(mat_nomissing, sigmamu = my_scale, ...)
    } else if (identical(cov, robustbase::covMcd)) {
      S <- cov(mat_nomissing, scalefn = my_scale, alpha = alpha, ...)
    } else {
      S <- cov(mat_nomissing, ...)
    }
    if (is.list(S)) {
      if ("cov" %in% names(S)) {
        S <- S$cov
      } else {
        stop("I can't find a covariance matrix in the list returned by cov()")
      }
    }
    if (!inherits(S, "matrix")) {
      stop("cov() did not return a matrix")
    }
    # Invert covariance matrix
    Sinv <- try(solve(S), silent = TRUE)
    if (inherits(Sinv, "try-error")) {
      # Add a small ridge to the covariance matrix to avoid singularity issues
      Sinv <- try(solve(S + diag(1e-6, nrow(S), ncol(S))), silent = TRUE)
      if (inherits(Sinv, "try-error")) {
        # Add a bigger ridge
        Sinv <- solve(S + diag(1e-2, nrow(S), ncol(S)))
      }
      warning("Covariance matrix is singular. Adding a small ridge penalty.")
    }
    if(what_terms) terms_list <- c(terms_list, list("cov" = S, "cov_inverse" = Sinv))
    # Compute scaled and rotated data
    U <- chol(Sinv)
    z <- mat %*% t(U)
  } else {
    # Just scale, no rotation
    s <- apply(mat, 2, my_scale)
    z <- sweep(mat, 2L, s, "/")
    if(what_terms) terms_list <- c(terms_list, list("scale" = s))
  }
  if(what == "terms") return(terms_list)
  # Convert back to matrix, data frame or tibble if necessary
  idx <- which(numeric_col)
  for (i in seq_along(idx)) {
    object[, idx[i]] <- z[, i]
  }
  # Rename columns if there has been rotation
  if (!is.null(cov)) {
    names(object)[numeric_col] <- paste0("z", seq(sum(numeric_col)))
  }
  if(what == "all") return(c(list("object" = object), terms_list))
  return(object)
}
