#' Compute robust multivariate scaled data
#'
#' @description A multivariate version of [base::scale()], that takes account
#' of the covariance matrix of the data, and uses robust estimates
#' of center, scale and covariance by default. The centers are removed using medians, the
#' scale function is the IQR, and the covariance matrix is estimated using a
#' robust OGK estimate. The data are scaled using the Cholesky decomposition of
#' the inverse covariance. Then the scaled data are returned.
#'
#' @details Optionally, the centering and scaling can be done for each variable
#' separately, so there is no rotation of the data, by setting `cov = NULL`.
#' Also optionally, non-robust methods can be used by specifying `center = mean`,
#' `scale = stats::sd()`, and `cov = stats::cov()`. Any non-numeric columns are retained
#' with a warning.
#'
#' @param object A vector, matrix, or data frame containing some numerical data.
#' @param center A function to compute the center of each numerical variable. Set
#' to NULL if no centering is required.
#' @param scale	 A function to scale each numerical variable. When
#' `cov = robustbase::covOGK()`, it is passed as the `sigmamu` argument.
#' @param cov A function to compute the covariance matrix. Set to NULL if no rotation required.
#' @param warning Should a warning be issued if non-numeric columns are ignored?
#' @return A vector, matrix or data frame of the same size and class as `object`,
#' but with numerical variables replaced by scaled versions.
#' @seealso [base::scale()], [stats::sd()], [stats::cov()], [robustbase::covOGK()], [robustbase::s_Qn()]
#' @author Rob J Hyndman
#' @examples
#' # Univariate z-scores (no rotation)
#' z <- mvscale(faithful, center = mean, scale = sd, cov = NULL, warning = FALSE)
#' # Non-robust scaling with rotation
#' z <- mvscale(faithful, center = mean, cov = stats::cov, warning = FALSE)
#' # Robust scaling and rotation
#' z <- mvscale(faithful, warning = FALSE)
#' @export
mvscale <- function(
  object,
  center = stats::median,
  scale = robustbase::s_Qn,
  cov = robustbase::covOGK,
  warning = TRUE
) {
  d <- NCOL(object)
  # Check if input is a vector
  is_vec <- d == 1L & !inherits(object, "matrix") & !inherits(object, "data.frame")
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
  } else {
    # It must be a data frame. So let's find the numeric columns
    numeric_col <- unlist(lapply(object, is.numeric))
    if (!all(numeric_col) & warning) {
      warning(
        "Ignoring non-numeric columns: ",
        paste(names(object)[!numeric_col], collapse = ", ")
      )
    }
    mat <- as.matrix(object[, numeric_col])
  }
  # Remove centers
  if (!is.null(center)) {
    med <- apply(mat, 2, center)
    mat <- sweep(mat, 2L, med)
  }
  # Create more resilient version of scale function
  if (!is.null(scale)) {
    my_scale <- function(x, ..., na.rm = TRUE) {
      s <- scale(x, ..., na.rm = na.rm)
      s[s == 0] <- 1 # Avoid division by zero
      return(s)
    }
  } else {
    my_scale <- function(x, ..., na.rm = TRUE) {
      1
    }
  }
  # Scale
  if (d == 1L) {
    z <- mat / my_scale(mat)
    if (is_vec) {
      return(as.vector(z))
    }
  } else if (!is.null(cov)) {
    if (identical(cov, robustbase::covOGK)) {
      S <- cov(mat, sigmamu = my_scale)$cov
    } else {
      S <- cov(mat)
    }
    Sinv <- try(solve(S), silent = TRUE)
    if (inherits(Sinv, "try-error")) {
      # Add a small ridge to the covariance matrix to avoid singularity issues
      Sinv <- try(solve(S + diag(1e-6, nrow(S), ncol(S))), silent = TRUE)
      if (inherits(Sinv, "try-error")) {
        # Add a bigger ridge
        Sinv <- solve(S + diag(1e-2, nrow(S), ncol(S)))
      }
    }
    U <- chol(Sinv)
    z <- mat %*% t(U)
  } else {
    s <- apply(mat, 2, my_scale)
    z <- sweep(mat, 2L, s, "/")
  }
  # Convert back to matrix, data frame or tibble if necessary
  idx <- which(numeric_col)
  for (i in seq_along(idx)) {
    object[, idx[i]] <- z[, i]
  }
  # Rename columns if there has been rotation
  if (!is.null(cov)) {
    names(object)[numeric_col] <- paste0("z", seq(sum(numeric_col)))
  }
  return(object)
}
