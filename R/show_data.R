# Function to construct a data frame for plotting purposes
# The object is a distributional object returned from dist_kde()

show_data <- function(object, prob, threshold, anomalies = FALSE) {
  # Names of distributions
  dist_names <- names_dist(object)
  d <- dimension_dist(object)

  # Extract data
  x <- lapply(vctrs::vec_data(object), function(u) u$kde$x)
  names(x) <- dist_names
  if (all(lengths(x) == 0)) {
    stop("No observations found")
  }
  # Drop distributions with no data
  some_data <- which(lengths(x) > 0)
  object <- object[some_data]
  x <- x[some_data]
  show_x <- purrr::map2(
    x, names(x),
    function(u, dist) {
      tmp <- tibble::as_tibble(u) |> dplyr::mutate(Distribution = dist)
      d <- NCOL(u)
      colnames(tmp)[seq(d)] <- c("x", "y")[seq(d)]
      return(tmp)
    }
  )
  # Compute density values
  show_x <- mapply(
      function(u, dist) {
        d <- NCOL(u) - 1
        u$den <- unlist(density(dist, at = as.matrix(u[, seq(d)])))
        return(u)
      },
      u = show_x, dist = as.list(object),
      SIMPLIFY = FALSE
    )

  # Divide into HDR groups
  if(!is.null(threshold)) {
    show_x <- mapply(
      function(u, threshold) {
        u$group <- cut(u$den, breaks = c(0, threshold$threshold, Inf), labels = FALSE)
        u$group <- factor(u$group,
          levels = rev(seq(length(prob) + 1)),
          labels = c(paste0(sort(prob) * 100, "%"), "Outside")
        )
        u$level <- sort(threshold$level)[as.numeric(u$group)]
        u$level[is.na(u$level)] <- Inf
        return(u)
      },
      u = show_x, threshold = split(threshold, threshold$Distribution),
      SIMPLIFY = FALSE
    )
  }

  # Compute surprisal probabilities
  if(anomalies) {
    p <- mapply(
        function(data, dist) { c(surprisal_prob(data, dist, loo = TRUE)) },
        x, object,
        SIMPLIFY = FALSE
    )
    # Anomalies are points with p < 0.005
    show_x <- mapply(
      function(u, p) {
        u$anomaly <- p < 0.005 & u$group == "Outside"
        return(u)
      },
      u = show_x, p,
      SIMPLIFY = FALSE
    )
  }
  # Combine into a single tibble
  purrr::list_rbind(show_x)
}
