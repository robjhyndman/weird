# Compute value of density at each observation using kde
calc_kde_scores <- function(y, h = kde_bandwidth(y), H = kde_bandwidth(y), ...) {
  n <- NROW(y)
  d <- NCOL(y)
  # Estimate density at each observation
  if(d == 1L) {
    gridsize = 10001
    K0 = 1/(h * sqrt(2 * pi))
    fi <- ks::kde(y, h = h, gridsize = gridsize, binned = n > 2000,
            eval.points = y, compute.cont = FALSE, ...)$estimate
  } else {
    gridsize = 101
    K0 = det(H)^(-1/2) * (2*pi)^(-d/2)
    fi <- ks::kde(y, H = H, gridsize = gridsize, binned = n > 2000,
            eval.points = y, compute.cont = FALSE, ...)$estimate
  }
  loo_scores <- -log(pmax(0, (n * fi - K0) / (n - 1)))
  scores <- -log(pmax(0, fi))
  return(list(scores = scores, loo_scores = loo_scores))
}
