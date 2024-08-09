
# Color palette designed for plotting Highest Density Regions
#
# A sequential color palette is returned, with the first color being `color`,
# and the rest of the colors being a mix of `color` with increasing amounts of white.
# If `prob` is provided, then the mixing proportions are determined by `prob` (and
# n is ignored). Otherwise the mixing proportions are equally spaced between 0 and 1.
#
# @param n Number of colors in palette.
# @param color First color of vector.
# @param prob Vector of probabilities between 0 and 1.
# @return A function that returns a vector of colors of length `length(prob) + 1`.
# @examples
# hdr_palette(prob = c(0.5, 0.99))

hdr_palette <- function(n, color = "#0072b2", prob = NULL) {
  if (missing(prob)) {
    prob <- seq(n - 1) / n
  } else if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  pc_colors <- grDevices::colorRampPalette(c(color, "white"))(180)[32:130]
  idx <- approx(seq(99) / 100, seq(99), prob, rule = 2)$y
  c(color, pc_colors[idx])
}

utils::globalVariables(c("x", "y", "y1", "y2"))
