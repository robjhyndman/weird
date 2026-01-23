# Bilinear interpolation on a grid
# x: vector of x grid points
# y: vector of y grid points
# z: matrix of z values at grid points (rows correspond to x, columns to y)
# x0: x coordinate of the point to interpolate
# y0: y coordinate of the point to interpolate
# Returns: interpolated z value at (x0, y0)

bilinear_interpolation <- function(x, y, z, x0, y0) {
  nx <- length(x)
  ny <- length(y)
  if (!identical(dim(z), c(nx, ny))) {
    stop("Dimensions of z do not match lengths of x and y")
  }
  n <- length(x0)
  if (length(y0) != n) {
    stop("x0 and y0 must have the same length")
  }
  i <- floor((x0 - x[1]) / (x[2] - x[1])) + 1L
  j <- floor((y0 - y[1]) / (y[2] - y[1])) + 1L
  outside <- (i < 1 | i > nx - 1 | j < 1 | j > ny - 1)
  ii <- i[!outside]
  jj <- j[!outside]
  z11 <- as.vector(z)[ii + (jj - 1) * nx]
  z12 <- as.vector(z)[ii + jj * nx]
  z21 <- as.vector(z)[ii + 1 + (jj - 1) * nx]
  z22 <- as.vector(z)[ii + 1 + jj * nx]
  ez <- rep(NA_real_, n)
  ex <- (x0[!outside] - x[ii]) / (x[ii + 1] - x[ii])
  ey <- (y0[!outside] - y[jj]) / (y[jj + 1] - y[jj])
  ez[!outside] <- (1 - ex) *
    (1 - ey) *
    z11 +
    (1 - ex) * (ey) * z12 +
    (ex) * (1 - ey) * z21 +
    (ex) * (ey) * z22
  return(ez)
}
