# PETS2006 video from the CDnet 2014 change detection benchmark (Wang et al., 2014)
# http://changedetection.net (baseline category), downloaded via
# https://www.kaggle.com/datasets/maamri95/cdnet2014
# Usage: Rscript data-raw/pets2006.R <path to dataset/baseline/PETS2006>
# All 1200 frames, converted to greyscale and downsampled from 720 x 576
# to 120 x 96 by averaging 6 x 6 blocks.
# The resulting data-raw/pets2006.rds is downloaded by fetch_pets2006().

path <- commandArgs(trailingOnly = TRUE)[1]
frames <- 1:1200
block <- 6

# Average non-overlapping block x block cells of a matrix
downsample <- function(x) {
  nr <- nrow(x) / block
  nc <- ncol(x) / block
  x <- array(x, c(block, nr, block, nc))
  apply(x, c(2, 4), mean)
}
read_frame <- function(i) {
  img <- jpeg::readJPEG(file.path(path, "input", sprintf("in%06d.jpg", i)))
  grey <- 0.299 * img[, , 1] + 0.587 * img[, , 2] + 0.114 * img[, , 3]
  c(round(255 * downsample(grey)))
}

# One row per frame; pixels stored column by column (as in c(matrix))
pixels <- t(sapply(frames, read_frame))
storage.mode(pixels) <- "integer"
pets2006 <- tibble::tibble(frame = frames, pixels = pixels)
saveRDS(pets2006, "data-raw/pets2006.rds", compress = "xz")
