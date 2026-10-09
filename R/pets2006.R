#' PETS2006 video frames
#'
#' Greyscale frames from the PETS2006 video in the CDnet 2014 change detection
#' benchmark. The video was taken by a fixed camera overlooking the concourse
#' of a railway station. For most of the video, a man stands near the glass
#' wall at the back of the concourse with a bag, while other people walk past.
#' He then puts the bag down, and eventually walks away, leaving it behind.
#' The original 720 x 576 colour frames have been converted to greyscale and
#' reduced to 120 x 96 pixels by averaging over blocks of 6 x 6 pixels. The
#' data are downloaded and returned.
#'
#' The benchmark uses frames 1--299 to initialise background models, and
#' evaluates methods on frames 300--1200.
#'
#' @format A data frame with 1200 rows and 2 columns:
#' \describe{
#'   \item{frame}{Frame number}
#'   \item{pixels}{A 1200 x 11520 integer matrix of pixel intensities, from 0
#'     (black) to 255 (white), with one row per frame. Each row contains the
#'     96 x 120 pixels of a frame, stored column by column, so
#'     `matrix(pixels[i, ], nrow = 96)` gives frame `i` as an image.}
#' }
#' @return Data frame
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter 13,
#' \url{https://OTexts.com/weird/imagevideo.html}.
#' @examples
#' \dontrun{
#' pets2006 <- fetch_pets2006()
#' # Show frame 951
#' image(t(matrix(pets2006$pixels[951, ], nrow = 96))[, 96:1], col = grey.colors(256))
#' }
#' @source Wang, Y., Jodoin, P.-M., Porikli, F., Konrad, J., Benezeth, Y., &
#' Ishwar, P. (2014). CDnet 2014: An expanded change detection benchmark
#' dataset. *IEEE Conference on Computer Vision and Pattern Recognition
#' Workshops*, 393--400. \url{http://changedetection.net}
#' @aliases pets2006
#' @export
fetch_pets2006 <- function() {
  fetch_rds("pets2006")
}
