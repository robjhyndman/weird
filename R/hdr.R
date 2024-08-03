#' @title HDR plot
#' @description Produces a 1d or 2d box plot of HDR regions. The darker regions
#' contain observations with higher probability, while the lighter regions contain
#' points with lower probability. Points outside the largest HDR are shown as
#' individual points. Points with lookout probabilities
#' less than 0.05 are optionally shown in red.
#' @details The original HDR boxplot proposed by Hyndman (1996), R can be produced with
#' all arguments set to their defaults other than `lookout`.
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 Optionally, the name of the second variable to plot (a bare expression).
#' @param prob A numeric vector specifying the coverage probabilities for the HDRs.
#' @param scatterplot A logical argument indicating if a regular HDR plot is required
#' (\code{FALSE}), or if a scatterplot in the same colors is required (\code{TRUE}).
#' @param color The base color to use for the mode. Colors for the HDRs are generated
#' by whitening this color.
#' @param show_lookout A logical argument indicating if the plot should highlight observations with "lookout"
#' probabilities less than 0.05.
#' @param ... Other arguments passed to \code{\link[ks]{kde}}.
#' @return A ggplot object showing an HDR plot or scatterplot of the data.
#' @author Rob J Hyndman
#' @references Hyndman, R J (1996) Computing and Graphing Highest Density Regions,
#' *The American Statistician*, **50**(2), 120–126. \url{https://robjhyndman.com/publications/hdr/}
#' Kandanaarachchi, S & Hyndman, R J (2022) "Leave-one-out kernel density estimates for outlier detection",
#' *J Computational & Graphical Statistics*, **31**(2), 586-599. \url{https://robjhyndman.com/publications/lookout/}
#' @examples
#' df <- data.frame(x = c(rnorm(1000), rnorm(1000, 5, 1)))
#' df$y <- df$x + rnorm(200, sd = 2)
#' gg_hdrboxplot(df, x)
#' gg_hdrboxplot(df, x, y, scatterplot = TRUE)
#' oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   gg_hdrboxplot(duration, waiting, scatterplot = TRUE)
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   gg_hdrboxplot(Average)
#'
#' @rdname hdrplot
#' @export

gg_hdrboxplot <- function(data, var1, var2 = NULL, prob = c(0.5, 0.99),
                          color = "#00659e", scatterplot = FALSE,
                          show_lookout = FALSE, ...) {
  if (missing(var1)) {
    # Grab first variable
    data <- as.data.frame(data)
    var1 <- rlang::sym(names(data)[1])
    if (NCOL(data) > 1L) {
      message("No variable selected. Using ", rlang::as_name(var1))
    }
  }
  v2 <- dplyr::as_label(dplyr::enquo(var2))
  if (v2 == "NULL") {
    d <- 1L
    data <- data |> select({{ var1 }})
  } else {
    d <- 2L
    data <- data |> select({{ var1 }}, {{ var2 }})
  }
  dist <- dist_kde(data[,seq(d)], method = "double")

  if (d == 2L) {
    gg_density2(dist,
      prob = prob,
      colors = NULL, color = color, fill = !scatterplot, alpha = NULL,
      show_points = TRUE, show_mode = TRUE, scatterplot = scatterplot,
      show_lookout = show_lookout
    ) +
    ggplot2::guides(fill = "none", color = "none")

  } else {
    gg_density1(dist,
      show_hdr = TRUE, show_density = FALSE, ngrid = 501,
      prob = prob, alpha = NULL, jitter = TRUE,
      color = color, fill = TRUE, show_points = TRUE, show_mode = TRUE,
      show_lookout = show_lookout, ...
    ) +
      ggplot2::guides(alpha = "none") +
      ggplot2::scale_y_continuous(breaks = NULL) +
      labs(y = "", x = names(data)[1])
  }
}


#' @importFrom utils head tail
#' @importFrom tibble tibble
utils::globalVariables(c("ends", "type", "lower", "upper", "group"))
