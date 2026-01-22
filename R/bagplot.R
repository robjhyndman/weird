#' @title Bagplot
#' @description Produces a bivariate bagplot. A bagplot is analagous to a
#' univariate boxplot, except it is in two dimensions. Like a boxplot, it
#' shows the median, a region containing 50% of the observations, a region
#' showing the remaining observations other than outliers, and any outliers.
#'
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 The name of the second variable to plot (a bare expression).
#' @param show_points A logical argument indicating if a regular bagplot is required
#' (\code{FALSE}), or if a scatterplot in the same colors is required (\code{TRUE}).
#' @param color The base color to use for the median. Other colors are generated
#' as a mixture of `color` with white.
#' @param ... Other arguments are passed to the \code{\link[aplpack]{compute.bagplot}} function.
#' @return A ggplot object showing a bagplot or scatterplot of the data.
#' @author Rob J Hyndman
#' @references Rousseeuw, P. J., Ruts, I., & Tukey, J. W. (1999).
#'   The bagplot: A bivariate boxplot. \emph{The American Statistician}, \bold{52}(4), 382–387.
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 5.6,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' gg_bagplot(n01, v1, v2)
#' gg_bagplot(n01, v1, v2, show_points = TRUE)
#' @rdname bagplot
#' @seealso
#'  \code{\link[aplpack]{bagplot}}
#' @importFrom aplpack compute.bagplot
#' @importFrom ggplot2 geom_polygon geom_point ggplot aes
#' @importFrom dplyr select filter
#' @export

gg_bagplot <- function(
  data,
  var1,
  var2,
  color = "#00659e",
  show_points = FALSE,
  ...
) {
  col <- c(hdr_palette(color = color, prob = c(0.5, 0.99)), "#000000")
  data <- data |> select({{ var1 }}, {{ var2 }})
  bp <- aplpack::compute.bagplot(
    as.matrix(data),
    na.rm = TRUE,
    approx.limit = 1000,
    ...
  )
  cn <- colnames(data)
  p <- data |>
    ggplot(aes(x = {{ var1 }}, y = {{ var2 }}))
  if (show_points) {
    # Bag points
    if (!is.null(bp$pxy.bag)) {
      p <- p +
        geom_point(
          aes(x = {{ var1 }}, y = {{ var2 }}),
          data = as.data.frame(bp$pxy.bag),
          color = col[2]
        )
    }
    # Loop points
    if (!is.null(bp$pxy.outer)) {
      p <- p +
        geom_point(
          aes(x = {{ var1 }}, y = {{ var2 }}),
          data = as.data.frame(bp$pxy.outer),
          color = col[3]
        )
    }
    # Deepest point
    colnames(bp$xy) <- cn
    deep <- bp$xy |>
      as.data.frame() |>
      dplyr::filter(bp$hdepths == max(bp$hdepths))
    p <- p +
      geom_point(
        aes(x = {{ var1 }}, y = {{ var2 }}),
        data = deep,
        color = col[1]
      )
  } else {
    loop <- as.data.frame(bp$hull.loop)
    bag <- as.data.frame(bp$hull.bag)
    # Show loop polygon
    if (!is.null(loop)) {
      colnames(loop) <- cn
      p <- p +
        geom_polygon(
          aes(x = {{ var1 }}, y = {{ var2 }}),
          data = loop,
          fill = col[3]
        )
    }
    # Show bag polygon
    if (!is.null(bag)) {
      colnames(bag) <- cn
      p <- p +
        geom_polygon(
          aes(x = {{ var1 }}, y = {{ var2 }}),
          data = bag,
          fill = col[2]
        )
    }
  }
  if (!is.null(bp$pxy.outlier)) {
    outliers <- as.data.frame(as.matrix(bp$pxy.outlier))
    colnames(outliers) <- cn
    p <- p +
      geom_point(
        aes(x = {{ var1 }}, y = {{ var2 }}),
        data = outliers,
        col = col[4]
      )
  }
  if (!show_points) {
    # Show median
    p <- p +
      geom_point(
        data = data.frame(x = bp$center[1], y = bp$center[2]),
        aes(x = x, y = y),
        col = col[1],
        size = 2
      )
  }
  return(p)
}
