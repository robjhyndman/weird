#' @title Bagplot
#' @description Produces a bivariate bagplot. A bagplot is analagous to a
#' univariate boxplot, except it is in two dimensions. Like a boxplot, it
#' shows the median, a region containing 50% of the observations, a region
#' showing the remaining observations other than outliers, and any outliers.
#'
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 The name of the second variable to plot (a bare expression).
#' @param col The main color to use. The median is shown at full strength, while the
#' bag and loop have some transparency added.
#' @param ... Other arguments currently ignored.
#' @return A ggplot object showing a bagplot of the data.
#' @author Rob J Hyndman
#' @references Rousseeuw, P. J., Ruts, I., & Tukey, J. W. (1999).
#'   The bagplot: A bivariate boxplot. The American Statistician, 52(4), 382–387.
#' @examples
#' gg_bagplot(n01, v1, v2)
#' @rdname bagplot
#' @seealso
#'  \code{\link[aplpack]{bagplot}}
#' @importFrom aplpack compute.bagplot
#' @importFrom ggplot2 geom_polygon geom_point ggplot aes
#' @importFrom dplyr select
#' @export

gg_bagplot <- function(data, var1, var2, col="#0072B2", ...) {
  data <- data %>% select({{var1}},{{var2}})
  bp <- aplpack::compute.bagplot(as.matrix(data))
  loop <- as.data.frame(bp$hull.loop)
  bag <- as.data.frame(bp$hull.bag)
  cn <- colnames(data)
  p <- data %>%
    ggplot(aes(x={{ var1 }}, y={{ var2 }}))
  # Show loop polygon
  if(!is.null(loop)) {
    colnames(loop) <- cn
    p <- p + geom_polygon(aes(x={{ var1 }}, y={{ var2 }}), data = loop, fill = col, alpha=0.2)
  }
  # Show bag polygon
  if(!is.null(bag)) {
    colnames(bag) <- cn
    p <- p + geom_polygon(aes(x={{ var1 }}, y={{ var2 }}), data = bag, fill = col, alpha=0.4)
  }
  # Show points
  #if(show_points) {
  #  p <- p + geom_point(col="gray", size=0.5)
  #}
  # Show outliers
  if(!is.null(bp$pxy.outlier)) {
    outliers <- as.data.frame(as.matrix(bp$pxy.outlier))
    colnames(outliers) <- cn
    p <- p + geom_point(aes(x={{var1}}, y={{var2}}), data=outliers)
  }
  # Show median
  p <- p + geom_point(aes(x=bp$center[1], y=bp$center[2]), col=col, size=2)
  return(p)
}
