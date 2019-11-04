#' @title Function for simple boxplot
#' for statistical analysis
#' @name m_boxplot
#' @param x Variable to use for x axis
#' @param y Variable to use for y axis
#' @param data Data set to use
#' @import ggplot2
#' @export

# require(assertthat)
# require(ggplot2)

m_boxplot <- function(x, y, data) {

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.data.frame(x))
  assertthat::assert_that(is.data.frame(y))

  ggplot(data = data, mapping = aes(x = x, y = y))+
    geom_boxplot()+
    theme_bw()+
    geom_point(size = 1, position = position_jitter(width = 0.05), color = "blue")

}
