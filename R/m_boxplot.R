#' @title Function for simple boxplot
#'   for statistical analysis
#' @name m_boxplot
#' @param x factor vector for catigorical variable on x axis
#' @param y vector for response variable on y axis
#' @param labs vector of two elements for axis labels
#'   (c("x label", "y label))
#' @import ggplot2
#' @export

# require(assertthat)
# require(ggplot2)

m_boxplot <- function(x, y, labs = c("","")) {

  assertthat::assert_that(is.factor(x))
  assertthat::assert_that(is.vector(y))
  assertthat::are_equal(length(x), length(y))
  assertthat::assert_that(is.vector(labs))
  assertthat::assert_that(length(labs) == 2)

  # display sample sizes in variable names for each category
  lvls <- levels(x)
  names_with_N <- vector(mode = "character", length = length(lvls))
  for(pos in 1:length(lvls)) {
    name = lvls[pos]
    n <- length(x[which(x == name)])
    names_with_N[pos] <- paste0(name, " (", "n = ", n, ")")
  }

  ggplot(mapping = aes(x = x, y = y))+
    geom_boxplot()+
    scale_x_discrete(labels = names_with_N)+
    theme_bw()+
    geom_point(size = 1, position = position_jitter(width = 0.05), color = "blue")+
    labs(x = labs[1], y = labs[2])

}
