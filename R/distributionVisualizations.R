#' makes ggplot of normal distribution
#'
#' @param x data point
#' @return ggplot 2 graph object
#' @examples
#' vnorm(x = 1)
#' vnorm(x = 1.5, fill = "blue", lower.tail = FALSE)

vnorm <- function(x = 0, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE, fill = "red"){

  # initialize variables
  if(lower.tail){
    xvals <- seq(min(x, mean - 3*sd), max(x, mean + 3*sd), 0.01)
  } else {
    xvals <- seq(min(x, mean - 3*sd), max(x, mean + 3*sd), 0.01)
  }

  y <- dnorm(xvals, mean, sd)
  df <- data.frame(xvals,y)

  if(lower.tail){
    shade <- rbind( c(min(xvals), 0), subset(df, xvals < x), c(x, 0))
  } else {
    shade <- rbind( c(x, 0), subset(df, xvals > x), c(max(xvals), 0))
  }

  # returns this ggplot object
  ggplot(df, aes(xvals, y)) +
    geom_line() +
    geom_polygon(data = shade, aes(xvals,y), fill = fill) +
    xlab("x") +
    ylab("probabiliity")
}


# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
