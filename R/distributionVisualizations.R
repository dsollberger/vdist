#' Add together two numbers.
#'
#' @param a left-endpoint
#' @param b right-endpoint
#' @return ggplot 2 graph object
#' @examples
#' vnorm(b = 1)
#' vnorm(a = 1.5, fill = "blue", lower.tail = FALSE)

vnorm <- function(a = -3, b = 3, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE, fill = "red"){

  # initialize variables
  x <- seq(min(a, mean - 3*sd), max(b, mean + 3*sd), 0.01)
  y <- dnorm(x, mean, sd)
  df <- data.frame(x,y)

  if(lower.tail){
    shade <- rbind( c(min(x), 0), subset(df, x < b), c(b, 0))
  } else {
    shade <- rbind( c(a, 0), subset(df, x > a), c(max(x), 0))
  }

  ggplot(df, aes(x, y)) +
    geom_line() +
    geom_polygon(data = shade, aes(x,y), fill = fill) +
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
