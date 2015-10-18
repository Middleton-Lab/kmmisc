#' Plot a regression line not using abline().
#' 
#' @title Plot a linear regression line
#' @param x vector. x values.
#' @param lm.obj object of class lm containing the results of a 
#'   linear regression
#' @param ylog logical. Should the y values be logged?
#' @param lty optional parameter for line type
#' @param lwd optional parameter for line width
#'  
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#' 
#' @examples
#' set.seed(5)
#' x <- rnorm(20)
#' y <- 2 * x + rnorm(20)
#' fm <- lm(y ~ x)
#' plot(x, y)
#' abline(fm)
#'
#' plot(x, y)
#' DrawRegression(x, fm)
DrawRegression <- function(x,
                           lm.obj,
                           ylog = FALSE,
                           lty = 1,
                           lwd = 2) {	
  if (ylog == TRUE) {
      xmin <- min(log10(x))
      xmax <- max(log10(x))
      ymin <- min(predict(lm.obj))
      ymax <- max(predict(lm.obj))
      segments(10^xmin, 10^ymin, 10^xmax, 10^ymax, lty = lty, lwd = lwd)
    } else {
    xmin <- min(x)
    xmax <- max(x)
    ymin <- min(predict(lm.obj))
    ymax <- max(predict(lm.obj))
    segments(xmin, ymin, xmax, ymax, lty = lty, lwd = lwd)
  }
}
