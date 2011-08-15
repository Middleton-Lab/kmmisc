##' Draws a line segment of a given slope and intercept only over the
##' range of data present in vector x.
##' 
##' @title Draw a line segment
##' 
##' @param sl slope of the line
##' @param int intercept of the line
##' @param x vector. x values
##' @param xlog logical. Should x values be logged?
##' @param lty optional parameter for line type
##' @param lwd optional parameter for line width
##' 
##' @author Kevin Middleton
##'
##' @examples
##' set.seed(5)
##' x <- rnorm(20)
##' y <- 2 * x + rnorm(20)
##' fm <- lm(y ~ x)
##'
##' plot(x, y)
##' DrawSegment(coef(fm)[2], coef(fm)[1], x)
##'
##' @export
DrawSegment <- function(sl,
                        int,
                        x,
                        xlog = FALSE,
                        lty = 1,
                        lwd = 2){
  if (xlog == TRUE) {
      xmin <- min(log10(x))
      xmax <- max(log10(x))
      ymin <- sl*xmin + int
      ymax <- sl*xmax + int
      segments(10^xmin, 10^ymin, 10^xmax, 10^ymax, lty = lty, lwd = lwd)
    } else {
    xmin <- min(x)
    xmax <- max(x)
    ymin <- sl*xmin + int
    ymax <- sl*xmax + int   
    segments(xmin, ymin, xmax, ymax, lty = lty, lwd = lwd)
  }
}
