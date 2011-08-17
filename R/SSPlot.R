##' This function graphically displays the sum of squares for a linear
##' regression with a given slope (b). If the manipulate function is
##' available, an interactive plot can be produced.
##'
##' @title Sum of Squares Plot
##'
##' @param X Vector of x values
##' @param Y Vector of y values
##' @param b Slope for the regression
##' 
##' @author Kevin Middleton
##'
##' @export
##' 
##' @examples
##' n <- 30
##' X <- rnorm(n, mean = 10)
##' Y <- 2.4 * X + rnorm(n, mean = 1)
##' b <- 2
##' SSPlot(X, Y, b)
##'
##' \dontrun{
##' if (require(manipulate)) {
##'   manipulate(SSPlot(X, Y, b), b = slider(-10, 10, step = 0.1))
##' }
##' }
SSPlot <- function(X, Y, b){
  n <- length(X)
  SSy <- sum((Y - (X * b + (mean(Y) - b * mean(X))))^2)
  par(cex.lab = 2, cex.main = 2, mgp = c(2.5, 0.5, 0))
  plot(X, Y, type = "n",
       main = paste("b =", sprintf("%.2f", b), "\nSS =", 
         sprintf("%.2f", SSy)))
  points(mean(X), mean(Y), pch = 1, cex = 4, col = "blue")
  abline(a = mean(Y) - b * mean(X), b = b, col = "blue")
  for (i in 1:n){
    segments(X[i], Y[i], X[i], X[i] * b + (mean(Y) - b * mean(X)), 
             col = "red")
  }
  points(X, Y, pch = 16)
}
