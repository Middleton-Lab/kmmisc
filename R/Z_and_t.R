##' Plot the relationship between the Z and t distributions.
##' 
##' @title Plot the relationship between the Z and t distributions.
##' 
##' @param n numeric. Number of observations.
##' 
##' @author Kevin Middleton
##'
##' @export
##'
##' @examples
##' \dontrun{
##' if (require(manipulate)) {
##'   manipulate( Z_from_t(n), n = slider(2, 50) )
##' }}
Z_and_t <- function (n){
  df <- n - 1
  curve(dnorm(x), from = -4, to = 4,
        lwd = 8,
        mgp = c(2.5, 0.75, 0),
        cex.main = 2,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Probability",
        xlab = paste("df =", df),
        main = paste("n =", n))
  legend("topright", c("Z", "t"), col = c("black", "red"), lwd = 2)
  x <- seq(-4, 4, length = 100)
  y <- dt(x, df = df)
  lines(x, y, col = "red", lwd = 6)
}

