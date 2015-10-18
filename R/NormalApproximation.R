#' For a probability of success (\code{p}), plot the normal 
#' approximation of a binomial distribution.
#'
#' @title Normal approximation of a binomial distribution
#' 
#' @param p numeric. Probability of success of a trial.
#' 
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (require(manipulate)) {
#'   manipulate( normal_approx(p),
#'              p = slider(0.1, 0.9) )
#' }}
NormalApproximation <- function (p){
  i <- 30
  plot(dbinom(1:i, i, p), type = "h",
    lwd = 5, cex.main = 2, cex.axis = 1.4, cex.lab = 1.4,
    ylab = "Probability", xlab = paste("p =", p),
    ylim = c(0, 0.20), main = paste("n =", i))
  ybar <- i * p
  ysd <- sqrt(i * p * (1-p))
  x <- seq(0, 60, length = 200)
  y <- dnorm(x, mean = ybar, sd = ysd)
  lines(x, y, col = "red", lwd = 3)
}
