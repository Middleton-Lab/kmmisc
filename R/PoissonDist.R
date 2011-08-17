##' Plot a Poisson distribution given the mean number of successes per
##' interval (\code{lambda}).
##'
##' @title Plot a Poisson distribution.
##' 
##' @param lambda Numeric. Mean number of successes per interval.
##' 
##' @author Kevin Middleton
##'
##' @export
##'
##' @examples
##' \dontrun{
##' if require(manipulate){
##'   manipulate( poisson_dist(lambda), lambda = slider(0, 30) )
##' }
##' }
PoissonDist <- function (lambda){
  nmax <- 30
  barplot(dpois(0:nmax, lambda = lambda),
          names.arg = 0:nmax,
          ylim = c(0, 0.4),
          col = "red",
          xlab = "X Successes",
          ylab = "Probability",
          main = bquote(mu == .(lambda)), 
          xaxs = "i",
          yaxs = "i",
          cex.axis = 1.5,
          cex.lab = 1.5,
          cex.main = 2,
          mgp = c(2.5, 0.8, 0))
}
