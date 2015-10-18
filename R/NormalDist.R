#' Given a mean (\code{mu}) and standard deviation (\code{s}), plot 
#' the normal distribution.
#'
#' @title Plot a normal distribution
#' 
#' @param mu numeric. The mean.
#' @param s numeric. The standard deviation.
#' 
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (require(manipulate)) {
#'   manipulate(normal_dist(mu, s),
#'              mu = slider(-10, 10),
#'              s = slider(0.01, 10))
#' }}
NormalDist <- function(mu, s){
  x <- seq(mu - 3 * s, mu + 3 * s, by = 0.1)
  curve(dnorm(x, mu, sd = s),
    n = 201,
    col = "red", lwd = 2,
    xlab = "Y", ylab = "Probability",
    xaxs = "i", yaxs = "i",
    xlim = c(-20, 20), ylim = c(0, 0.45),
    cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
    mgp = c(2.5, 0.8, 0))
    text(-18, 0.4, bquote(mu == .(mu)), adj = 0)
    text(-18, 0.35, bquote(sigma == .(s)), adj = 0)
}
