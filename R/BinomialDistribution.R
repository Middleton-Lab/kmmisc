#' Given the probability of success \code{p} and the number of trials
#' \code{n}, this function plots the resulting binomial distribution.
#'
#' @title Plot of a binomial distribution
#'
#' @param p Numeric. Probability of success for each trial
#' @param n Numeric. Number of trials
#'
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (require(manipulate)){
#'   manipulate( BinomialDist(n = n), n = slider(2, 50) )
#'   manipulate( BinomialDist(p = p), p = slider(0, 1) )
#'   manipulate( BinomialDist(p = p, n = n),
#'                            p = slider(0, 1),
#'                            n = slider(2, 50) )
#' }
#' }
BinomialDist <- function(p = 0.5, n = 18) {
  barchart( dbinom(0:n, n, p) ~ as.factor(0:n),
                   horizontal = FALSE,
                   ylab = "Probablility",
                   xlab = "X Successes",
                   scales = list(axs = "i"))
}
