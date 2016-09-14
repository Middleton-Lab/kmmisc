#' Given the degrees of freedom (\code{df}), plot a chi-squared
#' distribution.
#'
#' @title Plot a Chi-squared distribution
#'
#' @param df Degrees of freedom
#'
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (require(manipulate)) {
#'   manipulate( chisq_dist(df), df = slider(1, 20) )
#' }}
ChisqDist <- function (df) {
  crit <- qchisq(0.95, df)
  curve(dchisq(x, df),
        from = 0,
        to = 40,
        xlab = expression(chi ^ 2),
        ylab = "Probability",
        main = paste("df =", df, "  Critical value =",
          format(crit, digits = 3)),
        lwd = 2,
        xaxs = "i",
        yaxs = "i",
        cex.axis = 1.5,
        cex.lab = 1.5,
        cex.main = 1.5,
        mgp = c(2.5, 0.8, 0))
  x <- seq(crit, 40, length = 100)
  y <- dchisq(x, df)
  polygon(c(x[1], x, x[100]), c(0, y, 0),
          col = "red",
          border = NA)
}
