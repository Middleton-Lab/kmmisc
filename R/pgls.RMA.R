##' Phylogenetic RMA regression
##'
##' This function performs phylogenetic RMA regression on an object of
##' class \code{pgls}. Confidence intervals and hypothesis tests for
##' arbitrary slopes are provided.
##'
##' @title Phylogenetic RMA regression
##'
##' @aliases pgls.RMA print.pgls.RMA
##'
##' @param object an object containing the results returned by
##'   \code{pgls()}.
##' @param h0 Null slope for hypothesis test. Defaults to 1.
##' @param param.CI Level for confidence interval. Defaults to 0.95.
##' @return an object of class \code{pgls.rma}. Containing:
##' \item{b.RMA}{RMA slope estimate}
##' \item{lower}{lower bound of the confidence interval}
##' \item{upper}{upper bound of the confidence interval}
##' \item{b0}{intercept estimate}
##' \item{param.CI}{level for confidence interval}
##' \item{h0}{null slope for hypothesis test}
##' \item{df}{non-phylogenetic degrees of freedom}
##' \item{df_phyl}{phylogenetic degrees of freedom}
##' \item{t}{value of t-statistic for slope hypothesis test}
##' \item{P}{P-value for slope hypothesis test}
##'
##' @author Kevin Middleton (\email{middletonk@@missouri.edu}) with
##'   code modified from \code{phytools::phyl.RMA()} written by Liam
##'   Revell.
##' @export pgls.RMA
##'
pgls.RMA <- function(object, h0 = 1, param.CI = 0.95){
  if (!inherits(object,"pgls")){
    stop("'object' must be of class 'pgls'.")
  }

  r2 <- summary(object)$r.squared

  yvar <- all.vars(terms(object$formula))[1]
  xvar <- all.vars(terms(object$formula))[2]
  y <- object$y
  x <- object$x[, xvar]
  names(y) <- object$data$phy$tip.label
  names(x) <- object$data$phy$tip.label

  rma.betas <- phyl.RMA(x, y, object$data$phy)$RMA.beta

  # RMA betas
  b0 <- rma.betas[1]
  b1 <- rma.betas[2]

  # SEb1
  SEb1 <- as.numeric(object$sterr[2])

  # df
  df <- summary(object)$df[2]
  df_phyl <- 2 + (length(object$data$phy$tip.label) - 2) /
    (1 + 0.5 * r2)

  # CI
  lower <- b1 - qt(1 - (1 - param.CI)/2, df = df_phyl) * SEb1
  upper <- b1 + qt(1 - (1 - param.CI)/2, df = df_phyl) * SEb1

  # Test vs. h0
  # Following phytools::phyl.RMA()
  t <- abs(log(abs(b1)) - log(abs(h0))) /
    sqrt((1 - r2) / (length(object$data$phy$tip.label) - 2))
  P <- 2 * pt(t, df = df_phyl, lower.tail = FALSE)

  outlist <- list(slope.RMA = b1,
                  lower = lower,
                  upper = upper,
                  intercept = b0,
                  param.CI = param.CI,
                  h0 = h0,
                  df = df,
                  df_phyl = df_phyl,
                  t = t,
                  P = P)
  class(outlist) <- "pgls.RMA"
  return(outlist)
}

print.pgls.RMA <- function(x, digits = 4, ...){
  cat("\n")
  cat("PGLS RMA\n")
  cat("\n")
  cat(x$param.CI, "Confidence Interval\n")
  cat("Lower\t Beta\t Upper\n")
  cat(format(x$lower, digits = digits), "\t",
      format(x$slope.RMA, digits = digits), "\t",
      format(x$upper, digits = digits), "\n")
  cat("\n")
  cat("Intercept =", x$intercept, "\n")
  cat("\n")
  cat("h0\t df\t t\t P\n")
  cat(format(x$h0, digits = digits), "\t",
      format(x$df_phyl, digits = digits), "\t",
      format(x$t, digits = digits), "\t",
      format(x$P, digits = digits), "\n\n")
}
