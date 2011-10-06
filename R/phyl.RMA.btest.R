phyl.RMA.t <- function(x,
                       y,
                       tree,
                       b0 = 0) {
  b <- phyl.RMA(x, y, tree)$RMA.beta[2]
  fm <- lm(y ~ x)
  summary.fm <- summary(fm)
  se.b <- summary.fm$coefficients[2, 2]
  t <- (b - b0) / se.b
  return(t)
}

##' Performs hypothesis testing for slopes given a phylogenetic RMA.
##'
##' @title Slope tests for phylogenetic RMA
##' @param x x variable
##' @param y y variable
##' @param tree Object of class phylo
##' @param b0 Slope to test against. Defaults to 0.
##' @param n Number of iterations for randomization. Defaults to 1000.
##' @param makeplot Should a plot be produced. Defualts to \code{TRUE}.
##' @return List containing
##' \item{p}{P-value determined by randomization}
##' \item{ts}{a vector of t-sttistics}
##' 
##' @author Kevin Middleton
##' @seealso \code{\link[phytools]{phyl.RMA}}
##'
##' @export
##'
##' @examples
##' \dontrun{
##' nTaxa <- 50
##' set.seed(6)
##' tree <- rtree(nTaxa)
##' x <- rnorm(nTaxa)
##' y <- 2 * x + rnorm(nTaxa, sd = 0.1)
##' names(x) <- tree$tip.label
##' names(y) <- tree$tip.label
##' # The next step is slow, so don't run it.
##' require(phytools)
##' phyl.RMA.btest(x, y, tree, b0 = 2, n = 50)}
##' 
phyl.RMA.btest <- function(x,
                           y,
                           tree,
                           b0 = 0,
                           n = 1000,
                           makeplot = TRUE) {
  ts <- numeric(length = n)
  for (i in seq_len(n)){
    yrand <- sample(y)
    names(yrand) <- tree$tip.label
    ts[i] <- phyl.RMA.t(x, yrand, tree, b0)
  }
  ts[1] <- phyl.RMA.t(x, y, tree, b0)
  ts <- abs(ts)
  p <- mean(ts[1] > ts)
  if (makeplot) {
    hist(btest$ts)
    abline(v = btest$ts[1], col = "red")
  }
  return(list(p = p, ts = ts))
}
