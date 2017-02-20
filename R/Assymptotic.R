#' Assymptotic equivalence of RMA and phyl.RMA()
#'
#' @title Assymptotic equivalence of RMA and phyl.RMA()
#'
#' @export
#'
#' @importFrom phytools phyl.RMA
#' @importFrom caper comparative.data pgls
#' @importFrom smatr sma
#' @importFrom ape compute.brlen rtree
#'
Assymptotic <- function(){
  set.seed(10)

  tr <- ape::rtree(10)
  tr <- ape::compute.brlen(tr, power = 0.001)
  plot(tr)

  x <- rnorm(10)
  y <- rnorm(10)

  names(x) <- tr$tip.label
  names(y) <- tr$tip.label
  phyl.RMA(x, y, tr)$RMA.beta

  M <- data.frame(x, y, species = tr$tip.label)
  cd <- caper::comparative.data(phy = tr, data = M, "species")
  caper::pgls(y ~ x, cd)

  smatr::sma(y ~ x)
}
