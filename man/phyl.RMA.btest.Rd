\name{phyl.RMA.btest}
\alias{phyl.RMA.btest}
\title{Slope tests for phylogenetic RMA}
\usage{
  phyl.RMA.btest(x, y, tree, b0 = 0, n = 1000,
    makeplot = TRUE)
}
\arguments{
  \item{x}{x variable}

  \item{y}{y variable}

  \item{tree}{Object of class phylo}

  \item{b0}{Slope to test against. Defaults to 0.}

  \item{n}{Number of iterations for randomization. Defaults
  to 1000.}

  \item{makeplot}{Should a plot be produced. Defualts to
  \code{TRUE}.}
}
\value{
  List containing \item{p}{P-value determined by
  randomization} \item{ts}{a vector of t-sttistics}
}
\description{
  Performs hypothesis testing for slopes given a
  phylogenetic RMA.
}
\examples{
\dontrun{
nTaxa <- 50
set.seed(6)
tree <- rtree(nTaxa)
x <- rnorm(nTaxa)
y <- 2 * x + rnorm(nTaxa, sd = 0.1)
names(x) <- tree$tip.label
names(y) <- tree$tip.label
# The next step is slow, so don't run it.
require(phytools)
phyl.RMA.btest(x, y, tree, b0 = 2, n = 50)}
}
\author{
  Kevin Middleton
}
\seealso{
  \code{\link[phytools]{phyl.RMA}}
}

