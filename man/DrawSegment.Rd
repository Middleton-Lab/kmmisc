\name{DrawSegment}
\alias{DrawSegment}
\title{Draw a line segment}
\usage{
  DrawSegment(sl, int, x, xlog = FALSE, lty = 1, lwd = 2)
}
\arguments{
  \item{sl}{slope of the line}

  \item{int}{intercept of the line}

  \item{x}{vector. x values}

  \item{xlog}{logical. Should x values be logged?}

  \item{lty}{optional parameter for line type}

  \item{lwd}{optional parameter for line width}
}
\description{
  Draws a line segment of a given slope and intercept only
  over the range of data present in vector x.
}
\examples{
set.seed(5)
x <- rnorm(20)
y <- 2 * x + rnorm(20)
fm <- lm(y ~ x)

plot(x, y)
DrawSegment(coef(fm)[2], coef(fm)[1], x)
}
\author{
  Kevin Middleton
}

