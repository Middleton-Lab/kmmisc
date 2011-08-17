\name{ChisqDist}
\alias{ChisqDist}
\title{Plot a Chi-squared distribution}
\usage{
  ChisqDist(df)
}
\arguments{
  \item{df}{Degrees of freedom}
}
\description{
  Given the degrees of freedom (\code{df}), plot a
  chi-squared distribution.
}
\examples{
\dontrun{
if (require(manipulate)) {
manipulate( chisq_dist(df), df = slider(1, 20) )
}}
}
\author{
  Kevin Middleton
}

