\name{ExcelDateToRDate}
\alias{ExcelDateToRDate}
\title{Convert Excel Dates to R Dates}
\usage{
  ExcelDateToRDate(x)
}
\arguments{
  \item{x}{vector of Excel (numeric) dates}
}
\value{
  x converted to R's date class.
}
\description{
  This function converts numeric Excel dates to R dates
  using the appropriate offset for Excel's starting from
  1899-12-30.
}
\author{
  Kevin Middleton (\email{middletonk@missouri.edu})
}
\references{
  See \url{http://markmail.org/message/zcydoj6nvtqe57xl}
}
\keyword{data}

