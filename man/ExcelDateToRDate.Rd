\name{ExcelDateToRDate}
\alias{ExcelDateToRDate}
\title{ExcelDateToRDate}
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
  Convert Excel Dates to R Dates
}
\details{
  This function converts numeric Excel dates to R dates
  using the appropriate offset for Excel's starting from
  1899-12-30.
}
\references{
  See \url{http://markmail.org/message/zcydoj6nvtqe57xl}
}
\keyword{data}

