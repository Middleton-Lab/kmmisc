#' This function converts numeric Excel dates to R dates using the
#' appropriate offset for Excel's starting from 1899-12-30.
#'
#' @title Convert Excel Dates to R Dates
#'
#' @param x vector of Excel (numeric) dates
#'
#' @return x converted to R's date class.
#'
#' @references See \url{http://markmail.org/message/zcydoj6nvtqe57xl}
#'
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @keywords data
#'
#' @export
#'
ExcelDateToRDate <- function(x) {
  x_as_date <- as.Date(x, origin = "1899-12-30")
  return(x_as_date)
}
