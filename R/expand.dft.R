##' This function expands a \code{data.frame} based on a column 
##' (\code{col.exp}). Useful for generating long tables from 
##' contingency tables.
##' 
##' @title Expand a data.frame
##' 
##' @param x a \code{data.frame}
##' @param col.exp a character string of the column name or the 
##' column number to use for "expansion"
##' @param na.strings passed to \code{type.convert}
##' @param as.is passed to \code{type.convert}
##' @param dec passed to \code{type.convert}
##' 
##' @return DF a \code{data.frame} of \code{x} in "long" format.
##' 
##' @references See
##'   \url{https://stat.ethz.ch/pipermail/r-help/2009-January/185561.html}
##'   for discussion of \code{expand.dft()}.
##' 
##' @author Modified for the \code{abd} package by Kevin Middleton 
##'   from a function posted by Marc Schwartz to the r-help mailing 
##'   list.
##' 
##' @seealso \code{\link{table}}
##' 
##' @examples
##' OCAD <- data.frame(Sex = c("Male", "Female", "Male", "Female"),
##'                    Status = c("Present", "Present", "Absent", "Absent"),
##'                    Count = c(92, 15, 21, 20))
##' OCAD <- expand.dft(OCAD, col.exp = "Count")
##' table(OCAD)
##' 
##' @export
##' 
expand.dft <- function(x, 
                       col.exp, 
                       na.strings = "NA", 
                       as.is = FALSE, 
                       dec = ".") {
  # Error checking
  if (missing(x)) {
    stop("x is missing. You must supply a data.frame.")
  }
  if (missing(col.exp)) {
    stop("col.exp is missing You must supply a column name to expand.")
  }
  if (class(x) != "data.frame") {
    stop("x must be a data.frame.")
  }
  if (class(col.exp) != "character") {
    if (class(col.exp) != "numeric") {
      stop("col.exp must be character or numeric.")
    }
  }
  
  # Get the column name if col.exp is numeric
  if (class(col.exp) == "numeric") col.exp <- names(x)[col.exp]
  
  # Frequencies to expand via rep()
  to.expand <- x[, col.exp]
  
  DF <- sapply(1:nrow(x), function(i) x[rep(i, each = to.expand[i]), ], 
               simplify = FALSE)
  DF <- subset(do.call("rbind", DF), select = -(get(col.exp)))
  for (i in 1:ncol(DF)) {
    DF[[i]] <- type.convert(as.character(DF[[i]]),
                            na.strings = na.strings, as.is = as.is, dec = dec)
  }
  DF
} 
