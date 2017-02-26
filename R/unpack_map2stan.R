#' Unpack map or map2stan fitted model
#'
#' @param fm Fitted model
#'
#' @return data.frame of fm
#' @export
#'
unpack_map2stan <- function(fm) {
  post <- extract.samples(fm)
  p <- precis(fm, depth = 2)@output
  param_est <- rownames(p)

  out <- list()
  counter <- 1
  for (i in 1:length(post)) {
    param <- post[[i]]
    if (is.matrix(param)) {
      for (j in 1:ncol(param)) {
        out[[counter]] <- param[, j]
        counter <- counter + 1
      }
    } else if (is.array(param)) {
      out[[counter]] <- param
      counter <- counter + 1
    } else {
      stop("column(s) not matrix or array")
    }
  }
  out_df <- as.data.frame(out)
  names(out_df) <- param_est
  out_df <- apply(out_df, 2, as.numeric)
  return(as.data.frame(out_df))
}
