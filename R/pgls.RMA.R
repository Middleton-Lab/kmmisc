pgls.RMA <- function(fm, h0 = 1, param.CI = 0.95){
  if (!inherits(fm,"pgls")){
    stop("'fm' must be of class 'pgls'.")
  }
  
  r2 <- summary(fm)$r.squared
  
  # RMA b
  b <- as.numeric(coef(fm)[2] / sqrt(r2))
  
  # SEb
  SEb <- as.numeric(fm$sterr[2])
  
  # df
  df <- summary(fm)$df[2]
  df_phyl <- 2 + (length(fm$data$phy$tip.label) - 2) / 
    (1 + 0.5 * r2)
  
  # CI
  lower <- b - qt(1 - (1 - param.CI)/2, df = df_phyl) * SEb
  upper <- b + qt(1 - (1 - param.CI)/2, df = df_phyl) * SEb
  
  # Test vs. h0
  # Following phytools::phyl.RMA()
  t <- abs(log(abs(b)) - log(abs(h0))) / 
    sqrt((1 - r2) / (length(fm$data$phy$tip.label) - 2))
  P <- 2 * pt(t, df = df_phyl, lower.tail = FALSE)
  
  outlist <- list(b.RMA = b,
                  lower = lower,
                  upper = upper,
                  param.CI = param.CI,
                  h0 = h0,
                  df = df,
                  df_phyl = df_phyl,
                  t = t,
                  P = P)
  class(outlist) <- "pgls.RMA"
  return(outlist)
}

print.pgls.RMA <- function(x, digits = 4, ...){
  cat("\n")
  cat("PGLS RMA\n")
  cat("\n")
  cat(x$param.CI, "Confidence Interval\n")
  cat("Lower\t Beta\t Upper\n")
  cat(format(x$lower, digits = digits), "\t",
      format(x$b.RMA, digits = digits), "\t",
      format(x$upper, digits = digits), "\n")
  cat("\n")
  cat("h0\t df\t t\t P\n")
  cat(format(x$h0, digits = digits), "\t",
      format(x$df_phyl, digits = digits), "\t",
      format(x$t, digits = digits), "\t",
      format(x$P, digits = digits), "\n\n")
}
