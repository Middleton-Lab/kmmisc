#' This function is intended to be used with scale_continuous from
#' the ggplot2 package to reformat axis labels as base-10 exponents.
#' See \url{http://goo.gl/fNMKjB} for more information.
#'
#' @title Format as base 10 exponent
#' @param x numeric to be reformatted.
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#' @export
scientific_10 <- function(x) {
  parse(text = gsub("e", " %*% 10^", scientific_format()(x)))
}

#' This function plots a MrBayes log file. Either a .mcmc file or a
#' .p file is acceptable, and the file type will be determined
#' automatically. If an mcmc file is chosen, the average standard
#' deviation of the split frequencies is plotted. If a p file is
#' chosen, the log-likelihood is plotted.
#'
#' @title Plot MrBayes log file
#'
#' @param logfile The MrBayes log file to be loaded. If \code{logfile
#'   = NULL}, then the user is prompted to choose a file.
#' @param burnin The burnin period, either specified in absolute
#'   number of generations or as a fraction of the total number of
#'   generations (\code{burnin} < 1). The default is 0.25.
#' @param lwd Line width for the plot. Defaults to 1.5.
#' @param cutoff The minimum value for acceptable convergence. Only
#'   applicable for mcmc files. Defaults to 0.05.
#' @param ideal The ideal value for acceptable convergence. Only
#'   applicable for mcmc files. Defaults to 0.01.
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#' @export
plot_mrb <- function(logfile = NULL,
                     burnin = 0.25,
                     lwd = 1.5,
                     cutoff = 0.05,
                     ideal = 0.01){

  if (is.null(logfile)){
    cat("\n\n\tChoose the .p or .mcmc file\n\n")
    flush.console()
    logfile <- file.choose(new = FALSE)
  }

  ext <- file_ext(logfile)

  if (!(ext %in% c("mcmc", "p"))) {
    stop("File should be either a .mcmc or .p file.")
  }

  # .mcmc has 6 header rows. .p has 1.
  nskip <- ifelse(ext == "mcmc", 6, 1)
  mb_log <- read.table(logfile,
                       header = TRUE,
                       skip = nskip)

  # For .mcmc, choose standard deviation of split frequency. For .p
  # choose LnL. Then subset mb_log.
  col_to_plot <- ifelse(ext == "mcmc",
                        "AvgStdDev.s.", "LnL")
  mb_log <- mb_log[, c("Gen", col_to_plot)]

  # Rename column 2
  names(mb_log)[2] <- "parameter"

  # Burnin
  if (burnin < 1){
    burnin_gen <- trunc(burnin * max(mb_log$Gen))
  } else {
    burnin_gen <- burnin
  }
  label_txt <- paste("burnin ", burnin_gen, sep = "")
  ylabel <- ifelse(ext == "mcmc",
                   "Average SD of Split Frequencies",
                   "Log-likelihood")

  p <- ggplot(mb_log, aes_string(x = "Gen", y = "parameter")) +
    geom_vline(xintercept = burnin_gen,
               color = I("blue"), lwd = lwd) +
    geom_line(lwd = lwd) +
    ylab(ylabel) + xlab("Generation") +
    scale_x_continuous(label = scientific_10) +
    annotate("text", label = label_txt,
             x = (burnin_gen - 0.08 * burnin_gen),
             y = max(mb_log$parameter),
             size = 8, colour = "blue", angle = 90, hjust = 1)
  if (ext == "mcmc") {
    p <- p +
      geom_hline(yintercept = cutoff,
                 color = I("yellow"), lwd = lwd) +
      geom_hline(yintercept = ideal,
                 color = I("red"), lwd = lwd)
  }
  print(p)
}
