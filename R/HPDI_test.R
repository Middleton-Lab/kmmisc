#' HPDI Interval Test
#'
#' @param samples 1d vector of posterior samples
#' @param test_value Numeric value to test against
#' @param upper Upper bound of intervals to test. Defaults to 0.99.
#' @param lower Upper bound of intervals to test. Defaults to 0.99.
#'
#' @return Widest HPDI not including the test value.
#' @export
#'
HPDI_test <- function(samples, test_value = 0, upper = 0.99, lower = 0.01) {
  intervals <- seq(upper, lower, by = -0.01)
  in_interval <- logical(length = length(intervals))
  for (ii in 1:length(intervals)) {
    hdi <- as.numeric(HPDI(samples, prob = intervals[ii]))
    in_interval[ii] <- ifelse(test_value > hdi[1] & test_value < hdi[2], FALSE, TRUE)
  }
  narrowest_interval <- ifelse(sum(in_interval) == 0, 0, max(intervals[in_interval]))
  return(narrowest_interval)
}

