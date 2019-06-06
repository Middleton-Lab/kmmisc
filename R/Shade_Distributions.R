shade_normal <- function(q, tail = "both", mean = 0, sd = 1) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  crit <- qnorm(q, mean, sd)
  M <- tibble(x = seq(-4, 4, length = 200),
                  y = dnorm(x))
  p <- ggplot(M, aes(x, y)) +
    geom_line() +
    labs(x = "Value", y = "Relative Likelihood")
  lower <- geom_ribbon(data = subset(M, x < crit),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  upper <- geom_ribbon(data = subset(M, x > abs(crit)),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  if (tail == "both") p <- p + lower + upper
  if (tail == "lower") p <- p + lower
  if (tail == "upper") p <- p + upper
  return(p)
}

shade_t <- function(q, df, tail = "both") {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  require(latex2exp, quietly = TRUE)
  crit <- qt(q, df)
  M <- tibble(x = seq(-4, 4, length = 200),
                  y = dt(x, df))
  p <- ggplot(M, aes(x, y)) +
    geom_line() +
    labs(x = TeX("$t$"), y = "Relative Likelihood")
  lower <- geom_ribbon(data = subset(M, x < crit),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  upper <- geom_ribbon(data = subset(M, x > abs(crit)),
                       aes(ymax = y), ymin = 0,
                       fill = "red", alpha = 0.5)
  if (tail == "both") p <- p + lower + upper
  if (tail == "lower") p <- p + lower
  if (tail == "upper") p <- p + upper
  return(p)
}

shade_F <- function(q, df1, df2, vline = NULL) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  require(latex2exp, quietly = TRUE)
  crit <- qf(q, df1, df2, lower.tail = FALSE)
  if (!is.null(vline)) {
    x_max <- max(vline * 1.05, crit * 1.5)
  } else {
    x_max <- crit * 1.5
  }
  M <- tibble(x = seq(0.001, x_max, length = 200),
                  y = df(x, df1, df2))
  p <- ggplot(M, aes(x, y)) +
    geom_line() +
    geom_ribbon(data = subset(M, x > crit),
                aes(ymax = y), ymin = 0,
                fill = "red", alpha = 0.5) +
    ylim(c(0, 1.1 * max(M$y))) +
    labs(x = TeX("$F$"), y = "Relative Likelihood")
  return(p)
}

shade_chisq <- function(q, df) {
  require(tidyverse, quietly = TRUE)
  require(cowplot)
  require(latex2exp, quietly = TRUE)
  crit <- qchisq(q, df, lower.tail = FALSE)
  M <- tibble(x = seq(0.1, crit * 1.5, length = 200),
                  y = dchisq(x, df))
  p <- ggplot(M, aes(x, y)) +
    geom_line() +
    geom_ribbon(data = subset(M, x > crit),
                aes(ymax = y), ymin = 0,
                fill = "red", alpha = 0.5) +
    ylim(c(0, 1.1 * max(M$y))) +
    labs(x = TeX("$\\chi^2"), y = "Relative Likelihood")
  return(p)
}

approxdens <- function(x) {
  dens <- density(x)
  f <- with(dens, approxfun(x, y))
  f(x)
}

shade_quantiles <- function(y, lower, upper) {
  require(rethinking, quietly = TRUE)
  y_med <- median(y)
  y_mode <- chainmode(y)
  
  dt <- tibble(y) %>%
    mutate(dy = approxdens(y),
           p = percent_rank(y), 
           pcat = as.factor(cut(p, breaks = c(lower, upper),
                                include.lowest = TRUE)))
  
  ggplot(dt, aes(y, dy)) +
    geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcat)) +
    geom_line() +
    scale_fill_brewer(guide = "none") +
    geom_vline(xintercept = y_med, color = "red") +
    annotate("text", x = y_med, y = 0,
             label = "Median", color = "red",
             hjust = 0) +
    geom_vline(xintercept = y_mode, color = "blue") +
    annotate("text", x = y_mode, y = 0.05,
             label = "Mode", color = "blue",
             hjust = 0) +
    labs(y = "Density", title = "Quantile")
}

shade_HPDI <- function(y, prob = 0.89) {
  require(rethinking, quietly = TRUE)
  y_med <- median(y)
  y_mode <- chainmode(y)
  HPD <- HPDI(y, prob = prob) %>% as.numeric()
  
  dt <- tibble(y) %>%
    mutate(dy = approxdens(y),
           p = percent_rank(y), 
           pcat = as.factor(cut(y, breaks = c(HPD[1], HPD[2]),
                                include.lowest = TRUE)))
  
  ggplot(dt, aes(y, dy)) +
    geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcat)) +
    geom_line() +
    scale_fill_brewer(guide = "none") +
    geom_vline(xintercept = y_med, color = "red") +
    annotate("text", x = y_med, y = 0,
             label = "Median", color = "red",
             hjust = 0) +
    geom_vline(xintercept = y_mode, color = "blue") +
    annotate("text", x = y_mode, y = 0.05,
             label = "Mode", color = "blue",
             hjust = 0) +
    labs(y = "Density", title = "HPDI")
}

compare_intervals <- function(y, lower, upper, prob) {
  require(cowplot)
  p1 <- shade_quantiles(y, lower, upper)
  p2 <- shade_HPDI(y, prob)
  plot_grid(p1, p2, nrow = 2)
}
