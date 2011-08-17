library(rbenchmark)
library(snow)

cl <- makeCluster(c("localhost","localhost"), type = "SOCK")

tm <- Sys.time()

myfun <- function() {
  x <- runif(500000);
  library(rbenchmark)
  Fun1 <- function() all( abs(x - mean(x)) < .Machine$double.eps ^ 0.5 )
  Fun2 <- function() {diff(range(x)) < .Machine$double.eps ^ 0.5}
  Fun3 <- function() {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = .Machine$double.eps ^ 0.5))
  }
  benchmark(Fun1(), Fun2(), Fun3(),
            columns=c("test", "replications", "elapsed", "relative"),
            order="relative", replications = 500)
}

xx <- clusterCall(cl, myfun)
Sys.time() - tm
stopCluster(cl)
