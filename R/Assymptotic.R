library("phytools")
set.seed(10)

tr <- rtree(10)
tr <- compute.brlen(tr, power = 0.001)
plot(tr)

x <- rnorm(10)
y <- rnorm(10)

names(x) <- tr$tip.label
names(y) <- tr$tip.label
phyl.RMA(x, y, tr)$RMA.beta

M <- data.frame(x, y, species = tr$tip.label)
cd <- comparative.data(phy = tr, data = M, "species")
pgls(y ~ x, cd)

library("smatr")
sma(y ~ x)
