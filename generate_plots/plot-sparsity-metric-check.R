rm(list = ls())
setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
mean.lengths <- c(0.1, 0.5,1,2)
ns <- c(50, 500)
m <- 1e4


get.sim <- function(m, mean.length, n, ps) {
  sapply(ps, function(p) {
    sim.res <- sapply(1:m, function(i) {
      set.seed(i)
      data <- just.give.me.some.data(n = n, mean.length = mean.length, p = p)
      y <- data$y
      itses:::sparsity.estimator(y)
    })
    quantile(sim.res, probs = c(0.025, 0.5, 0.975))
  })
}

k <- 6/8
ce <- 1.5
pdf("output/figures/sparse_est.pdf",  height = length(ns)*k*4, width = k*12)
par(mfrow = c(length(ns), length(mean.lengths)))

for(n in ns) {
    n.ps <- n
    ps <- seq(from = 0, to = 1, length.out = n.ps )[-1][-(n.ps-1)]
    for(mean.length in mean.lengths) {
  sim.data <- get.sim(m, mean.length, n, ps)
  plot(ps, sim.data[2,],
       cex = ce, cex.lab=ce, cex.axis=ce, cex.main=ce, cex.sub=ce,
       type = "l", xlim = c(0,1), ylim = c(0,1), xlab = "True sparsity", ylab = "Estimated sparsity")
  lines(ps, sim.data[1,], lty = 2 )
  lines(ps, sim.data[3,], lty = 2 )
  lines(ps, ps, lty = 1, col = "indianred1")
  title(paste0("n=", n, ", l=", mean.length))
}
}
dev.off()