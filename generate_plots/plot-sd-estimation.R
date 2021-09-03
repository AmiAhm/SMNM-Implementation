rm(list = ls())
library(latex2exp)
library(itses)
setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
mean.lengths <- c(0.1, 0.5,1,2)
ns <- c(10, 50, 500, 5000)
h <- 0.4
m <- 1e4

sim.sd.est <- function(mean.length, m, n, h) {
  sapply(1:m, function(i) {
    data <- just.give.me.some.data(seed = i, mean.length = mean.length, n = n)
    y <- data$y
    theta <- data$theta
    t <- sum(theta == 0)

    y.thresholded.oracle <- y[order(abs(y))][1:t]
    sd.doh.t.oracle <- itses:::mad.estimator(y.thresholded.oracle)


    sd.doh <- itses:::mad.estimator(y)
    sd.doh.lopes <- sparse.mad.estimator(y, h = h)


    c(sd.doh.t.oracle, sd.doh, sd.doh.lopes)
  })
}
  pdf("output/figures/sd_ests.pdf", paper="a4", height = 13, width = 13)
par(mfrow=c(length(ns), length(mean.lengths)), mar = c(5,2,2,2))
for(n in ns) {
for(mean.length in mean.lengths) {
  sim.ests <- t(sim.sd.est(mean.length, m, n, h))
  sim.ests <- as.data.frame(sim.ests)
  names(sim.ests) <- c("Oracle\nSMAD", "MAD", "SMAD")
  boxplot(sim.ests,
           ylim = c(0, 12),
           col = c("grey", "indianred1", "skyblue"),
          las = 1.7,
  ylab = TeX("$\\sigma$"))
  abline(h = 1, col = "red")
  title(paste0("n=", n, ", l=", mean.length))
}
}
dev.off()

