rm(list = ls())
setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
library(itses)
library(latex2exp)
theta <-  c(0, 0.1, 0.5, 2, 3)

sd <- 1

set.seed(3)
y <- rnorm(length(theta), mean = theta, sd = 1)

print(round(y, digits = 3))
lambdas <- seq(0, to = 5, length.out = 10000)

true.risk.st <- sapply(lambdas, function(lambda) itses:::risk.st(theta = theta, lambda = lambda))
true.risk.ht <- sapply(lambdas, function(lambda) itses:::risk.ht(theta = theta, lambda = lambda))


sure.risk <- sapply(lambdas, function(lambda) risk.hat(y = y, sd = 1, lambda = lambda))

sure.threhsold.i <- which.min(sure.risk)
sure.threshold <- lambdas[sure.threhsold.i]
opt.threshold.st <- itses:::get.risk.oracle.threshold(theta = theta)$lambda
opt.threshold.ht <- itses:::get.risk.oracle.threshold(theta = theta, method = "HT")$lambda

sparse.oracle.risk.ratio <- function(thetas, lambda, method = "ST"){
  if(method == "ST"){
    risk.fun <- itses:::risk.st
  }else if(method == "HT"){
    risk.fun <- itses:::risk.ht
  }
  n <- length(thetas)
  risks <- sapply(thetas, function(theta) risk.fun(theta, lambda)/(1/n+min(theta^2, 1)))

  sum(risks)
}

sparse.orcs.risk.ratios.st <- sapply(lambdas, function(lambda) sparse.oracle.risk.ratio(theta, lambda))
sparse.orcs.risk.ratios.ht <- sapply(lambdas, function(lambda) sparse.oracle.risk.ratio(theta, lambda, method = "HT"))

pdf("output/figures/sure_risk.pdf", height = 3.5, width = 7)
  layout.matrix <- matrix(c(1, 2,3), nrow = 1, ncol = 3)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(2.9,2.9,1.3)) # Widths of the two columns
par(mar=c(5,4,4,0.5))
plot(lambdas, sure.risk, type = "l", ylim = c(0, 17),
     ylab = "Risk",
     main = "a) Soft-Thresholding",
     xlab = TeX(r"($\lambda$)"))
lines(lambdas, true.risk.st, col = "red")

lines(lambdas, sparse.orcs.risk.ratios.st, col = "purple")

plot(lambdas, true.risk.ht, type = "l", ylim = c(0, 17), col = "red",
     main = "b) Hard-Thresholding",
     ylab = "Risk",
     xlab = TeX(r"($\lambda$)"))
lines(lambdas, sparse.orcs.risk.ratios.ht, col = "purple")

par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", col = c("black", "red", "purple"), lty = 1, legend = c("SURE", "Oracle risk", "Sparse-oracle\nrisk-ratio"
))

dev.off()