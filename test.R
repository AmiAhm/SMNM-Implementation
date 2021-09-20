rm(list = ls())
library(latex2exp)
library(itses)
setwd("~/GitHub/SMNM-Implementation")

set.seed(1)
theta <- c(rnorm(4000, mean = 0, sd = 0.5), rnorm(1000, mean = 1, sd = 0.5))
sd <- 0.7
y <- (1+rnorm(length(theta), sd = sd) )*theta

result <- itses(y,
                sd = sd,
                method = "ST",
                max.threshold = Inf,
                minimizationmethod = "sampling",
                noisetype = list(sample = function(b, theta){
                  noise <- (1+rnorm(b*length(theta), sd = sd))
                  y.star <- matrix(noise, ncol = b)*theta
                  y.star
                }),
                m = 5)

result$lambda
b <- 1e3
n <- length(theta)
noise <- (1+rnorm(b*length(theta), sd = sd) )
y.star <- matrix(noise, ncol = b)*theta
lambdas <- seq(0, 1.3, length.out = 100)
  # Calculate risk over all samples and add at different thresholds
risks <- sapply(lambdas, function(lambda) {
  mean(sapply(1:b, function(i) itses:::loss.w.st(theta, y.star[, i], lambda)))
})
pdf("output/figures/speckle-example.pdf", height = 6, width = 6)
par(mfrow = c(1,1))
plot(result$iteration_result[[length(result$iteration_result)]][,1], result$iteration_result[[length(result$iteration_result)]][,2],
     xlim = c(0, .65),
     ylim = c(0, 1300),
     xlab = TeX(r"($\lambda$)"),
     ylab = "Risk",
     type = "l")
title(main = "Speckle noise \n n=5000, b = 1000, m = 5", adj = 0)

abline(v = result$lambda)
lines(lambdas, risks, col = "purple")

j <-which.min(risks)
abline(v=lambdas[j], col = "purple", lty = 2)
#abline(v = itses(y, sparse.mad = F)$lambda, lty = 2)

#par(mar=c(0, 0, 0, 0))
#plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("bottomright", legend = c("Oracle risk",
                               "Estimated risk",
                               "Oracle threshold",
                               "Speckle ITSES-ST"
                               #"Regular ITSES threshold"
),
       lty = c(1,1,2,1,2),
       col=c("purple", "black", "purple", "black", "black"))

dev.off()


## Custom estimator
library(itses)

theta <- c(rnorm(10, mean = 0, sd = 0.05), rnorm(5, mean = 1, sd = 0.5))
sd <- 0.03
y <- (1+rnorm(length(theta), sd = sd) )*theta

method <- function(y, lambda){
                  l2 <- mean(abs(y))
                  is.shrunk <- y/l2 < 1
                  y[is.shrunk] <- y[is.shrunk]*lambda
                  y[!is.shrunk] <- y[!is.shrunk]/lambda
                  y
                }
max.threshold <- 1.5*sd+1
b <- 50
result <- itses(y,
                sd = sd,
                method = "ST",
                min.threshold = 1,
                max.threshold = max.threshold,
                minimizationmethod = "sampling",
                noisetype = list(sample = function(b, theta){
                  noise <- (1+rnorm(b*length(theta), sd = sd))
                  y.star <- matrix(noise, ncol = b)*theta
                  y.star
                }),
                m = 10, k = 200, b = b)

result$lambda
n = length(theta)
noise <- (1+rnorm(b*length(theta), sd = sd) )
y.star <- matrix(noise, ncol = b)*theta
lambdas <- seq(1, max.threshold, length.out = 100)
  # Calculate risk over all samples and add at different thresholds
risks <- sapply(lambdas, function(lambda) {
  mean(sapply(1:b, function(i) itses:::get.l2.loss(theta, method(y.star[, i], lambda))))
})
plot(result$iteration_result[[length(result$iteration_result)]][,1], result$iteration_result[[length(result$iteration_result)]][,2],
     xlim = c(1, max.threshold),
     ylim = c(0, max(risks, result$iteration_result[[length(result$iteration_result)]][,2], na.rm = T)),
type = "l")
abline(v = result$lambda)
lines(lambdas, risks, col = "purple")

j <-which.min(risks)
abline(v=lambdas[j], col = "purple", lty = 2)
points(y, rep(0, length(y)), pty = 3)
itses(y, sparse.mad = F)$lambda