library(itses)
theta <- c(rep(0, 1000), rnorm(10, mean = -2, sd = 1), rnorm(10, mean = 7, sd = 1), 1:5)
sd <- 0.5
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
lambdas <- seq(0, 10, length.out = 100)
  # Calculate risk over all samples and add at different thresholds
risks <- sapply(lambdas, function(lambda) {
  mean(sapply(1:b, function(i) itses:::loss.w.st(theta, y.star[, i], lambda)))
})
plot(result$iteration_result[[length(result$iteration_result)]][,1], result$iteration_result[[length(result$iteration_result)]][,2],
     xlim = c(0, 10), ylim = c(0, max(risks, result$iteration_result[[length(result$iteration_result)]][,2])))
abline(v = result$lambda)
abline(v = result$lambdas, lty = 2)
lines(lambdas, risks)

## Custom estimator
library(itses)

theta <- c(rep(0, 10), rnorm(10, mean = -2, sd = 1), rnorm(10, mean = 7, sd = 1), 1:5)
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
                method = method,
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
     xlim = c(1, max.threshold), ylim = c(0, max(risks, result$iteration_result[[length(result$iteration_result)]][,2], na.rm = T)))
abline(v = result$lambda)
abline(v = result$lambdas, lty = 2)
lines(lambdas, risks)

j <-which.min(risks)
lambdas[j]