library(itses)

theta <- c(rnorm(10), 10:20)
sd = 0.5
y <- (1+rnorm(length(theta), sd = sd) )*theta

result <- itses(y, sd = sd, method = "ST", max.threshold = Inf,
                minimizationmethod = "sampling", noisetype = "speckle", m = 1)

result$lambda


b = 1e3
n = length(theta)
noise <- (1+rnorm(b*length(theta), sd = sd) )
y.star <- matrix(noise, ncol = b)*theta


lambdas <- seq(0, result$lambda*2, length.out = 100)

  # Calculate risk over all samples and add at different thresholds
risks <- sapply(lambdas, function(lambda) {
  mean(sapply(1:b, function(i) itses:::loss.w.st(theta, y.star[, i], lambda)))
})

plot(result$iteration_result[[length(result$iteration_result)]][,1], result$iteration_result[[length(result$iteration_result)]][,2],
     xlim = c(0, result$lambda*2), ylim = c(0, max(risks, result$iteration_result[[length(result$iteration_result)]][,2])))
abline(v = result$lambda)
abline(v = result$lambdas, lty = 2)


lines(lambdas, risks)