rev.st <- function(y, lambda) {
  over.th <- abs(y) > lambda
  y[over.th] <- sign(y[over.th])*lambda
  y
}

risk.rev.st <- function(theta, lambda) {
    risks <- (theta^2+2*theta*lambda + lambda^2 - 1)*pnorm(-lambda-theta) +
      (theta^2 -2*theta*lambda+lambda^2 -1)*pnorm(-lambda+theta)+1 -
      (lambda - theta)*dnorm(lambda-theta) - (lambda + theta)*dnorm(lambda+theta)

    sum(risks)

}

dlambda.risk.rev.st <- function(theta, lambda) {
 drisks <-2*(theta+lambda)*pnorm(-lambda-theta) + 2*(-theta + lambda)*pnorm(-lambda + theta)
  sum(drisks)
 }

d2lambda.risk.rev.st <- function(theta, lambda) {
 d2risks <- 2*(pnorm(-lambda-theta)+ pnorm(-lambda+theta))-
    2*(theta+lambda)*dnorm(-lambda-theta)-2*(-theta + lambda)*dnorm(-lambda+theta)

 sum(d2risks)
}

# plot.risk(risk.fun = risk.rev.st, name = "MOD-WINSC")
data <- just.give.me.some.data(n = 1e2, mean.length = 0.2)
y <- data$y
theta <- data$theta

result <- itses(y, method = "REVST", sd = 1)
last.sim <- result$iteration_result[[length(result$iteration_result)]]

lambdas <- seq(0, 13, length.out = 1e3)
true.risks <- sapply(lambdas, function(lambda) risk.rev.st(theta, lambda))
plot(lambdas, true.risks, type = "l")

j <- which.min(true.risks)
opt.lambda <- lambdas[j]

abline(v=opt.lambda)

mean(rev.st(y, opt.lambda))