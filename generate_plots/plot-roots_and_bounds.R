setwd("~/GitHub/SMNM-Implementation")
library(latex2exp)
# Return root rho function given n
get_p <- function(n) {
  function(lambda) {
    (1+lambda^2)*(2*(n+1)*pnorm(-lambda)-1)-
      2*lambda*dnorm(lambda)*(n+1)
  }
}

# Just checking
p <- get_p(3)
p(0) # Should be n
p(1e4) # She be towards -inf

# Calculate roots for different n-s
ns <- seq(from = 3, to = 1e9, by = 10000)
minimax.lambdas <- sapply(ns, function(n) {
  p <- get_p(n)
  uniroot(p, lower = 0, upper = sqrt(2*log(n))+1)$root
})


# What is the square log root boundary
upper.lambdas <- sapply(ns, function(n) sqrt(2*log(n)))

# Adjusted square log root with log log etc.
upper.adjusted.lambdas <- sapply(ns, function(n) sqrt(2*log(n+1)-4*log(log(n+1))-log(2*pi)))

loglogn <- sapply(ns, function(n) log(log(n)))

# Plot results
pdf(file="output/figures//minimaxth.pdf", width = 5, height = 3.5)
  layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(3,1.2)) # Widths of the two columns
par(mar=c(5, 3.4, 4, 0),
    mgp = c(2.3,1,0.5)
)
plot(ns,
     minimax.lambdas,
     type = "l",
     ylim = c(0,1*max(upper.lambdas)),
     xlab = "a",
     ylab = TeX(r"($\kappa^*)"),
     lty = 1)
lines(ns, upper.lambdas, col = "red", lty = 1)
lines(ns, upper.adjusted.lambdas, col = "green", lty = 1)
lines(ns, loglogn, col = "blue", lty = 1)

par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center",
       legend = c(TeX(r"($\sqrt{2\log(a)}$)"),
                  TeX(r"($\tilde{\kappa}^{**}_{a,0}$)"),
                  TeX(r"($\kappa^*_{a,0}$)"),
                  TeX(r"( $\log\log(a)$)")),
       col = c("red", "green", "black", "blue"), lty = rep(1,3))
dev.off()