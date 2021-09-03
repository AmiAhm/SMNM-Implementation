setwd("~/GitHub/SMNM-Implementation")
library(latex2exp)


d2.d2theta.risk <- function(theta, lambda) {
  2*(pnorm(lambda-theta)-pnorm(-lambda-theta)) +
    2*theta*(-dnorm(lambda-theta)+dnorm(-lambda-theta))
}


thetas <- seq(-10, 10, length.out = 1000)
lambdas <- seq(from = 0, to = 20, by = 5)

risks <- sapply(lambdas, function(lambda) sapply(thetas,
                                               function(theta) d2.d2theta.risk(theta, lambda)))
pdf(file= "output/figures/d2Riskd2theta_theta.pdf", height = 4, width = 4)

    # Change layout to allow for legend outside to the left.
  layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(3,1)) # Widths of the two columns
par(mar=c(5,4.5,4,0.5))


plot(thetas, risks[,1], type = "l", col = 1,
     xlab = TeX(r"($\mu$)"),
     ylab = TeX(r"($d^2R/d\mu^2$)"),
     ylim = c(-8, 2.5),
     main = "")
for(i in 2:length(lambdas)) {
  lines(thetas, risks[,i], type = "l", col = i)
}

  par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("left",
       legend = lambdas, 
       col = 1:length(lambdas), 
       lty = 1, 
       title = TeX(r"($\lambda$)")
)
dev.off()
