setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")

library(latex2exp)


thetas <- c(0, Inf)
lambdas <- seq(0, 10, length.out = 1000)

risks <- sapply(thetas, function(theta) sapply(lambdas,
                                               function(lambda) itses:::dlambda.risk.st(theta, lambda)))
pdf(file= "output/figures/dlambda_drisk_lambda.pdf", height = 4, width = 4)

    # Change layout to allow for legend outside to the left.
  layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(3,1)) # Widths of the two columns
par(mar=c(5,4,4,0.5))


plot(lambdas, risks[,1], type = "l", col = 1,
     xlab = TeX(r"($\lambda$)"),
     ylab = TeX(r"($dR/d\kappa$)"),
     ylim = c(-3, max(risks)),
     main = "")
for(i in 2:length(thetas)) {
  lines(lambdas, risks[,i], type = "l", col = i)
}
abline(h = 0, lty = 2)

  par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left",
       legend = c(TeX(r"($0$)"), TeX(r"($\infty$)")),
       col = 1:length(thetas), 
       lty = 1, 
       title = TeX(r"($\theta$)")
)
dev.off()
