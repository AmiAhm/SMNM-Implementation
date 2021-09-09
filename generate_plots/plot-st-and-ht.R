# Plot Figure 1.1
setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
library(latex2exp)
library(itses)

lambda <- 1


pdf('output/figures/st-ht.pdf', height = 3.5, width = 6.6)
par(mfrow = c(1,2), mar = c(5,5,4,2))
x <- seq(from = -3, to = 3, length.out = 100)
y <- itses:::soft.threshold.estimator(x, lambda)

x1 <- seq(from = 1.001*lambda, to = 3, length.out = 100)
y1.ht <-  itses:::hard.threshold.estimator(x1, lambda)
plot(x, y,
     type = "l",
     col = "darkblue",
     xlab = "y",
     ylab = TeX("$\\hat{\\theta}^{ST}_{\\lambda}(y)$"),
     xlim = c(-2,2),
     ylim = c(-2,2),
)
title(main = "a) Soft-Threshold", cex.main = 1)


lines(x, x, lty = "dashed")
plot(-x1, -y1.ht,
     type = "l",
     col = "darkblue",
     xlab = "y",
     ylab = TeX("$\\hat{\\theta}^{HT}_{\\lambda}(y)$"),
     xlim = c(-2,2),
     ylim = c(-2,2)
)
title(main = "b) Hard-Threshold", cex.main = 1)

lines(c(-lambda, lambda), c(-lambda, lambda), lty = "dashed")
lines(c(-lambda, lambda), c(0, 0), col = "darkblue")
lines(x1, y1.ht, col = "darkblue")


dev.off()



pdf('output/figures/st.pdf', height = 4, width = 4)
par(mfrow = c(1,1), mar = c(5,5,4,2))
plot(x, y,
     type = "l",
     col = "darkblue",
     xlab = "y",
     ylab = TeX("$\\hat{\\theta}^{ST}_{\\lambda}(y)$"),
     xlim = c(-2,2),
     ylim = c(-2,2),
)
title(main = "", cex.main = 1)
lines(x, x, lty = "dashed")
dev.off()

pdf('output/figures/ht.pdf', height = 4, width = 4)
par(mfrow = c(1,1), mar = c(5,5,4,2))
plot(-x1, -y1.ht,
     type = "l",
     col = "darkblue",
     xlab = "y",
     ylab = TeX("$\\hat{\\theta}^{HT}_{\\lambda}(y)$"),
     xlim = c(-2,2),
     ylim = c(-2,2)
)
title(main = "", cex.main = 1)
lines(c(-lambda, lambda), c(-lambda, lambda), lty = "dashed")
lines(c(-lambda, lambda), c(0, 0), col = "darkblue")
lines(x1, y1.ht, col = "darkblue")
dev.off()