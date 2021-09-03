setwd("~/GitHub/SMNM-Implementation/")
source("R-utility/extra-utility.R")
library(latex2exp)
library(lattice)
mu  <- seq(0, 10, length.out = 1000)
kappa <- seq(0, 100, length.out = 1000)
data <- expand.grid(mu, kappa)
colnames(data) <- c("mu", "kappa")

# results <- sapply(1:nrow(data), function(i) {
#   st <- itses:::d2lambda.risk.st(theta = data[i,]$mu, lambda = data[i,]$kappa) > 0
#   ht <- itses:::d2lambda.risk.ht(theta = data[i,]$mu, lambda = data[i,]$kappa) > 0
#   c(st, ht)
# })
# results <- t(results)
# colnames(results) <- c("st", "ht")
#
# data <- cbind(data, results)
# save(data, file = "output/d2sign.RData")
load( file = "output/d2sign.RData")

#pdf("output/figures/sign_d2_st.pdf", width = 4, height = 4)
png("output/figures/sign_d2_st.png", height = 3200, width =3200, res = 800)
levelplot(st  ~ mu*kappa,
          data,
          xlab = TeX("$\\mu$"),
          ylab = TeX("$\\kappa$"),
          col.regions = gray(0:100/100),
          colorkey = F
)
dev.off()

#pdf("output/figures/sign_d2_ht.pdf", width = 4, height = 4)
png("output/figures/sign_d2_ht.png", height = 3200, width =3200, res = 800)
levelplot(ht  ~ mu*kappa,
          data,
          xlab = TeX("$\\mu$"),
          ylab = TeX("$\\kappa$"),
          col.regions = gray(0:100/100),
          adj = 0,
          colorkey = F)
dev.off()