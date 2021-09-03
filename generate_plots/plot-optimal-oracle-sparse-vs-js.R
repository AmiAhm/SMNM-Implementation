# Plot Figure 2.1
setwd("~/GitHub/SMNM-Implementation/")
library(latex2exp)
library(lattice)

# Uncomment if you want to run
# sd <- 1
# m <- 1e3
# grid <- expand.grid(x1 = seq(0,5, length.out = m),
#             x2 = seq(0,5, length.out = m))
# res <- sapply(1:nrow(grid), function(i) {
#   print(i)
#   row <- grid[i,]
#   l <- sum(row^2)
#   n <- length(row)
#   r.js <- n*sd^2*l/(n*sd^2+l)
#   r <- sum((row^2)[row^2< sd^2])+ sum(row^2 > sd^2)*sd^2
#   r < r.js
# })
#
#
# data <- cbind(grid, res)
# data <- as.data.frame(data)
# colnames(data) <- c("x", "y", "z")
#save(data, file = "output/optimal_oracle_sparse_vs_js.RData")
load("output/optimal_oracle_sparse_vs_js.RData")

#pdf("output/figures/optimal_oracle.pdf", height = 4, width = 4)
png("output/figures/optimal_oracle.png", height = 3200, width =3200, res = 800)
levelplot(z  ~ x*y,
          data,
          xlab = TeX("$\\theta_1$"),
          ylab = TeX("$\\theta_2$"),
          col.regions = gray(0:100/100), 
          colorkey = F)
dev.off()