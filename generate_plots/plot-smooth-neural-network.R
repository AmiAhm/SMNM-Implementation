setwd("~/GitHub/SMNM-Implementation")
df.magnitude <- read.csv("output/neural-network-pruning/magnitude_data.csv")
df.adapt <- read.csv("output/neural-network-pruning/adapt_data.csv")

is <- unique(df.magnitude$i)
js <- unique(df.magnitude$j)

titles <- c("a) Batchnorm. and L2", "b) Batchnorm. and no L2", "c) Batchnorm. and L2", "d) No Batchnorm. and no L2")

point.between.line <- function(x, xs, ys){
  i <- which(xs > x)[1]
  x1 <- xs[i-1]
  x2 <- xs[i]
  y1 <- ys[i-1]
  y2 <- ys[i]
  (y2-y1)/(x2-x1)*(x-x1)+y1
}

xx <- seq(0.51, 0.99, length.out = 1e3)
pdf("output/figures/nn-mean-rels.pdf", width = 9, height = 3)
par(mfrow = c(1,4))
for(i in is){
  yy <- NULL
  yy2 <- NULL
  for(j in js){
  df.m.temp <- df.magnitude[df.magnitude$i == i,]
    df.m.temp <- df.m.temp[df.m.temp$j == j,]
  df.m.temp <- as.data.frame(df.m.temp)
  yy <- rbind(sapply(xx, function(x) point.between.line(x, df.m.temp$sparsity, df.m.temp$accuracy)),yy)

    df.adapt.temp <- df.adapt[df.adapt$i == i,]
    df.adapt.temp <- df.adapt.temp[df.adapt.temp$j == j,]
  df.adapt.temp <- as.data.frame(df.adapt.temp)
  yy2 <- rbind(sapply(xx, function(x) point.between.line(x, df.adapt.temp$sparsity, df.adapt.temp$accuracy)),yy)

}
  yyy <- apply(yy, 2, mean)
  yyy2 <- apply(yy2, 2, mean)

  xlim <- if(i == 1 | i == 3) c(0.92,0.98) else NULL

  plot(xx, yyy, type = "l", lty = 1,
       ylim = c(min(yyy, yyy2, na.rm = T), max(yyy, yyy2, na.rm = T)),
       xlim = xlim,
  xlab = "Sparsity",
       ylab = "Test accuracy"
  )
  print(i)
  title(main = titles[i+1], adj = 0)
  lines(xx, yyy2, type = "l", lty = 1, col = "red")


  # i <- seq(1, length(xx), by = 100)
  # yyy.max <- apply(yy[,i], 2, max)
  # yyy.min <- apply(yy[,i], 2, min)
  # arrows(xx[i], yyy.min, xx[i],yyy.max, length=0.05, angle=90, code=3)
  #
  # i <- seq(25, length(xx), by = 100)
  # yyy.max <- apply(yy2[,i], 2, max)
  # yyy.min <- apply(yy2[,i], 2, min)
  # arrows(xx[i], yyy.min, xx[i],yyy.max, length=0.05, angle=90, code=3, col = "red")




}
dev.off()
