rm(list = ls())
library(itses)
library(latex2exp)
setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
source("R-utility/itses-result-plotter.R")
legend.inset <- c(-0.43,-0.48)
chars <- c("a", "b", "c", "d")
n <- 25
mean.length <- 0.5
sd <- 1
m <- 50
create.plots <- function(chars, is, n, mean.length, sd, method, name) {
  pdf(name,
    height = 2.7, width = 9)

  layout.matrix <- matrix(c(1, 2, 3, 4,5), nrow = 1, ncol = 4)

  layout(mat = layout.matrix,
         heights = 2, # Heights of the two rows
         widths = c(2, 2, 2, 0.9)) # Widths of the two columns

par(mar=c(5, 2.8, 4, 0),
    mgp = c(1.8,1,0))

for(i in 1:length(is)) {
  data <- just.give.me.some.data(seed = is[i], n = n, mean.length = mean.length)
  y <- data$y
  theta <- data$theta

  iterative.res <- itses(y,
                          minimizationmethod   = "numeric",
                          sd = sd,
                         m = m,
                          method = method,
                          debug = T)



  plot.fun(iterative.res, theta, sd = 1,
           cha = chars[i], inc.legend = F)

}

  par(mar=c(0, 0, 0, 0))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  get.legend1(method, "center")

dev.off()


}
is <- 1:3
create.plots(chars, is, n, mean.length, sd , "ST",'output/figures/st-main.pdf' )
create.plots(chars, is, n, mean.length, sd , "HT",'output/figures/ht-main.pdf' )


