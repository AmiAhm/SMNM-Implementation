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

create.plots <- function(is, n, mean.length, sd, method, name,minimization.method = "numeric") {
  cha <- "a"
  pdf(name,
    width = 12, height = 8)


par(mfrow = c(length(is), 4),
  mar=c(5, 2.8, 4, 0),
    mgp = c(1.8,1,0))

for(i in 1:length(is)) {
  data <- just.give.me.some.data(seed = is[i], n = n, mean.length = mean.length)
  y <- data$y
  theta <- data$theta

  iterative.res <- itses(y,
                         m = m,
                          minimization.method   = minimization.method,
                          sd = sd,
                          method = method,
                          debug = T)


  par(mar=c(0, 0, 0, 0))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

  if(i == 1) get.legend1(method, pos = "right")

  par(mar=c(5, 2.8, 4, 1),
    mgp = c(1.8,1,0))

  inc.legend <- F
  if(i == 1) inc.legend <- T

  cha <- plot.fun(iterative.res, theta, sd = 1,
           cha = cha, inc.legend = inc.legend, inc.legend1 = F,
            extend =  TRUE)$cha

}


dev.off()


}
is <- 1:3
create.plots(is, n, mean.length, sd , "ST",  'output/figures/st-main-sampling-steps.pdf',"sampling" )
create.plots(is, n, mean.length, sd , "HT", 'output/figures/ht-main-sampling-steps.pdf', "sampling")

create.plots(is, n, mean.length, sd , "ST",  'output/figures/st-main-numeric-steps.pdf',"numeric" )
create.plots(is, n, mean.length, sd , "HT", 'output/figures/ht-main-numeric-steps.pdf', "numeric")
