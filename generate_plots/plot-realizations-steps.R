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

create.plots <- function(is, n, mean.length, sd, method, name,minimizationmethod = "numeric", show.loss.fun = T,
                         height = 8, size.multiplier = 1, legend.outside = T, include.risk.th  = T) {
  cha <- "a"

  width.multiplier <- 1
  if(!include.risk.th) width.multiplier  <- 2/3
  pdf(name,
    width = width.multiplier*size.multiplier*12, height = size.multiplier*height)

  cols <- 3
if(legend.outside) cols <- 4
if(!include.risk.th) cols <- cols - 1
  par(mfrow = c(length(is), cols),
  mar=c(5, 2.8, 4, 0),
    mgp = c(1.8,1,0))

for(i in 1:length(is)) {
  data <- just.give.me.some.data(seed = is[i], n = n, mean.length = mean.length)
  y <- data$y
  theta <- data$theta

  iterative.res <- itses(y,
                         m = m,
                          minimizationmethod   = minimizationmethod,
                          sd = sd,
                          method = method,
                          debug = T)


  if(legend.outside){
    par(mar=c(0, 0, 0, 0))
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    }
  if(i == 1){
   if(legend.outside){
     get.legend1(method, pos = "right", show.loss.fun = show.loss.fun)
   }
  }
  par(mar=c(5, 2.8, 4, 1),
    mgp = c(1.8,1,0))

  inc.legend <- F
  if(i == 1) inc.legend <- T

  cha <- plot.fun(iterative.res, theta, sd = 1,
           cha = cha, inc.legend = inc.legend, inc.legend1 = !legend.outside,
            extend =  TRUE, show.loss.fun = show.loss.fun, include.risk.th  = include.risk.th )$cha


}
dev.off()
}
is <- 1:3
create.plots(is, n, mean.length, sd , "ST",  'output/figures/st-main-sampling-steps.pdf',"sampling" )
create.plots(is, n, mean.length, sd , "HT", 'output/figures/ht-main-sampling-steps.pdf', "sampling")

create.plots(is, n, mean.length, sd , "ST",  'output/figures/st-main-numeric-steps.pdf',"numeric" )
create.plots(is, n, mean.length, sd , "HT", 'output/figures/ht-main-numeric-steps.pdf', "numeric")


create.plots(8, n, 2, sd ,
                    "ST",
             'output/figures/st-main-numeric-steps-one.pdf',
             "numeric",
             show.loss.fun= F, height = 4, legend.outside = F, size.multiplier = 2*3/4, include.risk.th = F)

