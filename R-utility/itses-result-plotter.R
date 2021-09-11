source("R-utility/extra-utility.R")
library(latex2exp)
library(ggplot2)
get.legend1 <- function(method, pos = "topright", loss.threshold = F, show.loss.fun = T, universal = T) {
  legend(pos,
       legend = c("Final risk est.",
                  if(show.loss.fun) "True loss" else NULL,
                  "True risk",
                  TeX("$\\lambda_{ITSES}$"),
                  TeX("Risk oracle $\\lambda^*$"),
                  if(loss.threshold) TeX("$\\lambda_{LossOracle}$") else NULL,
                  if(method == "ST") TeX("$\\lambda_{SureShrink}$") else NULL,
                  if(universal) TeX("$\\tilde{\\lambda}_{Universal}$") else NULL,
                  TeX("$|Y_i|$")),
       col = c(alpha("black", 1),
               if(show.loss.fun) "red" else NULL,
               "purple",
               "black",
               "purple",
               if(loss.threshold) "red" else NULL,
               if(method == "ST") "black" else NULL,
               if(universal) "darkblue" else NULL,
               "black"),
       lty = c(1,
               if(show.loss.fun) 1 else NULL,
               1,
               1,
               2,
               if(loss.threshold) 2 else NULL,
               if(method == "ST") 2 else NULL,
               if(universal) 3 else NULL,
               NA),
       pch = c(NA,
               if(show.loss.fun) NA else NULL,
               NA,
               NA,
               NA,
               if(loss.threshold) NA else NULL,
               if(method == "ST") NA else NULL,
               if(universal) NA else NULL,
               4),
       xpd = T,
       bg = "white"
)
  }


plot.fun <- function(iterative.res,
                theta,
                     name = NULL, theta2 = F,
                     show.method = T, show.loss.fun = T,
                 sd.true = 1, mean.length = NULL, cha = "a", loss.threshold = F, y.min = -2, show.n.its = T,
                 inc.legend = F, inc.legend1 = F,  method = "ST", risk.max = 40, y.max = 5, extend = F,
universal = T) {
  y <- iterative.res$y
  n <- length(y)
  method <- iterative.res$method
  sd <- iterative.res$sd
  m <- iterative.res$m

  print(mean(theta == 0))


  visu <- sd*itses:::get.visu.threshold(y/sd)


  if(method == "ST") {
    ss <- sd*get.sureshrink.threshold(y/sd)
  }else{
    ss <- NULL
  }
  loss.oracle <- sd.true*get.loss.oracle.threshold(theta/sd.true, y = y/sd.true, method = method)

  lambda <- iterative.res$lambda

  thresholds <- c(visu, ss, loss.oracle, ss, lambda)
  risk.oracle <- sd.true*itses:::get.risk.oracle.threshold(theta/sd.true,
                                                   method = method, also.check = thresholds/sd.true, max.threshold = Inf)$lambda

  if(!is.finite(lambda)) lambda <- max(abs(y))
  if(!is.finite(risk.oracle)) risk.oracle <- max(abs(y))
  if(!is.finite(loss.oracle)) loss.oracle <- max(abs(y))


  xx <- seq(from = 0, to = max(abs(y), visu, risk.oracle, loss.oracle, lambda), length.out = 100)



  if(method == "ST") {
    loss <- itses:::loss.w.st
    risk <- itses:::risk.st
  }else if(method == "HT") {
    loss <- itses:::loss.w.ht
    risk <- itses:::risk.ht
  }


  est.risks.lambdas <- iterative.res$iteration_result[[length(iterative.res$iteration_result)]][,1]
  est.risks <- iterative.res$iteration_result[[length(iterative.res$iteration_result)]][,2]
  true.loss <- sapply(xx, function(lambda) sd.true^2*loss(theta/sd.true, y/sd.true, lambda/sd.true))
  true.risks <- sapply(xx, function(lambda) sd.true^2*risk(theta/sd.true, lambda/sd.true))

plot(est.risks.lambdas, est.risks,
     type = "l",
     col = "black",
     xlim = c(0, y.max),
     ylim = c(y.min, risk.max),
     xlab = TeX("$|Y|$ and $\\lambda$"),
     ylab = "Squared error")
points(abs(y), rep(y.min,n), pch = 4)
if(universal) abline(v=visu, col = "darkblue", lty = 3)
if(method =="ST") abline(v = ss, col = "black", lty = 2)
if(loss.threshold) abline(v= loss.oracle, col = "red",  lty = "dashed")
abline(v=risk.oracle, col = "purple", lty ="dashed")
if(show.loss.fun) lines(xx, true.loss, col = "red")
lines(xx, true.risks, col = "purple")
abline(v=lambda, col = "black")

if (inc.legend & inc.legend1) {
  get.legend1(method, loss.threshold = loss.threshold, show.loss.fun = show.loss.fun, universal = universal)
}


  true.risk.oracle <- sd.true^2*risk(theta/sd.true, risk.oracle/sd.true)

  if(method == "ST") {
    true.risk.ss <- risk(theta/sd.true, ss/sd.true)*sd.true^2
    ss.rr <- true.risk.ss/true.risk.oracle
    ss.rr <- round(ss.rr, digits = 3)
  }

  true.risk.visu <- sd.true^2*risk(theta/sd.true, visu/sd.true)
  visu.rr <- true.risk.visu/true.risk.oracle
  visu.rr <- round(visu.rr, digits = 3)


  itses.rr <- sd.true^2*risk(theta/sd.true, lambda/sd.true)/true.risk.oracle
  itses.rr <- round(itses.rr, digits = 3)

  met <- "smp."
  if(iterative.res$minimizationmethod == "numeric") {
    met <- "numr."
  }
title(paste0(cha, ") ",
             if(!is.null(name)) paste0(name,", ") else NULL,
             "n = ", n,
             if(!is.null(mean.length)) paste0(", l = ", mean.length) else NULL,
             ", m = ", m,
             if(show.n.its) paste0(" (", length(iterative.res$lambdas)-1," steps)") else NULL,
             "\nITSES-",method,
             if(show.method) paste0("(",met ,")") else NULL,
             " risk ratio: ",
             itses.rr,
             if(method == "ST") "\nSS risk ratio: " else "\nUniv.risk ratio: ",
             if(method == "ST") ss.rr else visu.rr

), adj = 0)

    cha <- getnextchar(cha)

  if(!extend) {
    return(list(cha=cha))
  }


  plot(1:length(iterative.res$lambdas)-1,
       iterative.res$lambdas,type = "l", xlab = "Iteration", ylab = TeX(r"($\lambda$)"),
  ylim = c(0, 5))
  title(paste0(cha, ")"), adj = 0)

  if (inc.legend) {
   legend("topright", col = c("black", "purple"),
         legend = c(TeX(r"($\lambda_{ITSES} w/its.$)"), TeX(r"($\lambda^*$)")),
  lty = 1)
  }


  abline(h=risk.oracle, col = "purple")

  cha <- getnextchar(cha)

  risks <- sapply(1:length(iterative.res$iteration_result), function(i) min(iterative.res$iteration_result[[i]][,2]))
  true.risks <- sapply(iterative.res$lambdas, function(lambda) sd.true^2*risk(theta/sd.true, lambda/sd.true))
  plot(1:length(risks),
       risks,type = "l", xlab = "Iteration", ylab = "Squared Error",
       ylim = c(0, risk.max),
  xlim = c(0, max(length(risks))))
  title(paste0(cha, ")"), adj = 0)

  lines(1:length(true.risks)-1,true.risks, col = "red")

  abline(h=true.risk.oracle, col = "purple")



  title(paste0(cha, ")"), adj = 0)

  if (inc.legend) {
  legend("topright", col = c("black", "red", "purple"),
         legend = c("Estimated risk w/its.", TeX(r"(True Risk of $\lambda_{ITSES}$ w/its.)"), TeX(r"(Risk of oracle threshold $\lambda^*$)")),
  lty = 1)
  }


  cha <- getnextchar(cha)

  list(cha=cha)

}