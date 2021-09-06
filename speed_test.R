setwd("~/GitHub/SMNM-Implementation/")
source("R-utility/extra-utility.R")


repetitions <- 100
speeds <- c()
for(i in 1:repetitions){
  print(i)
  data <- just.give.me.some.data(n = 1000, mean.length = 1, seed = i)
  y <- data$y

  res <- c()

  start_time <- Sys.time()
  get.sureshrink.threshold(y)
  end_time <- Sys.time()
  res <- c(res, end_time - start_time)


  start_time <- Sys.time()
  itses(y, method = "ST")
  end_time <- Sys.time()
  res <- c(res, end_time - start_time)


  start_time <- Sys.time()
  itses(y, method = "HT")
  end_time <- Sys.time()
  res <- c(res, end_time - start_time)

  start_time <- Sys.time()
  itses(y, method = "ST", minimizationmethod = "sampling")
  end_time <- Sys.time()
  res <- c(res, end_time - start_time)


  start_time <- Sys.time()
  itses(y, method = "HT", minimizationmethod = "sampling")
  end_time <- Sys.time()
  res <- c(res, end_time - start_time)

  speeds <- rbind(speeds, res)

}

save(speeds, file="output/speeds.RData")

load("output/speeds.RData")
speeds <- apply(speeds, 2, mean)
names(speeds) <- c("SS", "ITSES-ST (numeric)", "ITSES-HT (numeric)", "ITSES-ST (sampling)", "ITSES-HT (sampling)")

speeds <- as.matrix(speeds)
library(xtable)
print(xtable(speeds, type = "latex"), file = "output/speeds.tex")
