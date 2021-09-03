setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
library(itses)

theta <- c(0, 0.1, 0.5, 2, 3)

sds <- seq(from = 0.001, to = 3, length.out = 100)
optimal.thresholds <- sapply(sds, function(sd) {
  print(sd)
  oracle.lambda.st.risk <- sd*itses:::get.risk.oracle.threshold(theta = theta/sd, method = "ST",
                                                        max_num_iters = 5000,k = 500)$lambda
  oracle.lambda.ht.risk <- sd*itses:::get.risk.oracle.threshold(theta = theta/sd,
                                                        max_num_iters = 5000,
                                                        method = "HT", k = 500)$lambda

  r.st <- sd^2*itses:::risk.st(theta/sd, oracle.lambda.st.risk/sd)
  r.ht <- sd^2*itses:::risk.ht(theta/sd, oracle.lambda.ht.risk/sd)

  c(oracle.lambda.st.risk, oracle.lambda.ht.risk, r.st, r.ht)

}
)
library(latex2exp)

pdf("output/figures/sd-op-threshold.pdf", height = 4, width = 7)
par(mfrow=c(1,2), mgp = c(3, 0.5, 0), mar = c(5,4.7,4,2))
plot(sds, optimal.thresholds[2,], type = "l", col = "red",
     ylim = c(0, 5),
     xlab = TeX(r"($\sigma$)"),
     main = "a)",
     ylab = TeX(r"($\lambda^*$)") )
lines(sds, optimal.thresholds[1,], type = "l", col = "blue")


plot(sds, optimal.thresholds[4,], type = "l", col = "red",
     xlab = TeX(r"($\sigma$)"),
     main = "b)",
     ylab = TeX(r"($R(\theta, \hat{\theta}_{\lambda^*}(\mathbf{Y})$)") )
lines(sds, optimal.thresholds[3,], type = "l", col = "blue")
#legend("topleft", col = c(4, 2), lty = 1, legend = c("ST", "HT"))
dev.off()