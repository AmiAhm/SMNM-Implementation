library(latex2exp)

setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")

plot.risk <- function(risk.fun = itses:::risk.st, name = "st") {
mus <- seq(from = 0, to = 5, by = 1)
kappas <- seq(0.01, 10, length.out = 1000)

risks <- sapply(mus, function(mu) sapply(kappas,
                                function(kappa) risk.fun(mu, kappa)))
pdf(file=paste0("output/figures//riskplot_", name, "_kappa.pdf"), width = 4, height = 4)

  # Change layout to allow for legend outside to the left.
  layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(3,1)) # Widths of the two columns
par(mar=c(5,4,4,0.5))

plot(kappas, risks[,1], type = "l", col = 1,
     xlab = TeX("$\\kappa$"),
     ylab = "Risk",
     ylim = c(0, max(risks)),
     main = TeX(paste0("$R(\\mu, \\hat{\\theta}_{\\kappa}^{", name, "}(Z))$")))
for(i in 2:length(mus)) {
  lines(kappas, risks[,i], type = "l", col = i)
}

par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left",
       legend = mus,
       col = 1:length(mus),
       lty = 1,
       title = TeX("$\\mu$")
       )
dev.off()


# Different mu, kappa fixed
kappas <- seq(from = 0, to = 5, by = 1)
mus <- seq(-10, 10, length.out = 1000)

risks <- sapply(kappas, function(kappa) sapply(mus,
                                               function(mu) risk.fun(mu, kappa)))

  pdf(file=paste0("output/figures/riskplot_", name, "_mu.pdf"), width = 4, height = 4)

    # Change layout to allow for legend outside to the left.
  layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(3,1)) # Widths of the two columns
par(mar=c(5,4,4,0.5))


plot(mus, risks[,1], type = "l", col = 1,
     xlab = TeX("$\\mu$"),
     ylab = "Risk",
     ylim = c(0, max(risks)),
     main = TeX(paste0("$R(\\mu, \\hat{\\theta}_{\\kappa}^{", name, "}(Z))$")))
for(i in 2:length(kappas)) {
  lines(mus, risks[,i], type = "l", col = i)
}

  par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left",
       legend = kappas,
       col = 1:length(kappas),
       lty = 1,
       title = TeX("$\\kappa$")
)
dev.off()
}


plot.risk(risk.fun = itses:::risk.st, name = "ST")
plot.risk(risk.fun = itses:::risk.ht, name = "HT")