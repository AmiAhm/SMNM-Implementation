setwd("~/GitHub/SMNM-Implementation")
library(latex2exp)

q <- function(kappa, a) {
  (1+kappa^2)*(2*(a+1)*pnorm(-kappa)-1)-2*kappa*dnorm(kappa)*(a+1)
}
q.a.inv.sqrt.a <- function(a) {
  q(1/sqrt(a), a)
}

q.logloga <- function(a) {
  q(log(log(a)), a)
}

q.sqrt2loga <- function(a) {
  q(sqrt(2*log(a)), a)
}



pdf("output/figures/qinc.pdf", height = 3, width = 6)
    # Change layout to allow for legend outside to the left.
  layout.matrix <- matrix(c(1, 2,3), nrow = 1, ncol = 3)
layout(mat = layout.matrix,
       heights = c(4), # Heights of the two rows
       widths =c(2.9,2.9,1.1)) # Widths of the two columns
par(mar=c(5,4,4,0.5))

as <- seq(from = 1, to = 6, by = 0.01)
qas <- sapply(as, q.a.inv.sqrt.a)

plot(as, qas, type = "l",
     main = "a)",
     xlab = TeX(r"($a$)"),
     ylab = TeX(r"($q_a(f(a))$)")
     )
abline(h = 0, lty = 2)
points(3, q.a.inv.sqrt.a(3), pch = 3, col = "red")

as <- seq(from = 3, to = 1e3, by = 0.1)
qaslogloga <- sapply(as, q.logloga)
qassqrt2loga <- sapply(as, q.sqrt2loga)

plot(as, qassqrt2loga, type = "l", xlim = c(1, 1e3),
     main = "b)",
     ylim = c(min(qassqrt2loga),  max(qaslogloga)),
     ylab = TeX(r"($q_a(f(a))$)"), col = "red",
xlab = "a")
lines(as, qaslogloga, col = "blue")
abline(h=0, lty = 2)


  par(mar=c(0, 0, 0, 0))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("left",
       title = TeX(r"($f(a)$, $a \geq 3$)"),
  lty = c(
    #1,
    1,1,1),


       col = c(
         "black",
         "blue", "red"),
  legend = c(
    TeX(r"($1/\sqrt{a}$)"),
             TeX(r"($\log(\log(a))$)"),
                    TeX(r"($\sqrt{2\log a}$)"))
)
dev.off()

q.a.inv.sqrt.a(3)


