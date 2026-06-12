N     <- 150:270
alpha <- 0.025
p0    <- 0.89
p1    <- 0.95

crit  <- qbinom(p = 1 - alpha, size = N, prob = p0)
power <- 1 - pbinom(q = crit, size = N, prob = p1)

par(
  mar    = c(4.5, 4.5, 1.5, 1.5),
  family = "sans",
  mgp    = c(2.8, 0.7, 0),
  tcl    = -0.3
)

plot(N, power,
     type = "l",
     lwd  = 1.8,
     col  = "#1D9E75",
     las  = 1,
     xlab = "Sample size (n)",
     ylab = "Power",
     ylim = c(0.65, 1.00),
     yaxt = "n",
     xaxt = "n",
     bty  = "l")

axis(2, at = seq(0.65, 1.00, by = 0.05),
     labels = sprintf("%.2f", seq(0.65, 1.00, by = 0.05)),
     las = 1, cex.axis = 0.85)

axis(1, at = seq(150, 270, by = 20),
     cex.axis = 0.85)

legend("bottomright",
       title     = "Example design",
       title.col = "#2C2C2A",
       legend    = c(
         expression(paste(p[0], " = 0.11  (performance goal)")),
         expression(paste(p[1], " = 0.05  (expected proportion)")),
         expression(paste(alpha,  " = 0.025  (one-sided)"))
       ),
       col    = NA,
       lty    = NA,
       pch    = NA,
       bty    = "n",
       cex    = 0.82,
       text.col = "#888780")