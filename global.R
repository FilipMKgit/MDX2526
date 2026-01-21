################################################################
#packages
################################################################
library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(thematic)
library(binom)
library(plotly)

################################################################
#themes + plot theme
################################################################
default_mode <- bs_theme(bootswatch = "flatly")
dark_mode <- bs_theme(bootswatch = "darkly")

plot_theme_large <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x  = element_text(size = 14),
  axis.text.y  = element_text(size = 14)
)

thematic_shiny()

################################################################
#proportions: analytic z power formula (fast)
################################################################
total_sample_size_prop <- function(p0, p1, delta, sig.level, power, r = 1) {
  
  #note: used when Method = "Z (power formula)" in the proportions tab
  
  if (is.na(sig.level) || sig.level <= 0 || sig.level >= 1) return(Inf)
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  if (is.na(r) || r <= 0) return(Inf)
  
  if (p0 <= 0 || p0 >= 1) return(Inf)
  if (p1 <= 0 || p1 >= 1) return(Inf)
  if (delta <= 0) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sig.level)
  z_beta  <- stats::qnorm(power)
  
  eff <- (p1 - p0 + delta)
  if (eff <= 0) return(Inf)
  
  p_bar <- (p0 + r * p1) / (1 + r)
  
  v0 <- (1 + 1 / r) * p_bar * (1 - p_bar)
  v1 <- p0 * (1 - p0) + (1 / r) * p1 * (1 - p1)
  
  n0 <- ((z_alpha * sqrt(v0) + z_beta * sqrt(v1))^2) / (eff^2)
  n1 <- r * n0
  
  n0 <- ceiling(n0)
  n1 <- ceiling(n1)
  
  n0 + n1
}

################################################################
#proportions: CI + simulation power sizing (slower, method-dependent)
################################################################
prop_ci_vec <- function(x, n, conf.level, method) {
  
  #note: used inside simulation sizing when Method != "Z (power formula)"
  #note: this returns CI bounds for each simulated x
  
  if (method == "z") {
    p_hat <- x / n
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    se <- sqrt(p_hat * (1 - p_hat) / n)
    lower <- pmax(0, p_hat - z * se)
    upper <- pmin(1, p_hat + z * se)
    return(list(lower = lower, upper = upper))
  }
  
  out <- binom::binom.confint(x = x, n = n, conf.level = conf.level, methods = method)
  list(lower = out$lower, upper = out$upper)
}

prop_power_ci_sim <- function(p0, p1, delta, alpha, r = 1,
                              ci_method = "wilson",
                              n0,
                              nsim = 1000,
                              seed = 1) {
  
  #note: estimates power for the CI rule at a fixed n0
  #note: decision rule is lower(p1) - upper(p0) > -delta
  
  if (is.infinite(n0) || is.na(n0) || n0 < 2) return(0)
  
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  if (is.na(r) || r <= 0) return(0)
  
  if (p0 <= 0 || p0 >= 1) return(0)
  if (p1 <= 0 || p1 >= 1) return(0)
  if (delta <= 0) return(0)
  
  conf.level <- 1 - 2 * alpha
  n1 <- ceiling(r * n0)
  
  set.seed(seed)
  
  x0 <- stats::rbinom(n = nsim, size = n0, prob = p0)
  x1 <- stats::rbinom(n = nsim, size = n1, prob = p1)
  
  ci0 <- prop_ci_vec(x0, n0, conf.level, ci_method)
  ci1 <- prop_ci_vec(x1, n1, conf.level, ci_method)
  
  rd_lower <- ci1$lower - ci0$upper
  mean(rd_lower > -delta)
}

total_sample_size_prop_ci_power <- function(p0, p1, delta, alpha, power,
                                            r = 1, ci_method = "wilson",
                                            nsim = 1000, seed = 1,
                                            n0_max = 200000) {
  
  #note: used when Method != "Z (power formula)" in the proportions tab
  #note: binary search on n0 until simulated power hits the target
  
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  p_hi <- prop_power_ci_sim(
    p0 = p0, p1 = p1, delta = delta, alpha = alpha, r = r,
    ci_method = ci_method, n0 = n0_max, nsim = nsim, seed = seed + 999
  )
  if (p_hi < power) return(Inf)
  
  lo <- 2
  hi <- n0_max
  
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    
    p_mid <- prop_power_ci_sim(
      p0 = p0, p1 = p1, delta = delta, alpha = alpha, r = r,
      ci_method = ci_method, n0 = mid, nsim = nsim, seed = seed + mid
    )
    
    if (p_mid >= power) {
      hi <- mid
    } else {
      lo <- mid + 1
    }
  }
  
  n0 <- lo
  n1 <- ceiling(r * n0)
  n0 + n1
}

################################################################
#continuous: analytic z power formula (fast)
################################################################
total_sample_size_mean <- function(mu0, mu1, sd, delta, sigma, power, r = 1) {
  
  #note: used in the continuous tab plots + the "N at chosen Δ" box
  
  if (is.na(sigma) || sigma <= 0 || sigma >= 1) return(Inf)
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  if (is.na(r) || r <= 0) return(Inf)
  
  if (is.na(sd) || sd <= 0) return(Inf)
  if (is.na(delta) || delta < 0) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sigma)
  z_beta  <- stats::qnorm(power)
  
  diff_exp <- mu1 - mu0
  eff <- diff_exp + delta
  if (eff <= 0) return(Inf)
  
  n_con <- ((1 + 1 / r) * (sd^2) * (z_alpha + z_beta)^2) / (eff^2)
  n_treat <- r * n_con
  
  n_con <- ceiling(n_con)
  n_treat <- ceiling(n_treat)
  
  n_con + n_treat
}
