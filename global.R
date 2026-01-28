library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(thematic)
library(binom)
library(plotly)

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

total_sample_size_prop <- function(p0, p1, delta, sig.level, power, r = 1) {
  if (is.na(sig.level) || sig.level <= 0 || sig.level >= 1) return(Inf)
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  if (is.na(r) || r <= 0) return(Inf)
  
  if (p0 <= 0 || p0 >= 1) return(Inf)
  if (p1 <= 0 || p1 >= 1) return(Inf)
  if (is.na(delta) || delta < 0) return(Inf)
  
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

prop_ci_vec <- function(x, n, conf.level, method) {
  if (length(n) == 1 && length(x) > 1) {
    n <- rep(n, length(x))
  }
  
  if (method == "z") {
    p_hat <- x / n
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    se <- sqrt(p_hat * (1 - p_hat) / n)
    lower <- pmax(0, p_hat - z * se)
    upper <- pmin(1, p_hat + z * se)
    return(list(lower = lower, upper = upper))
  }
  
  out <- tryCatch(
    binom::binom.confint(x = x, n = n, conf.level = conf.level, methods = method),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(list(lower = rep(NA_real_, length(x)), upper = rep(NA_real_, length(x))))
  }
  
  list(lower = as.numeric(out$lower), upper = as.numeric(out$upper))
}

prop_power_ci_sim <- function(p0, p1, delta, alpha, r = 1,
                              ci_method = "wilson",
                              n0,
                              nsim = 1000,
                              seed = 1) {
  if (is.infinite(n0) || is.na(n0) || n0 < 2) return(0)
  
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  if (is.na(r) || r <= 0) return(0)
  
  if (p0 <= 0 || p0 >= 1) return(0)
  if (p1 <= 0 || p1 >= 1) return(0)
  if (is.na(delta) || delta < 0) return(0)
  
  conf.level <- 1 - 2 * alpha
  n1 <- ceiling(r * n0)
  
  set.seed(seed)
  
  x0 <- stats::rbinom(n = nsim, size = n0, prob = p0)
  x1 <- stats::rbinom(n = nsim, size = n1, prob = p1)
  
  ci0 <- prop_ci_vec(x0, n0, conf.level, ci_method)
  ci1 <- prop_ci_vec(x1, n1, conf.level, ci_method)
  
  rd_lower <- ci1$lower - ci0$upper
  hit <- (rd_lower > -delta)
  hit[is.na(hit)] <- FALSE
  mean(hit)
}

total_sample_size_prop_ci_power <- function(p0, p1, delta, alpha, power,
                                            r = 1, ci_method = "wilson",
                                            nsim = 1000, seed = 1,
                                            n0_max = 200000) {
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  p_hi <- prop_power_ci_sim(
    p0 = p0, p1 = p1, delta = delta, alpha = alpha, r = r,
    ci_method = ci_method, n0 = n0_max, nsim = nsim, seed = seed + 999
  )
  
  if (is.na(p_hi)) p_hi <- 0
  if (p_hi < power) return(Inf)
  
  lo <- 2
  hi <- n0_max
  
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    
    p_mid <- prop_power_ci_sim(
      p0 = p0, p1 = p1, delta = delta, alpha = alpha, r = r,
      ci_method = ci_method, n0 = mid, nsim = nsim, seed = seed + mid
    )
    
    if (is.na(p_mid)) p_mid <- 0
    
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

total_sample_size_prop_1arm <- function(p0, p1, delta, sig.level, power) {
  if (is.na(sig.level) || sig.level <= 0 || sig.level >= 1) return(Inf)
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  if (p0 <= 0 || p0 >= 1) return(Inf)
  if (p1 <= 0 || p1 >= 1) return(Inf)
  if (is.na(delta) || delta < 0) return(Inf)
  
  p_thr <- p0 - delta
  if (p_thr <= 0 || p_thr >= 1) return(Inf)
  
  eff <- p1 - p_thr
  if (eff <= 0) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sig.level)
  z_beta  <- stats::qnorm(power)
  
  v0 <- p_thr * (1 - p_thr)
  v1 <- p1   * (1 - p1)
  
  n <- ((z_alpha * sqrt(v0) + z_beta * sqrt(v1))^2) / (eff^2)
  ceiling(n)
}

prop_power_ci_sim_1arm <- function(p0, p1, delta, alpha,
                                   ci_method = "wilson",
                                   n,
                                   nsim = 1000,
                                   seed = 1) {
  if (is.infinite(n) || is.na(n) || n < 2) return(0)
  
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  
  if (p0 <= 0 || p0 >= 1) return(0)
  if (p1 <= 0 || p1 >= 1) return(0)
  if (is.na(delta) || delta < 0) return(0)
  
  p_thr <- p0 - delta
  if (p_thr <= 0 || p_thr >= 1) return(0)
  
  conf.level <- 1 - 2 * alpha
  
  set.seed(seed)
  
  x <- stats::rbinom(n = nsim, size = n, prob = p1)
  ci <- prop_ci_vec(x, n, conf.level, ci_method)
  
  hit <- (ci$lower > p_thr)
  hit[is.na(hit)] <- FALSE
  mean(hit)
}

total_sample_size_prop_ci_power_1arm <- function(p0, p1, delta, alpha, power,
                                                 ci_method = "wilson",
                                                 nsim = 1000, seed = 1,
                                                 n_max = 200000) {
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  p_hi <- prop_power_ci_sim_1arm(
    p0 = p0, p1 = p1, delta = delta, alpha = alpha,
    ci_method = ci_method, n = n_max, nsim = nsim, seed = seed + 999
  )
  
  if (is.na(p_hi)) p_hi <- 0
  if (p_hi < power) return(Inf)
  
  lo <- 2
  hi <- n_max
  
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    
    p_mid <- prop_power_ci_sim_1arm(
      p0 = p0, p1 = p1, delta = delta, alpha = alpha,
      ci_method = ci_method, n = mid, nsim = nsim, seed = seed + mid
    )
    
    if (is.na(p_mid)) p_mid <- 0
    
    if (p_mid >= power) {
      hi <- mid
    } else {
      lo <- mid + 1
    }
  }
  
  lo
}

total_sample_size_mean <- function(mu0, mu1, sd, delta, sigma, power, r = 1) {
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
