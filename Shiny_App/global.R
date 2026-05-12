library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(thematic)
library(binom)
library(plotly)
library(shinybusy)
library(base64enc)

default_mode <- bs_theme(bootswatch = "litera")

plot_theme_large <- theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(size = 17, face = "bold",
                                     colour = "#1a2e35", margin = margin(b = 8)),
    axis.title        = element_text(size = 13, colour = "#4a5568"),
    axis.text         = element_text(size = 11, colour = "#718096"),
    panel.grid.major  = element_line(colour = "#edf0f4", linewidth = 0.6),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(14, 18, 14, 14)
  )

thematic_shiny(font = NA)

# -- UI helper: result box ---------------------------------------------------
box_ui <- function(title, msg) {
  tags$div(
    class = "n-result-box",
    tags$div(class = "n-result-title", title),
    tags$div(class = "n-result-msg",   msg)
  )
}

# -- UI helper: accordion panel ----------------------------------------------
acc_panel <- function(id, heading, open = FALSE, ...) {
  body_class <- if (open) "pgp-accordion-body open" else "pgp-accordion-body"
  hdr_class  <- if (open) "pgp-accordion-header open" else "pgp-accordion-header"
  tags$div(
    id    = id,
    class = "pgp-accordion",
    tags$div(
      class = hdr_class,
      tags$span(heading),
      tags$span("\u25be", class = "pgp-accordion-chevron")
    ),
    tags$div(class = body_class, ...)
  )
}

# -- Proportion CI helper ----------------------------------------------------
prop_ci_vec <- function(x, n, conf.level, method) {
  if (length(n) == 1 && length(x) > 1) n <- rep(n, length(x))
  
  if (method == "z") {
    p_hat <- x / n
    z     <- stats::qnorm(1 - (1 - conf.level) / 2)
    se    <- sqrt(p_hat * (1 - p_hat) / n)
    return(list(lower = pmax(0, p_hat - z * se),
                upper = pmin(1, p_hat + z * se)))
  }
  
  out <- tryCatch(
    binom::binom.confint(x = x, n = n, conf.level = conf.level, methods = method),
    error = function(e) NULL
  )
  
  if (is.null(out))
    return(list(lower = rep(NA_real_, length(x)),
                upper = rep(NA_real_, length(x))))
  
  list(lower = as.numeric(out$lower), upper = as.numeric(out$upper))
}

# -- Two-arm analytic N ------------------------------------------------------
total_sample_size_prop <- function(p0, p1, delta, sig.level, power, r = 1) {
  if (is.na(sig.level) || sig.level <= 0 || sig.level >= 1) return(Inf)
  if (is.na(power)    || power    <= 0 || power    >= 1) return(Inf)
  if (is.na(r)        || r        <= 0)                  return(Inf)
  if (p0 <= 0 || p0 >= 1) return(Inf)
  if (p1 <= 0 || p1 >= 1) return(Inf)
  if (is.na(delta) || delta < 0) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sig.level)
  z_beta  <- stats::qnorm(power)
  eff     <- p1 - p0 + delta
  if (eff <= 0) return(Inf)
  
  p_bar <- (p0 + r * p1) / (1 + r)
  v0    <- (1 + 1 / r) * p_bar * (1 - p_bar)
  v1    <- p0 * (1 - p0) + (1 / r) * p1 * (1 - p1)
  
  n0 <- ((z_alpha * sqrt(v0) + z_beta * sqrt(v1))^2) / eff^2
  ceiling(n0) + ceiling(r * n0)
}

# -- Two-arm simulation power ------------------------------------------------
prop_power_ci_sim <- function(p0, p1, delta, alpha, r = 1,
                              ci_method = "wilson", n0,
                              nsim = 1000, seed = 1) {
  if (is.infinite(n0) || is.na(n0) || n0 < 2) return(0)
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  if (is.na(r) || r <= 0) return(0)
  if (p0 <= 0 || p0 >= 1) return(0)
  if (p1 <= 0 || p1 >= 1) return(0)
  if (is.na(delta) || delta < 0) return(0)
  
  conf.level <- 1 - 2 * alpha
  n1 <- ceiling(r * n0)
  set.seed(seed)
  
  x0  <- stats::rbinom(nsim, n0, p0)
  x1  <- stats::rbinom(nsim, n1, p1)
  ci0 <- prop_ci_vec(x0, n0, conf.level, ci_method)
  ci1 <- prop_ci_vec(x1, n1, conf.level, ci_method)
  
  hit <- (ci1$lower - ci0$upper) > -delta
  hit[is.na(hit)] <- FALSE
  mean(hit)
}

total_sample_size_prop_ci_power <- function(p0, p1, delta, alpha, power,
                                            r = 1, ci_method = "wilson",
                                            nsim = 1000, seed = 1,
                                            n0_max = 200000) {
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  p_hi <- prop_power_ci_sim(p0, p1, delta, alpha, r, ci_method,
                            n0 = n0_max, nsim = nsim, seed = seed + 999)
  if (is.na(p_hi)) p_hi <- 0
  if (p_hi < power) return(Inf)
  
  lo <- 2; hi <- n0_max
  while (lo < hi) {
    mid   <- floor((lo + hi) / 2)
    p_mid <- prop_power_ci_sim(p0, p1, delta, alpha, r, ci_method,
                               n0 = mid, nsim = nsim, seed = seed + mid)
    if (is.na(p_mid)) p_mid <- 0
    if (p_mid >= power) hi <- mid else lo <- mid + 1
  }
  
  n0 <- lo
  n0 + ceiling(r * n0)
}

# -- Single-arm analytic N ----------------------------------------------------
total_sample_size_prop_1arm <- function(p0, p1, delta, sig.level, power) {
  if (is.na(sig.level) || sig.level <= 0 || sig.level >= 1) return(Inf)
  if (is.na(power)    || power    <= 0 || power    >= 1) return(Inf)
  if (p0 <= 0 || p0 >= 1) return(Inf)
  if (p1 <= 0 || p1 >= 1) return(Inf)
  if (is.na(delta) || delta < 0) return(Inf)
  
  p_thr <- p0 - delta
  if (p_thr <= 0 || p_thr >= 1) return(Inf)
  
  eff <- p1 - p_thr
  if (eff <= 0) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sig.level)
  z_beta  <- stats::qnorm(power)
  
  ceiling(((z_alpha * sqrt(p_thr * (1 - p_thr)) +
              z_beta  * sqrt(p1   * (1 - p1)))^2) / eff^2)
}

# -- Single-arm simulation power ----------------------------------------------
prop_power_ci_sim_1arm <- function(p0, p1, delta, alpha,
                                   ci_method = "wilson", n,
                                   nsim = 1000, seed = 1) {
  if (is.infinite(n) || is.na(n) || n < 2) return(0)
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  if (p0 <= 0 || p0 >= 1) return(0)
  if (p1 <= 0 || p1 >= 1) return(0)
  if (is.na(delta) || delta < 0) return(0)
  
  p_thr <- p0 - delta
  if (p_thr <= 0 || p_thr >= 1) return(0)
  
  conf.level <- 1 - 2 * alpha
  set.seed(seed)
  
  x  <- stats::rbinom(nsim, n, p1)
  ci <- prop_ci_vec(x, n, conf.level, ci_method)
  
  hit <- ci$lower > p_thr
  hit[is.na(hit)] <- FALSE
  mean(hit)
}

total_sample_size_prop_ci_power_1arm <- function(p0, p1, delta, alpha, power,
                                                 ci_method = "wilson",
                                                 nsim = 1000, seed = 1,
                                                 n_max = 200000) {
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  p_hi <- prop_power_ci_sim_1arm(p0, p1, delta, alpha, ci_method,
                                 n = n_max, nsim = nsim, seed = seed + 999)
  if (is.na(p_hi)) p_hi <- 0
  if (p_hi < power) return(Inf)
  
  lo <- 2; hi <- n_max
  while (lo < hi) {
    mid   <- floor((lo + hi) / 2)
    p_mid <- prop_power_ci_sim_1arm(p0, p1, delta, alpha, ci_method,
                                    n = mid, nsim = nsim, seed = seed + mid)
    if (is.na(p_mid)) p_mid <- 0
    if (p_mid >= power) hi <- mid else lo <- mid + 1
  }
  
  lo
}

# -- Interim CI threshold helper -----------------------------------------------
# For a given n and NI boundary, finds the minimum event count that achieves NI
# (efficacy: CI lower > boundary) or maximum event count that stays NI
# (safety: CI upper < boundary), for each CI method independently.
# Called per-method with a scalar x, so avoids the binom.confint row-count issue.
interim_x_threshold <- function(n, boundary, conf_level, method, is_safety) {
  if (is.na(n) || n < 1) return(NA_integer_)
  result <- NA_integer_
  for (x in 0L:as.integer(n)) {
    ci <- tryCatch(
      prop_ci_vec(x, n, conf_level, method),
      error = function(e) list(lower = NA_real_, upper = NA_real_)
    )
    if (is_safety) {
      if (!is.na(ci$upper) && ci$upper < boundary) result <- x
      # CI upper is monotone-increasing in x; once it exceeds boundary, done
      else if (!is.na(ci$upper)) break
    } else {
      if (!is.na(ci$lower) && ci$lower > boundary) return(x)
    }
  }
  result
}