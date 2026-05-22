# =============================================================================
# global.R  —  PG-Power
# Single-arm performance goal sample size calculator.
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(thematic)
library(binom)
library(plotly)
library(shinybusy)
library(base64enc)
library(TrialSize)

# ── Theme ─────────────────────────────────────────────────────────────────────
default_mode <- bs_theme(bootswatch = "litera")

# ── Shared ggplot2 theme ───────────────────────────────────────────────────────
plot_theme_large <- theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(size=17, face="bold", colour="#1a2e35", margin=margin(b=8)),
    axis.title       = element_text(size=13, colour="#4a5568"),
    axis.text        = element_text(size=11, colour="#718096"),
    panel.grid.major = element_line(colour="#edf0f4", linewidth=0.6),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill="transparent", colour=NA),
    panel.background = element_rect(fill="white",       colour=NA),
    plot.margin      = margin(14, 18, 14, 14)
  )

thematic_shiny(font = NA)

# ── NULL coalesce operator ────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# UI helpers
# =============================================================================

box_ui <- function(title, msg) {
  tags$div(
    class = "n-result-box",
    tags$div(class = "n-result-title", title),
    tags$div(class = "n-result-msg",   msg)
  )
}

acc_panel <- function(id, heading, open = FALSE, ...) {
  body_class <- paste0("pgp-accordion-body", if (open) " open" else "")
  hdr_class  <- paste0("pgp-accordion-header", if (open) " open" else "")
  tags$div(
    id    = id,
    class = "pgp-accordion",
    tags$div(class = hdr_class,
             tags$span(heading),
             tags$span("\u25be", class = "pgp-accordion-chevron")
    ),
    tags$div(class = body_class, ...)
  )
}

# =============================================================================
# Statistical functions
# =============================================================================

# -- Proportion CI (vectorised) -----------------------------------------------
# Used by the CI simulation binary search.
prop_ci_vec <- function(x, n, conf.level, method) {
  if (length(n) == 1 && length(x) > 1) n <- rep(n, length(x))
  if (method == "z") {
    p_hat <- x / n
    z     <- stats::qnorm(1 - (1 - conf.level) / 2)
    se    <- sqrt(p_hat * (1 - p_hat) / n)
    return(list(lower = pmax(0, p_hat - z*se), upper = pmin(1, p_hat + z*se)))
  }
  out <- tryCatch(
    suppressWarnings(
      binom::binom.confint(x=x, n=n, conf.level=conf.level, methods=method)
    ),
    error = function(e) NULL
  )
  if (is.null(out)) return(list(lower=rep(NA_real_, length(x)),
                                upper=rep(NA_real_, length(x))))
  list(lower=as.numeric(out$lower), upper=as.numeric(out$upper))
}

# -- Single-arm CI simulation power at a given n ──────────────────────────────
# p0 = performance goal (already mirrored for safety if needed)
# p1 = expected device rate (already mirrored for safety if needed)
# delta = 0 (pure PG test, no NI margin)
prop_power_ci_sim_1arm <- function(p0, p1, delta = 0, alpha,
                                   ci_method = "wilson", n,
                                   nsim = 1000, seed = 1) {
  if (is.infinite(n) || is.na(n) || n < 2)       return(0)
  if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) return(0)
  if (p0 <= 0 || p0 >= 1 || p1 <= 0 || p1 >= 1)  return(0)
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

# -- Single-arm binary search for minimum N (simulation) ─────────────────────
total_sample_size_prop_ci_power_1arm <- function(p0, p1, delta = 0,
                                                 alpha, power,
                                                 ci_method = "wilson",
                                                 nsim = 1000, seed = 1,
                                                 n_max = 200000) {
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  p_hi <- prop_power_ci_sim_1arm(p0, p1, delta, alpha, ci_method,
                                 n=n_max, nsim=nsim, seed=seed+999)
  if (is.na(p_hi) || p_hi < power) return(Inf)
  lo <- 2; hi <- n_max
  while (lo < hi) {
    mid   <- floor((lo + hi) / 2)
    p_mid <- prop_power_ci_sim_1arm(p0, p1, delta, alpha, ci_method,
                                    n=mid, nsim=nsim, seed=seed+mid)
    if (is.na(p_mid)) p_mid <- 0
    if (p_mid >= power) hi <- mid else lo <- mid + 1
  }
  lo
}