server <- function(input, output, session) {
  
  # ── Reload app ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_reload_app, { session$reload() })
  
  # ── Code block UI helper ────────────────────────────────────────────────────
  code_block_ui <- function(code_txt, margin_top = "18px") {
    tags$div(
      style = paste0("margin-top:", margin_top, ";"),
      tags$pre(
        style = paste0(
          "background:#f1f5f9; border:1px solid #e2e8f0; border-radius:8px;",
          "padding:14px 16px; font-family:'DM Mono',monospace; font-size:12px;",
          "color:#1a2e35; line-height:1.75; overflow-x:auto; white-space:pre;"
        ),
        code_txt
      )
    )
  }
  
  # ── Calculation code text ────────────────────────────────────────────────────
  build_calc_code_txt <- function() {
    is_safety <- isTRUE(input$endpoint == "safety")
    ci_m   <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    use_z  <- isTRUE(ci_m == "z_power")
    alpha  <- as.numeric(input$sig.level)
    pwr    <- input$power
    pg     <- input$p0.expected   # performance goal
    pd     <- input$p1.expected   # expected device rate
    
    if (use_z && !is_safety) {
      paste(c(
        "# Single-arm performance goal  —  TrialSize::OneSampleProportion.NIS",
        "# H0: device rate <= PG   H1: device rate > PG  (higher is better)",
        "",
        "library(TrialSize)",
        paste0("PG    <- ", pg,  "   # performance goal"),
        paste0("p_dev <- ", pd,  "   # expected device rate"),
        paste0("alpha <- ", alpha, "   # one-sided alpha"),
        paste0("beta  <- ", round(1 - pwr, 3), "   # 1 - power"),
        "",
        "n <- ceiling(",
        "  OneSampleProportion.NIS(alpha, beta,",
        "    p = p_dev, delta = PG, differ = p_dev - PG)",
        ")"
      ), collapse = "\n")
      
    } else if (use_z && is_safety) {
      paste(c(
        "# Single-arm performance goal  —  TrialSize::OneSampleProportion.NIS",
        "# H0: device rate >= PG   H1: device rate < PG  (lower is better)",
        "# Mirror: test (1 - device rate) > (1 - PG)",
        "",
        "library(TrialSize)",
        paste0("PG    <- ", pg,  "   # performance goal (max acceptable rate)"),
        paste0("p_dev <- ", pd,  "   # expected device rate"),
        paste0("alpha <- ", alpha, "   # one-sided alpha"),
        paste0("beta  <- ", round(1 - pwr, 3), "   # 1 - power"),
        "",
        "PG_m    <- 1 - PG",
        "p_dev_m <- 1 - p_dev",
        "n <- ceiling(",
        "  OneSampleProportion.NIS(alpha, beta,",
        "    p = p_dev_m, delta = PG_m, differ = p_dev_m - PG_m)",
        ")"
      ), collapse = "\n")
      
    } else {
      conf_lev <- round(1 - 2 * alpha, 3)
      dir_txt  <- if (is_safety) "lower is better" else "higher is better"
      paste(c(
        paste0("# Single-arm performance goal  —  CI simulation  (", dir_txt, ")"),
        paste0("# CI method: ", ci_m, "   conf.level: ", conf_lev),
        "",
        paste0("PG    <- ", pg, ";  p_dev <- ", pd),
        paste0("conf_level <- ", conf_lev, "   # = 1 - 2*alpha"),
        "",
        "# Simulate nsim trials; check CI bound vs performance goal",
        paste0('ci <- prop_ci_vec(x, n, conf_level, method = "', ci_m, '")'),
        if (is_safety) "# Declare success if ci$upper < PG"
        else           "# Declare success if ci$lower > PG",
        "# Binary search finds smallest n achieving target power"
      ), collapse = "\n")
    }
  }
  
  # ── Endpoint presets ─────────────────────────────────────────────────────────
  endpoint_defaults <- list(
    efficacy = list(pg = 0.88, pd = 0.93),
    safety   = list(pg = 0.14, pd = 0.06)
  )
  observeEvent(input$endpoint, {
    d <- endpoint_defaults[[input$endpoint]]
    if (is.null(d)) return()
    updateSliderInput(session, "p0.expected", value = d$pg)
    updateSliderInput(session, "p1.expected", value = d$pd)
  }, ignoreInit = TRUE)
  
  # ── Alpha badge ──────────────────────────────────────────────────────────────
  output$alpha_display <- renderUI({
    a <- as.numeric(input$sig.level)
    if (is.null(a) || is.na(a)) return(NULL)
    badge <- list(
      `0.005` = list("very stringent",       "#1e3a5f"),
      `0.01`  = list("stringent",            "#7c3aed"),
      `0.025` = list("pivotal device",       "#5b35d5"),
      `0.04`  = list("relaxed pivotal",      "#0369a1"),
      `0.05`  = list("common exploratory",   "#e07b39"),
      `0.1`   = list("liberal",              "#94a3b8"),
      `0.15`  = list("permissive / pilot",   "#b45309")
    )
    key <- as.character(a)
    b   <- badge[[key]]
    tags$div(
      style = "font-size:11.5px; color:#64748b; margin:-6px 0 8px; line-height:1.6;",
      tags$span(paste0("\u03b1 = ", a, " \u2192 ", round(100*(1-2*a),1), "% CI")),
      if (!is.null(b)) tags$span(b[[1]], style = paste0(
        "display:inline-block; margin-left:6px; padding:1px 7px; font-size:10px;",
        "font-weight:700; border-radius:10px; letter-spacing:0.03em;",
        "background:", b[[2]], "22; color:", b[[2]], "; border:1px solid ", b[[2]], "55;"
      ))
    )
  })
  
  # ── Power badge ──────────────────────────────────────────────────────────────
  output$power_display <- renderUI({
    p <- input$power
    if (is.null(p) || is.na(p)) return(NULL)
    badge_txt <- NULL; badge_col <- NULL
    if      (abs(p - 0.80) < 0.001) { badge_txt <- "most common";             badge_col <- "#e07b39" }
    else if (abs(p - 0.85) < 0.001) { badge_txt <- "common";                  badge_col <- "#7c3aed" }
    else if (abs(p - 0.90) < 0.001) { badge_txt <- "recommended for pivotal"; badge_col <- "#5b35d5" }
    else if (p >= 0.95)              { badge_txt <- "very high";               badge_col <- "#94a3b8" }
    tags$div(
      style = "font-size:11.5px; color:#64748b; margin:-6px 0 8px; line-height:1.6;",
      tags$span(paste0("Power = ", round(p*100), "%  (\u03b2 = ", round((1-p)*100), "% miss rate)")),
      if (!is.null(badge_txt)) tags$span(badge_txt, style = paste0(
        "display:inline-block; margin-left:6px; padding:1px 7px; font-size:10px;",
        "font-weight:700; border-radius:10px; letter-spacing:0.03em;",
        "background:", badge_col, "22; color:", badge_col, "; border:1px solid ", badge_col, "55;"
      ))
    )
  })
  
  # ── Helpers ──────────────────────────────────────────────────────────────────
  get_dropout_rate <- reactive({
    dr <- if (is.null(input$dropout_rate)) 10 else as.numeric(input$dropout_rate)
    if (is.na(dr) || dr < 0 || dr >= 100) 10 else dr
  })
  
  get_plot_colour <- reactive({
    col <- input$plot_colour
    if (is.null(col) || !col %in% c("#5b35d5","#18bdb9","#c0392b","#1a2e35")) "#5b35d5" else col
  })
  
  # ── Core N calculation ────────────────────────────────────────────────────────
  # Primary: TrialSize::OneSampleProportion.NIS (Z formula)
  # Fallback: CI simulation (prop_ci_vec binary search) for non-z methods
  # Returns the smaller of the two when both are finite.
  #
  # Safety direction uses mirror: test (1-p) > (1-PG)
  #
  calc_n_trialsize <- function(pg, pd, alpha, beta, is_safety) {
    if (is_safety) { p_ts <- 1 - pd; d_ts <- 1 - pg }
    else           { p_ts <- pd;     d_ts <- pg       }
    differ <- p_ts - d_ts
    if (differ <= 0 || p_ts <= 0 || p_ts >= 1 || d_ts <= 0 || d_ts >= 1) return(Inf)
    result <- tryCatch(
      TrialSize::OneSampleProportion.NIS(alpha, beta, p_ts, d_ts, differ),
      error = function(e) Inf
    )
    if (is.null(result) || is.na(result) || result <= 0) Inf else ceiling(result)
  }
  
  calc_n_sim <- function(pg, pd, alpha, power, ci_method, sim_n, seed, is_safety) {
    if (is_safety)
      total_sample_size_prop_ci_power_1arm(1-pg, 1-pd, 0, alpha, power,
                                           ci_method = ci_method, nsim = sim_n, seed = seed)
    else
      total_sample_size_prop_ci_power_1arm(pg, pd, 0, alpha, power,
                                           ci_method = ci_method, nsim = sim_n, seed = seed)
  }
  
  prop_total_n <- function(pg, pd,
                           ci_method = input$ci_method_prop,
                           sim_n     = as.numeric(input$sim_quality),
                           seed      = as.numeric(input$sim_seed)) {
    alpha     <- as.numeric(input$sig.level)
    beta      <- 1 - input$power
    is_safety <- isTRUE(input$endpoint == "safety")
    
    n_ts  <- calc_n_trialsize(pg, pd, alpha, beta, is_safety)
    
    if (isTRUE(ci_method == "z_power")) return(n_ts)
    
    n_sim <- tryCatch(
      calc_n_sim(pg, pd, alpha, input$power, ci_method, sim_n, seed, is_safety),
      error = function(e) Inf
    )
    
    # Return the smaller of the two finite estimates
    vals <- c(n_ts, n_sim)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) Inf else min(vals)
  }
  
  # ── Debounced inputs ─────────────────────────────────────────────────────────
  pg_d    <- debounce(reactive(input$p0.expected), 400)   # performance goal
  pd_d    <- debounce(reactive(input$p1.expected), 400)   # expected device rate
  alpha_d <- debounce(reactive(input$sig.level),   400)
  power_d <- debounce(reactive(input$power),       400)
  
  # ── Core reactives ────────────────────────────────────────────────────────────
  prop_n <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    prop_total_n(pg_d(), pd_d())
  })
  
  # Sensitivity sweep: required n as device rate varies around its current value
  prop_df_sens <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    is_safety <- isTRUE(input$endpoint == "safety")
    pg <- pg_d()
    lo <- if (is_safety) max(0.001, pd_d() - 0.15) else max(0.001, pd_d() - 0.10)
    hi <- if (is_safety) min(pg - 0.001, pd_d() + 0.05) else min(0.999, pd_d() + 0.10)
    if (lo >= hi) return(data.frame(x = numeric(0), y = numeric(0)))
    x <- seq(lo, hi, by = 0.005)
    if (!any(abs(x - pd_d()) < 1e-9)) x <- sort(c(x, pd_d()))
    y <- sapply(x, function(pdi) prop_total_n(pg, pdi))
    data.frame(x = x, y = y)
  })
  
  # Exact binomial power curve — preserves the sawtooth pattern
  prop_df_power <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    n_req <- tryCatch(prop_n(), error = function(e) NA)
    if (is.na(n_req) || is.infinite(n_req)) return(data.frame(x = numeric(0), y = numeric(0)))
    alpha   <- as.numeric(alpha_d())
    is_sfty <- isTRUE(input$endpoint == "safety")
    pg <- if (is_sfty) 1 - pg_d() else pg_d()
    pd <- if (is_sfty) 1 - pd_d() else pd_d()
    n_max <- min(ceiling(n_req * 2.2), 2000)
    n_seq <- seq(max(2, floor(n_req * 0.1)), n_max, by = 1)
    y <- sapply(n_seq, function(n) {
      x_crit <- qbinom(1 - alpha, n, pg) + 1L
      if (x_crit > n) return(NA_real_)
      1 - pbinom(x_crit - 1L, n, pd)
    })
    data.frame(x = n_seq, y = y)
  })
  
  # ── CI comparison table ───────────────────────────────────────────────────────
  all_compare_methods <- c(
    "TrialSize (Z)"             = "z_power",
    "Wilson"                    = "wilson",
    "Exact (Clopper-Pearson)"   = "exact",
    "Agresti-Coull"             = "ac",
    "Asymptotic (Wald)"         = "asymptotic",
    "prop.test"                 = "prop.test",
    "Bayes"                     = "bayes",
    "Logit"                     = "logit",
    "Cloglog"                   = "cloglog",
    "Probit"                    = "probit"
  )
  
  compare_df <- reactive({
    req(input$showCompare)
    if (!isTRUE(input$showCompare)) return(NULL)
    ns <- vapply(all_compare_methods, function(m) {
      prop_total_n(pg_d(), pd_d(), ci_method = m)
    }, numeric(1))
    current <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    data.frame(
      Method    = names(all_compare_methods),
      `Total N` = ifelse(is.infinite(ns), "Not achievable", format(ns, big.mark = ",")),
      Selected  = ifelse(all_compare_methods == current, "\u2713", ""),
      check.names = FALSE
    )
  })
  
  # ── n result box ─────────────────────────────────────────────────────────────
  output$n_box_prop <- renderUI({
    if (!isTRUE(input$showNBox_prop)) return(NULL)
    n_out     <- prop_n()
    dropout_r <- get_dropout_rate()
    alpha_val <- as.numeric(alpha_d())
    is_safety <- isTRUE(input$endpoint == "safety")
    method_label <- if (isTRUE(input$ci_method_prop == "z_power"))
      "TrialSize (Z formula)" else paste0("CI simulation (", input$ci_method_prop, ")")
    
    if (is.infinite(n_out)) return(box_ui(
      "Required sample size",
      paste0("Not achievable (N = \u221e) — check performance goal and device rate.  [", method_label, "]")
    ))
    
    n_enrol  <- ceiling(n_out / (1 - dropout_r / 100))
    pg_val   <- as.numeric(pg_d())
    z_a      <- qnorm(1 - alpha_val)
    n_events <- if (is_safety)
      floor(n_out * pg_val   - z_a * sqrt(n_out * pg_val   * (1 - pg_val)))
    else
      ceiling(n_out * pg_val + z_a * sqrt(n_out * pg_val * (1 - pg_val)))
    
    if (!is_safety) {
      h0_txt <- paste0("H\u2080: device rate \u2264 ", pg_val, " (performance goal)")
      h1_txt <- paste0("H\u2081: device rate > ", pg_val, " (device meets performance goal)")
    } else {
      h0_txt <- paste0("H\u2080: device rate \u2265 ", pg_val, " (at or above max acceptable rate)")
      h1_txt <- paste0("H\u2081: device rate < ", pg_val, " (device meets performance goal)")
    }
    reject_rule <- if (!is_safety)
      paste0("Reject H\u2080 if \u2265 ", n_events, " of ", format(n_out, big.mark=","),
             " patients succeed  (CI lower > ", pg_val, ")")
    else
      paste0("Reject H\u2080 if \u2264 ", n_events, " of ", format(n_out, big.mark=","),
             " patients experience the event  (CI upper < ", pg_val, ")")
    
    tagList(
      box_ui("Required sample size", paste0(
        "n = ", format(n_out, big.mark=","), "  [", method_label, "]"
      )),
      tags$div(style="text-align:right;margin-top:4px;",
               tags$button(class="n-box-toggle", onclick="pgpToggleNBox(this);", "collapse \u25b4")
      ),
      tags$div(id="n_box_expanded", class="n-box-expanded", style="display:block;",
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "H\u2080:"),
                        tags$span(class="nb-val",   h0_txt)),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "H\u2081:"),
                        tags$span(class="nb-val",   h1_txt)),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Target \u03b1 (one-sided):"),
                        tags$span(class="nb-val",   paste0(alpha_val, "  \u2192 ",
                                                           round((1-2*alpha_val)*100,1), "% CI"))),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Method:"),
                        tags$span(class="nb-val",   method_label)),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Power:"),
                        tags$span(class="nb-val",   paste0(round(power_d()*100), "%"))),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Required n (evaluable):"),
                        tags$span(class="nb-val",   format(n_out, big.mark=","))),
               if (!is_safety) tags$div(class="nb-row",
                                        tags$span(class="nb-label", "Min successes needed:"),
                                        tags$span(class="nb-val",   paste0(
                                          format(n_events, big.mark=","), " / ", format(n_out, big.mark=","),
                                          " (\u2265 ", round(100*n_events/n_out, 1), "%)"))),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", paste0("Enrolment (", dropout_r, "% dropout):")),
                        tags$span(class="nb-val",   paste0(
                          format(n_enrol, big.mark=","), " to obtain ",
                          format(n_out, big.mark=","), " evaluable"))),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Decision rule:"),
                        tags$span(class="nb-val",   reject_rule))
      )
    )
  })
  
  # ── Plots ────────────────────────────────────────────────────────────────────
  finish_plotly <- function(p) {
    ggplotly(p) %>%
      layout(hovermode="x unified", paper_bgcolor="transparent",
             plot_bgcolor="white", font=list(family="DM Sans, sans-serif")) %>%
      config(displaylogo=FALSE, displayModeBar=FALSE)
  }
  
  output$plot_power <- renderPlotly({
    df    <- prop_df_power()
    pc    <- get_plot_colour()
    n_req <- tryCatch(prop_n(), error = function(e) NA)
    validate(need(nrow(df) > 0 && !all(is.na(df$y)),
                  "Could not compute power curve — check performance goal and device rate."))
    df_c   <- df[!is.na(df$y), ]
    target <- as.numeric(power_d())
    p <- ggplot(df_c, aes(x=x, y=y)) +
      geom_hline(yintercept=target, linetype="dashed", colour="#e07b39", linewidth=0.8) +
      geom_line(colour=pc, linewidth=1.1) +
      geom_point(colour=pc, size=1.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,1)) +
      labs(title="Sample size vs achieved power", x="Sample size (n)", y="Achieved power") +
      plot_theme_large
    if (!is.na(n_req) && !is.infinite(n_req)) {
      pw <- df_c$y[which.min(abs(df_c$x - n_req))]
      if (length(pw) > 0 && !is.na(pw))
        p <- p + geom_point(data=data.frame(x=n_req, y=pw), aes(x=x,y=y),
                            colour="#e07b39", size=5, shape=21, fill="#e07b39", alpha=0.85)
    }
    ggplotly(p) %>%
      layout(hovermode="x unified", paper_bgcolor="transparent", plot_bgcolor="white",
             font=list(family="DM Sans, sans-serif"),
             annotations=list(list(
               x=max(df_c$x)*0.98, y=target+0.03, yref="y",
               text=paste0("Target: ", round(target*100), "%"),
               showarrow=FALSE, xanchor="right",
               font=list(color="#e07b39", size=11)))) %>%
      config(displaylogo=FALSE, displayModeBar=FALSE)
  })
  
  output$plot2 <- renderPlotly({
    df <- prop_df_sens()
    pc <- get_plot_colour()
    validate(need(nrow(df) > 0 && !all(is.infinite(df$y)),
                  "No achievable sample size in this range — check performance goal and device rate."))
    pd_val  <- pd_d()
    idx     <- which.min(abs(df$x - pd_val))
    chosen_n <- if (length(idx) > 0) df$y[idx] else Inf
    x_lab <- if (isTRUE(input$endpoint == "safety"))
      "Expected device event rate" else "Expected device success rate"
    p <- ggplot(df, aes(x=x, y=y)) +
      geom_line(colour=pc, linewidth=1.1) +
      geom_point(colour=pc, size=2) +
      labs(title="Expected device rate vs required sample size",
           x=x_lab, y="Required sample size (n)") +
      plot_theme_large
    if (isTRUE(input$showVline))
      p <- p + geom_vline(xintercept=pd_val, linetype="dashed", colour="#e07b39", linewidth=0.9)
    if (isTRUE(input$showVline) && !is.infinite(chosen_n))
      p <- p + geom_point(data=data.frame(x=pd_val, y=chosen_n), aes(x=x,y=y),
                          colour="#e07b39", size=5, shape=21, fill="#e07b39", alpha=0.85)
    finish_plotly(p)
  })
  
  # ── CI comparison section ────────────────────────────────────────────────────
  output$compare_section <- renderUI({
    if (!isTRUE(input$showCompare)) return(NULL)
    df <- compare_df()
    if (is.null(df)) return(NULL)
    tagList(
      tags$hr(),
      tags$div(class="compare-header",
               tags$h5("CI method comparison \u2014 n for each method (\u2713 = selected)"),
               tags$p(class="compare-params", paste0(
                 "PG = ", input$p0.expected, "  |  device rate = ", input$p1.expected,
                 "  |  \u03b1 = ", input$sig.level, "  |  power = ", input$power))
      ),
      DT::renderDT(DT::datatable(df, rownames=FALSE, class="stripe hover compact",
                                 options=list(dom="t", paging=FALSE, searching=FALSE)))
    )
  })
  
  output$dataTable2 <- renderDT({
    if (!isTRUE(input$showTable2)) return(NULL)
    df <- prop_df_sens()
    colnames(df) <- c("Device Rate", "Required N")
    DT::datatable(df, rownames=FALSE, class="stripe hover compact",
                  options=list(pageLength=15))
  })
  
  # ── Generate Report helpers ──────────────────────────────────────────────────
  output$rpt_calc_summary_ui <- renderUI({
    n_out     <- tryCatch(prop_n(), error = function(e) NA)
    dropout_r <- get_dropout_rate()
    alpha_val <- as.numeric(input$sig.level)
    ci_m      <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    n_fmt     <- if (is.na(n_out) || is.infinite(n_out)) "Not achievable"
    else format(n_out, big.mark=",")
    n_enrol   <- if (is.na(n_out) || is.infinite(n_out)) NA_integer_
    else ceiling(n_out / (1 - dropout_r/100))
    nd_fmt    <- if (is.na(n_enrol)) "—" else format(n_enrol, big.mark=",")
    is_safety <- isTRUE(input$endpoint == "safety")
    pg_val    <- as.numeric(pg_d())
    z_a       <- qnorm(1 - alpha_val)
    n_succ    <- if (is.na(n_out) || is.infinite(n_out)) "—"
    else if (is_safety)
      format(floor(n_out*pg_val - z_a*sqrt(n_out*pg_val*(1-pg_val))), big.mark=",")
    else
      format(ceiling(n_out*pg_val + z_a*sqrt(n_out*pg_val*(1-pg_val))), big.mark=",")
    ci_lbl <- c(z_power="TrialSize (Z)", wilson="Wilson", exact="Clopper-Pearson",
                ac="Agresti-Coull", asymptotic="Wald", prop.test="prop.test",
                bayes="Bayes", logit="Logit", cloglog="Cloglog", probit="Probit")
    ci_lbl <- unname(ci_lbl[ci_m]); if (is.na(ci_lbl)) ci_lbl <- ci_m
    row <- function(lbl, val) tags$tr(
      tags$td(style="padding:4px 10px;font-size:12px;font-weight:600;color:#64748b;
               border:1px solid #e2e8f0;white-space:nowrap;", lbl),
      tags$td(style="padding:4px 10px;font-size:12px;color:#1a2e35;
               border:1px solid #e2e8f0;font-family:'DM Mono',monospace;", val)
    )
    tags$div(
      style="border:1px solid #e2e8f0;border-top:none;border-radius:0 0 8px 8px;
             padding:12px;background:#fafcff;",
      tags$table(style="border-collapse:collapse;width:100%;", tags$tbody(
        row("Endpoint direction",  if (is_safety) "Lower is better" else "Higher is better"),
        row("Performance goal (PG)", as.character(pg_d())),
        row("Expected device rate",  as.character(pd_d())),
        row("\u03b1 (one-sided)",    paste0(alpha_val, "  \u2192  ",
                                            round((1-2*alpha_val)*100,1), "% CI")),
        row("Power",               paste0(round(power_d()*100), "%")),
        row("Method",              ci_lbl),
        row("Required n",          n_fmt),
        row("Min successes",       n_succ),
        row(paste0("Enrolment (", dropout_r, "% dropout)"), nd_fmt)
      ))
    )
  })
  
  output$report_contents_ui <- renderUI({
    make_li <- function(label, on) {
      if (on) tags$li(tags$span("\u2713", class="rc-tick"), tags$span(label))
      else    tags$li(tags$span("\u2013", class="rc-cross"),
                      tags$span(label, style="color:#94a3b8;"))
    }
    make_group <- function(heading, items) {
      any_on <- any(sapply(items, function(x) x$on))
      if (!any_on) make_li(heading, FALSE)
      else tagList(
        tags$li(tags$span("\u2713", class="rc-tick"),
                tags$span(heading, style="font-weight:600;")),
        tags$ul(style="list-style:none;padding:0 0 0 20px;margin:0;",
                lapply(items, function(i) make_li(i$label, i$on)))
      )
    }
    general_items <- list(
      list(label="Results table",            on=isTRUE(input$rpt_results)),
      list(label="Full n summary",           on=isTRUE(input$rpt_n_box)),
      list(label="Interpretation",           on=isTRUE(input$rpt_interp_inc)),
      list(label="CI method comparison",     on=isTRUE(input$rpt_ci_compare)),
      list(label="Definitions",              on=isTRUE(input$rpt_definitions)),
      list(label="Calculation code",         on=isTRUE(input$rpt_calc_code))
    )
    sens_items <- list(
      list(label="Device rate sensitivity plot",  on=isTRUE(input$rpt_plot_p1)),
      list(label="Device rate sensitivity table", on=isTRUE(input$rpt_table_p1))
    )
    tags$ul(class="report-contents",
            lapply(general_items, function(i) make_li(i$label, i$on)),
            make_group("Sensitivity Analysis", sens_items)
    )
  })
  
  output$report_download_ui <- renderUI({
    if (isTRUE(input$report_format == "pdf"))
      downloadButton("downloadPDF", "\u2193 Download (.pdf)",
                     class="btn-sm btn-outline-secondary pgp-btn report-dl-btn")
    else
      downloadButton("downloadWord", "\u2193 Download (.docx)",
                     class="btn-sm btn-outline-secondary pgp-btn report-dl-btn")
  })
  
  # ── Report data ───────────────────────────────────────────────────────────────
  build_report_data <- function() {
    ci_m  <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    dr    <- if (is.null(input$dropout_rate)) 10 else as.numeric(input$dropout_rate)
    if (is.na(dr) || dr < 0 || dr >= 100) dr <- 10
    n_val <- prop_total_n(input$p0.expected, input$p1.expected,
                          ci_method=ci_m,
                          sim_n=if (is.null(input$sim_quality)) 1000 else as.numeric(input$sim_quality),
                          seed =if (is.null(input$sim_seed))    1    else as.numeric(input$sim_seed))
    ci_labels <- c(z_power="TrialSize (Z Formula)", wilson="Wilson Score",
                   exact="Clopper-Pearson", ac="Agresti-Coull", asymptotic="Wald",
                   prop.test="prop.test", bayes="Bayes", logit="Logit",
                   cloglog="Cloglog", probit="Probit")
    ci_lbl  <- unname(ci_labels[ci_m]); if (is.na(ci_lbl)) ci_lbl <- ci_m
    is_sfty <- isTRUE(input$endpoint == "safety")
    pg_val  <- as.numeric(input$p0.expected)
    z_a     <- qnorm(1 - as.numeric(input$sig.level))
    n_succ  <- if (is.infinite(n_val)) NA_integer_
    else if (is_sfty) floor(n_val*pg_val   - z_a*sqrt(n_val*pg_val*(1-pg_val)))
    else              ceiling(n_val*pg_val + z_a*sqrt(n_val*pg_val*(1-pg_val)))
    n_drop  <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr/100))
    list(
      n_val=n_val, ci_label=ci_lbl,
      n_fmt  = if (is.infinite(n_val)) "Not achievable" else format(n_val, big.mark=","),
      ns_fmt = if (is.na(n_succ)) "\u2014" else format(n_succ, big.mark=","),
      nd_fmt = if (is.na(n_drop)) "\u2014" else format(n_drop, big.mark=","),
      n_successes=n_succ, pg_val=pg_val, dropout_r=dr
    )
  }
  
  get_section_order <- function() c("results","n_box","interp","ci_compare",
                                    "definitions","calc_code","plot_p1","table_p1")
  
  capture_plots <- function() {
    pc <- get_plot_colour()
    list(p1 = tryCatch({
      df  <- prop_df_sens()
      lbl <- if (isTRUE(input$endpoint=="safety")) "Expected device event rate"
      else "Expected device success rate"
      p <- ggplot(df, aes(x=x, y=y)) +
        geom_line(colour=pc, linewidth=1.1) + geom_point(colour=pc, size=2) +
        labs(title="Device rate vs required sample size", x=lbl, y="n") +
        plot_theme_large
      tmp <- tempfile(fileext=".png")
      ggsave(tmp, p, width=6, height=3.5, dpi=150, bg="white")
      tmp
    }, error=function(e) NULL))
  }
  
  build_interp_text <- function(rd) {
    tpl <- if (!is.null(input$rpt_interp_text) && nchar(trimws(input$rpt_interp_text)) > 0)
      input$rpt_interp_text
    else paste0(
      "A total of {n} evaluable patients are required to demonstrate, with ",
      "{power_pct}% power, that the device rate meets the performance goal of {pg_pct}%, ",
      "assuming a true device rate of {pd_pct}%. ",
      "Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. ",
      "The study will be deemed successful if at least {n_successes} out of {n} ",
      "evaluable patients achieve the primary endpoint.")
    pg_pct  <- round(as.numeric(input$p0.expected)*100)
    pd_pct  <- round(as.numeric(input$p1.expected)*100)
    pwr_pct <- round(input$power*100)
    txt <- tpl
    txt <- gsub("{n}",           rd$n_fmt,             txt, fixed=TRUE)
    txt <- gsub("{n_dropout}",   rd$nd_fmt,            txt, fixed=TRUE)
    txt <- gsub("{n_successes}", rd$ns_fmt,            txt, fixed=TRUE)
    txt <- gsub("{power_pct}",   as.character(pwr_pct),txt, fixed=TRUE)
    txt <- gsub("{pg_pct}",      as.character(pg_pct), txt, fixed=TRUE)
    txt <- gsub("{pd_pct}",      as.character(pd_pct), txt, fixed=TRUE)
    txt <- gsub("{p0_pct}",      as.character(pg_pct), txt, fixed=TRUE)
    txt <- gsub("{p1_pct}",      as.character(pd_pct), txt, fixed=TRUE)
    txt <- gsub("{alpha}",       as.character(input$sig.level), txt, fixed=TRUE)
    txt <- gsub("{ci_method}",   rd$ci_label,          txt, fixed=TRUE)
    txt <- gsub("{dropout_pct}", as.character(rd$dropout_r), txt, fixed=TRUE)
    txt
  }
  
  make_sens_table_html <- function(df, col_names, caption, blue, th_fn, td_fn) {
    df2 <- df
    df2[[2]] <- ifelse(is.infinite(df2[[2]])|is.na(df2[[2]]), "\u2014",
                       format(round(df2[[2]]), big.mark=","))
    paste0(
      "<p style='font-size:9px;color:#555;font-style:italic;margin:4px 0 6px;'>",caption,"</p>",
      "<table style='border-collapse:collapse;width:100%;'>",
      "<tr>", paste(sapply(col_names, th_fn), collapse=""), "</tr>",
      paste(apply(df2, 1, function(r) paste0("<tr>",paste(sapply(r,td_fn),collapse=""),"</tr>")), collapse=""),
      "</table>"
    )
  }
  
  # ── PDF download ──────────────────────────────────────────────────────────────
  output$downloadPDF <- downloadHandler(
    filename    = function() paste0("PGPower_", format(Sys.Date(),"%d_%b_%Y"), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      tryCatch({
        rd   <- build_report_data()
        n_val <- rd$n_val; ci_label <- rd$ci_label
        n_fmt <- rd$n_fmt; ns_fmt <- rd$ns_fmt; nd_fmt <- rd$nd_fmt
        
        show_results <- !isTRUE(input$rpt_results   == FALSE)
        show_interp  <- !isTRUE(input$rpt_interp_inc== FALSE) && !is.infinite(n_val) && !is.na(rd$n_successes)
        show_ci_cmp  <- !isTRUE(input$rpt_ci_compare== FALSE)
        show_defs    <- !isTRUE(input$rpt_definitions==FALSE)
        show_code    <- !isTRUE(input$rpt_calc_code  ==FALSE)
        show_n_box   <- !isTRUE(input$rpt_n_box      ==FALSE)
        show_plot_p1 <- isTRUE(input$rpt_plot_p1)
        show_table_p1<- isTRUE(input$rpt_table_p1)
        
        plot_files <- if (show_plot_p1) capture_plots() else list(p1=NULL)
        blue <- "#2E74B5"
        td <- function(v) paste0("<td style='padding:5px 9px;border:1px solid #dde3ea;font-size:9.5px;'>",v,"</td>")
        th <- function(v) paste0("<th style='padding:5px 9px;background:#eef3f8;border:1px solid #dde3ea;text-align:left;font-size:9.5px;'>",v,"</th>")
        h2s <- paste0("color:",blue,";font-size:13px;margin:20px 0 6px;")
        hr  <- paste0("<hr style='border-color:",blue,";margin:14px 0;'>")
        is_sfty <- isTRUE(input$endpoint=="safety")
        ep_txt <- if (is_sfty) "Lower is better" else "Higher is better"
        
        results_html <- if (show_results) paste0(
          "<h2 style='",h2s,"'>Results</h2>",
          "<p style='font-size:9.5px;color:#444;'>",
          if (is_sfty) "H\u2080: device rate \u2265 PG &nbsp; H\u2081: device rate &lt; PG"
          else         "H\u2080: device rate \u2264 PG &nbsp; H\u2081: device rate &gt; PG",
          "</p>",
          "<table style='border-collapse:collapse;width:100%;'>",
          "<tr>",th("Power"),th("n"),th("n-events"),
          th(paste0(rd$dropout_r,"% dropout")),
          th("PG"),th("Device rate"),th("\u03b1"),th("Method"),"</tr>",
          "<tr>",td(format(input$power,nsmall=2)),td(n_fmt),td(ns_fmt),td(nd_fmt),
          td(input$p0.expected),td(input$p1.expected),
          td(input$sig.level),td(ci_label),"</tr></table>",hr
        ) else ""
        
        n_box_html <- if (show_n_box) {
          dr_r <- get_dropout_rate(); av <- as.numeric(input$sig.level)
          pg_v <- as.numeric(pg_d()); z_nb <- qnorm(1-av)
          n_sn <- if (is.infinite(n_val)) NA_integer_
          else if (is_sfty) floor(n_val*pg_v  - z_nb*sqrt(n_val*pg_v*(1-pg_v)))
          else              ceiling(n_val*pg_v + z_nb*sqrt(n_val*pg_v*(1-pg_v)))
          nd_n <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr_r/100))
          rows <- list(
            c("Endpoint direction", ep_txt),
            c("Performance goal (PG)", as.character(pg_d())),
            c("Expected device rate",  as.character(pd_d())),
            c("\u03b1 / CI equivalent",paste0(av," / ",round((1-2*av)*100,1),"% CI")),
            c("Power",                 paste0(round(power_d()*100),"%")),
            c("Method",                ci_label),
            c("Required n",            n_fmt),
            c("Events threshold",      if(is.na(n_sn)) "—" else format(n_sn, big.mark=",")),
            c(paste0("Enrolment (",dr_r,"% dropout)"),
              if(is.na(nd_n)) "—" else format(nd_n, big.mark=","))
          )
          paste0("<h2 style='",h2s,"'>Sample Size Summary</h2>",
                 "<table style='border-collapse:collapse;width:100%;'>",
                 "<tr>",th("Parameter"),th("Value"),"</tr>",
                 paste(sapply(rows, function(r) paste0("<tr>",td(r[1]),td(r[2]),"</tr>")), collapse=""),
                 "</table>",hr)
        } else ""
        
        interp_html <- if (show_interp) paste0(
          "<h2 style='",h2s,"'>Interpretation</h2>",
          "<p style='font-size:9.5px;line-height:1.7;'>",build_interp_text(rd),"</p>",hr
        ) else ""
        
        all_m <- c("TrialSize(Z)"="z_power","Wilson"="wilson","Exact"="exact",
                   "AC"="ac","Wald"="asymptotic","prop.test"="prop.test",
                   "Bayes"="bayes","Logit"="logit","Cloglog"="cloglog","Probit"="probit")
        m_ns <- sapply(all_m, function(m) {
          n <- prop_total_n(input$p0.expected, input$p1.expected,
                            ci_method=m, sim_n=400, seed=1)
          if (is.infinite(n)) "\u2014" else format(n, big.mark=",")
        })
        ci_html <- if (show_ci_cmp) paste0(
          "<h2 style='",h2s,"'>Sample size by method</h2>",
          "<p style='font-size:9px;color:#555;'>PG = ",input$p0.expected,
          " | device rate = ",input$p1.expected," | \u03b1 = ",input$sig.level,
          " | power = ",input$power,"</p>",
          "<table style='border-collapse:collapse;width:100%;'>",
          "<tr>",paste(sapply(names(all_m),th),collapse=""),"</tr>",
          "<tr>",paste(sapply(unname(m_ns),td),collapse=""),"</tr>",
          "</table>",hr
        ) else ""
        
        defs_list <- list(
          c("Power",             "Probability of correctly rejecting H\u2080."),
          c("n",                 "Minimum evaluable patients required."),
          c("Events threshold",  "Min/max events needed to meet the decision rule."),
          c("PG (Performance Goal)", "Pre-specified benchmark rate from literature or prior data."),
          c("Device rate",       "Expected true event/success rate of the device."),
          c("\u03b1",            "Probability of a false-positive result (one-sided)."),
          c("CI Method",         "Method used to construct the confidence interval.")
        )
        defs_html <- if (show_defs) paste0(
          "<h2 style='",h2s,"'>Definitions</h2>",
          "<table style='border-collapse:collapse;width:100%;'>",
          paste(sapply(defs_list, function(d) paste0(
            "<tr><td style='padding:4px 9px;font-weight:600;border:1px solid #dde3ea;font-size:9px;white-space:nowrap;'>",d[1],"</td>",
            "<td style='padding:4px 9px;border:1px solid #dde3ea;font-size:9px;'>",d[2],"</td></tr>"
          )), collapse=""),
          "</table>",hr
        ) else ""
        
        code_html <- if (show_code) paste0(
          "<h2 style='",h2s,"'>Calculation</h2>",
          "<pre style='background:#f1f5f9;border-radius:6px;padding:12px;",
          "font-family:\"Courier New\",monospace;font-size:8.5px;color:#1a1a2e;line-height:1.75;'>",
          build_calc_code_txt(),"</pre>"
        ) else ""
        
        img_tag <- function(f, cap) {
          if (is.null(f)||!file.exists(f)) return("")
          b64 <- base64enc::base64encode(f)
          paste0("<figure style='margin:12px 0;'><img src='data:image/png;base64,",b64,
                 "' style='width:100%;max-width:600px;'>",
                 "<figcaption style='font-size:9px;color:#555;margin-top:4px;'>",cap,"</figcaption></figure>")
        }
        plot_p1_html <- if (show_plot_p1) paste0(
          "<h2 style='",h2s,"'>Device Rate Sensitivity Plot</h2>",
          img_tag(plot_files$p1, "Device rate vs required sample size"),hr
        ) else ""
        
        df_p1_pdf <- if (show_table_p1) tryCatch(prop_df_sens(), error=function(e) NULL) else NULL
        table_p1_html <- if (show_table_p1 && !is.null(df_p1_pdf)) paste0(
          "<h2 style='",h2s,"'>Device Rate Sensitivity Table</h2>",
          make_sens_table_html(df_p1_pdf,
                               col_names=c("Device Rate","Required N"),
                               caption=paste0("PG = ",input$p0.expected),
                               blue=blue, th_fn=th, td_fn=td),hr
        ) else ""
        
        section_map <- list(results=results_html, n_box=n_box_html, interp=interp_html,
                            ci_compare=ci_html, definitions=defs_html, calc_code=code_html,
                            plot_p1=plot_p1_html, table_p1=table_p1_html)
        body_html <- paste(sapply(get_section_order(), function(s) section_map[[s]] %||% ""), collapse="")
        
        rpt_title <- if (!is.null(input$rpt_title) && nchar(trimws(input$rpt_title))>0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        sub_parts <- c(
          if (isTRUE(input$rpt_include_author) && !is.null(input$rpt_author_name) &&
              nchar(trimws(input$rpt_author_name))>0) paste0("Author: ",trimws(input$rpt_author_name)),
          if (!isTRUE(input$rpt_include_date  ==FALSE)) format(Sys.Date(),"%d %B %Y"),
          if (!isTRUE(input$rpt_include_method==FALSE)) paste0("Method: ",ci_label)
        )
        sub_line <- if (length(sub_parts)>0)
          paste0("<p class='sub'>",paste(sub_parts,collapse=" &nbsp;|&nbsp; "),"</p>") else ""
        
        html_out <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
          "<style>body{font-family:'Helvetica Neue',Arial,sans-serif;margin:40px;",
          "color:#1a2e35;font-size:10px;line-height:1.5;}",
          "h1{color:",blue,";font-size:17px;margin:0 0 4px;}",
          ".sub{font-size:9px;color:#666;margin:0 0 10px;}",
          "hr.top{border:none;border-top:2px solid ",blue,";margin:10px 0 16px;}",
          "@page{size:A4;margin:18mm 18mm 22mm 18mm;}</style></head><body>",
          "<h1>",rpt_title,"</h1>",sub_line,
          "<hr class='top'>",body_html,
          "<p style='margin-top:32px;font-size:8.5px;color:#94a3b8;font-style:italic;",
          "border-top:1px solid #e2e8f0;padding-top:10px;'>",
          "Generated by PG-Power.</p></body></html>"
        )
        tmp_html <- tempfile(fileext=".html")
        writeLines(html_out, con=tmp_html, useBytes=FALSE)
        if (requireNamespace("pagedown", quietly=TRUE)) {
          pagedown::chrome_print(tmp_html, output=file, wait=15); unlink(tmp_html)
        } else if (requireNamespace("webshot2", quietly=TRUE)) {
          webshot2::webshot(tmp_html, file=file, vwidth=794, vheight=1123); unlink(tmp_html)
        } else {
          file.copy(tmp_html, file, overwrite=TRUE); unlink(tmp_html)
          showNotification("PDF engine not found. Downloaded as HTML.", type="warning", duration=12)
        }
      }, error=function(e) message("PDF ERROR: ", conditionMessage(e)))
    }
  )
  
  # ── Word download ─────────────────────────────────────────────────────────────
  output$downloadWord <- downloadHandler(
    filename    = function() paste0("PGPower_", format(Sys.Date(),"%d_%b_%Y"), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      tryCatch({
        if (!requireNamespace("officer", quietly=TRUE))
          stop("Please install 'officer': install.packages('officer')")
        rd    <- build_report_data()
        n_val <- rd$n_val; ci_label <- rd$ci_label
        n_fmt <- rd$n_fmt; ns_fmt <- rd$ns_fmt; nd_fmt <- rd$nd_fmt
        show_plot_p1_w  <- isTRUE(input$rpt_plot_p1)
        show_table_p1_w <- isTRUE(input$rpt_table_p1)
        plot_files_w    <- if (show_plot_p1_w) capture_plots() else list(p1=NULL)
        is_sfty <- isTRUE(input$endpoint=="safety")
        ep_txt  <- if (is_sfty) "Lower is better" else "Higher is better"
        
        blue_col  <- "#2E74B5"
        title_fmt <- officer::fp_text(bold=TRUE,  font.size=12, color=blue_col)
        h2_fmt    <- officer::fp_text(bold=FALSE, font.size=10, color=blue_col)
        sub_fmt   <- officer::fp_text(font.size=8,  color="#444444")
        body_fmt  <- officer::fp_text(font.size=9)
        mono_fmt  <- officer::fp_text(font.family="Courier New", font.size=8, color="#1a1a2e")
        def_term  <- officer::fp_text(bold=TRUE,  font.size=9)
        def_body  <- officer::fp_text(bold=FALSE, font.size=9)
        hyp_fmt   <- officer::fp_text(font.size=9, color="#444444")
        tight_p   <- officer::fp_par(text.align="left")
        border_p  <- officer::fp_par(text.align="left",
                                     border.bottom=officer::fp_border(color=blue_col, width=1))
        
        doc <- officer::read_docx()
        rpt_title <- if (!is.null(input$rpt_title) && nchar(trimws(input$rpt_title))>0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        sub_parts <- c(
          if (isTRUE(input$rpt_include_author)&&!is.null(input$rpt_author_name)&&
              nchar(trimws(input$rpt_author_name))>0) paste0("Author: ",trimws(input$rpt_author_name)),
          if (!isTRUE(input$rpt_include_date  ==FALSE)) format(Sys.Date(),"%d %B %Y"),
          if (!isTRUE(input$rpt_include_method==FALSE)) paste0("Method: ",ci_label)
        )
        doc <- officer::body_add_fpar(doc,
                                      officer::fpar(officer::ftext(rpt_title, title_fmt), fp_p=tight_p))
        if (length(sub_parts)>0) doc <- officer::body_add_fpar(doc,
                                                               officer::fpar(officer::ftext(paste(sub_parts,collapse="  |  "), sub_fmt), fp_p=tight_p))
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
        doc <- officer::body_add_par(doc, "", style="Normal")
        
        if (!isTRUE(input$rpt_results==FALSE)) {
          summary_df <- data.frame(
            Power=format(input$power,nsmall=2), n=n_fmt,
            `Events threshold`=ns_fmt,
            Enrolment=nd_fmt,
            PG=format(input$p0.expected,nsmall=2),
            `Device rate`=format(input$p1.expected,nsmall=2),
            Alpha=format(as.numeric(input$sig.level)),
            Method=ci_label, check.names=FALSE, stringsAsFactors=FALSE)
          colnames(summary_df)[4] <- paste0(rd$dropout_r,"% dropout")
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Results", h2_fmt), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext(
                                          if(is_sfty) "H\u2080: device rate \u2265 PG  vs.  H\u2081: device rate < PG"
                                          else        "H\u2080: device rate \u2264 PG  vs.  H\u2081: device rate > PG",
                                          hyp_fmt), fp_p=tight_p))
          doc <- officer::body_add_table(doc, summary_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_n_box==FALSE)) {
          dr_w <- get_dropout_rate(); av_w <- as.numeric(input$sig.level)
          pg_w <- as.numeric(pg_d()); z_w <- qnorm(1-av_w)
          n_sn_w <- if (is.infinite(n_val)) NA_integer_
          else if (is_sfty) floor(n_val*pg_w  - z_w*sqrt(n_val*pg_w*(1-pg_w)))
          else              ceiling(n_val*pg_w + z_w*sqrt(n_val*pg_w*(1-pg_w)))
          nd_w   <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr_w/100))
          nb_df  <- data.frame(
            Parameter=c("Endpoint direction","Performance goal (PG)","Expected device rate",
                        "\u03b1 / CI equivalent","Power","Method","Required n",
                        "Events threshold",paste0("Enrolment (",dr_w,"% dropout)")),
            Value=c(ep_txt,as.character(pg_d()),as.character(pd_d()),
                    paste0(av_w," / ",round((1-2*av_w)*100,1),"% CI"),
                    paste0(round(power_d()*100),"%"), ci_label, n_fmt,
                    if(is.na(n_sn_w)) "—" else format(n_sn_w,big.mark=","),
                    if(is.na(nd_w)) "—" else format(nd_w,big.mark=",")),
            stringsAsFactors=FALSE)
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Sample Size Summary", h2_fmt), fp_p=tight_p))
          doc <- officer::body_add_table(doc, nb_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!is.infinite(n_val) && !is.na(rd$n_successes) &&
            !isTRUE(input$rpt_interp_inc==FALSE)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Interpretation", h2_fmt), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext(build_interp_text(rd), body_fmt), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_ci_compare==FALSE)) {
          all_m <- c("TrialSize(Z)"="z_power","Wilson"="wilson","Exact"="exact",
                     "AC"="ac","Wald"="asymptotic","prop.test"="prop.test",
                     "Bayes"="bayes","Logit"="logit","Cloglog"="cloglog","Probit"="probit")
          m_ns  <- sapply(all_m, function(m) {
            n <- prop_total_n(input$p0.expected,input$p1.expected,ci_method=m,sim_n=400,seed=1)
            if (is.infinite(n)) "\u2014" else format(n,big.mark=",")
          })
          ci_df <- as.data.frame(matrix(unname(m_ns),nrow=1), stringsAsFactors=FALSE)
          colnames(ci_df) <- names(all_m)
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Sample size by method", h2_fmt), fp_p=tight_p))
          doc <- officer::body_add_table(doc, ci_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_definitions==FALSE)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Definitions", h2_fmt), fp_p=tight_p))
          defs <- list(
            list("Power: ",           "Probability of correctly rejecting H\u2080."),
            list("n: ",               "Minimum evaluable patients required."),
            list("Events threshold: ","Min/max events required to meet the decision rule."),
            list("PG: ",              "Performance goal \u2014 pre-specified benchmark rate."),
            list("Device rate: ",     "Anticipated true event/success rate of the device."),
            list("\u03b1: ",          "Probability of a false-positive result (one-sided)."),
            list("CI Method: ",       "Method used to construct the confidence interval.")
          )
          for (d in defs) doc <- officer::body_add_fpar(doc,
                                                        officer::fpar(officer::ftext(paste0("\u2022  ",d[[1]]),def_term),
                                                                      officer::ftext(d[[2]],def_body), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_calc_code==FALSE)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Calculation", h2_fmt), fp_p=tight_p))
          for (line in strsplit(build_calc_code_txt(),"\n")[[1]])
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(line,mono_fmt), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (show_plot_p1_w && !is.null(plot_files_w$p1) && file.exists(plot_files_w$p1)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Device Rate Sensitivity Plot", h2_fmt), fp_p=tight_p))
          doc <- officer::body_add_img(doc, src=plot_files_w$p1, width=5.5, height=3.2)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        if (show_table_p1_w) {
          df_w <- tryCatch(prop_df_sens(), error=function(e) NULL)
          if (!is.null(df_w)) {
            df_w[[2]] <- ifelse(is.infinite(df_w[[2]])|is.na(df_w[[2]]),"\u2014",
                                format(round(df_w[[2]]),big.mark=","))
            df_w[[1]] <- sprintf("%.3f",df_w[[1]])
            colnames(df_w) <- c("Device Rate","Required N")
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext("Device Rate Sensitivity Table", h2_fmt), fp_p=tight_p))
            doc <- officer::body_add_table(doc, df_w, align_table="left")
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
            doc <- officer::body_add_par(doc, "", style="Normal")
          }
        }
        
        footer_fmt <- officer::fp_text(font.size=8, color="#94a3b8", italic=TRUE)
        page_fmt   <- officer::fp_text(font.size=8, color="#94a3b8")
        doc <- officer::body_add_par(doc, "", style="Normal")
        doc <- officer::body_add_fpar(doc, officer::fpar(
          officer::ftext("Generated by PG-Power.   ", footer_fmt),
          officer::run_word_field(field="PAGE",     prop=page_fmt),
          officer::ftext(" / ", page_fmt),
          officer::run_word_field(field="NUMPAGES", prop=page_fmt),
          fp_p=officer::fp_par(text.align="left",
                               border.top=officer::fp_border(color="#e2e8f0", width=1))
        ))
        print(doc, target=file)
      }, error=function(e) message("Word ERROR: ", conditionMessage(e)))
    }
  )
  
  # ── Download handlers (data/plots) ────────────────────────────────────────────
  output$downloadData_plot2 <- downloadHandler(
    filename = function() paste0("PGPower_sensitivity_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- prop_df_sens()
      colnames(df) <- c("Device Rate", "Required N")
      write.csv(df, file, row.names=FALSE)
    }
  )
  output$downloadPlot2 <- downloadHandler(
    filename    = function() paste0("PGPower_sensitivity_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      pc  <- get_plot_colour()
      df  <- prop_df_sens()
      lbl <- if (isTRUE(input$endpoint=="safety")) "Expected device event rate"
      else "Expected device success rate"
      p <- ggplot(df, aes(x=x,y=y)) +
        geom_line(colour=pc, linewidth=1.1) + geom_point(colour=pc, size=2) +
        labs(title="Device rate vs required sample size", x=lbl, y="Required n") +
        plot_theme_large
      ggsave(file, p, width=7, height=4, dpi=150, bg="white")
    }
  )
  
}