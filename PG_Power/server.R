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
    alpha  <- as.numeric(input$sig.level)
    pwr    <- input$power
    pg     <- if (isTRUE(input$prop_precision == "3dp")) round(input$p0.manual %||% input$p0.expected, 3) else input$p0.expected
    pd     <- if (isTRUE(input$prop_precision == "3dp")) round(input$p1.manual %||% input$p1.expected, 3) else input$p1.expected
    ci_lbl_map <- c(asymptotic="asymptotic", wilson="wilson", ac="ac",
                    exact="exact", prop.test="prop.test", bayes="bayes")
    cm <- if (is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
    conf_lev <- round(1 - 2 * alpha, 3)
    
    mirror_note <- if (is_safety) c(
      "# Safety direction: mirror so test becomes 'higher is better'",
      paste0("PG_m    <- 1 - ", pg, "   # = ", round(1-pg,3)),
      paste0("p_dev_m <- 1 - ", pd, "   # = ", round(1-pd,3)),
      "# Then apply the efficacy formula with PG_m, p_dev_m",
      "") else character(0)
    
    paste(c(
      paste0("# Single-arm performance goal  —  binom CI simulation"),
      paste0("# Endpoint: ", if (is_safety) "lower is better (safety)" else "higher is better (efficacy)"),
      paste0("# Decision rule: CI ", if (is_safety) "upper < PG" else "lower > PG"),
      "",
      "library(binom)",
      paste0("PG    <- ", pg,    "   # performance goal"),
      paste0("p_dev <- ", pd,    "   # expected performance"),
      paste0("alpha <- ", alpha, "   # one-sided alpha"),
      paste0("power <- ", pwr,   "   # desired power"),
      paste0("conf_level <- ", conf_lev, "   # = 1 - 2*alpha"),
      "",
      mirror_note,
      paste0('ci_method <- "', cm, '"'),
      "",
      "# Binary search: smallest n where CI bound clears the performance goal",
      "# (uses binom::binom.confint internally via prop_ci_vec)",
      "# n <- total_sample_size_prop_ci_power_1arm(",
      paste0("#   p0=", if(is_safety) paste0("1-PG=",round(1-pg,3)) else paste0("PG=",pg),
             ", p1=", if(is_safety) paste0("1-p_dev=",round(1-pd,3)) else paste0("p_dev=",pd), ","),
      paste0("#   delta=0, alpha=", alpha, ", power=", pwr, ", ci_method=ci_method)")
    ), collapse = "\n")
  }
  
  # ── Endpoint presets ─────────────────────────────────────────────────────────
  endpoint_defaults <- list(
    efficacy = list(pg = 0.88, pd = 0.93),
    safety   = list(pg = 0.11, pd = 0.05)
  )
  observeEvent(input$endpoint, {
    d <- endpoint_defaults[[input$endpoint]]
    if (is.null(d)) return()
    updateSliderInput(session,  "p0.expected", value = d$pg)
    updateNumericInput(session, "p0.manual",   value = d$pg)
    updateSliderInput(session,  "p1.expected", value = d$pd)
    updateNumericInput(session, "p1.manual",   value = d$pd)
  }, ignoreInit = TRUE)
  
  # ── Keep manual boxes in sync with sliders ──────────────────────────────────
  observeEvent(input$p0.expected, {
    if (!isTRUE(is_3dp()))
      updateNumericInput(session, "p0.manual", value = round(input$p0.expected, 3))
  }, ignoreInit = TRUE)
  observeEvent(input$p1.expected, {
    if (!isTRUE(is_3dp()))
      updateNumericInput(session, "p1.manual", value = round(input$p1.expected, 3))
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
  prop_total_n <- function(pg, pd,
                           ci_method = input$ci_method_prop,
                           sim_n     = {
                             v <- as.numeric(input$sim_quality)
                             if (is.null(v) || is.na(v)) 1000L else as.integer(v)
                           },
                           seed      = {
                             v <- as.numeric(input$sim_seed)
                             if (is.null(v) || is.na(v)) 1L else as.integer(v)
                           }) {
    alpha     <- as.numeric(input$sig.level)
    power     <- as.numeric(input$power)
    is_safety <- isTRUE(input$endpoint == "safety")
    cm        <- if (is.null(ci_method) || !nzchar(ci_method)) "asymptotic" else ci_method
    
    if (is_safety)
      total_sample_size_prop_ci_power_1arm(1-pg, 1-pd, 0, alpha, power,
                                           ci_method=cm, nsim=sim_n, seed=seed)
    else
      total_sample_size_prop_ci_power_1arm(pg, pd, 0, alpha, power,
                                           ci_method=cm, nsim=sim_n, seed=seed)
  }
  
  # ── Debounced inputs ─────────────────────────────────────────────────────────
  is_3dp  <- reactive({ isTRUE(input$prop_precision == "3dp") })
  pg_d    <- debounce(reactive({
    if (is_3dp()) {
      v <- input$p0.manual
      if (is.null(v) || is.na(v)) input$p0.expected else round(v, 3)
    } else input$p0.expected
  }), 800)
  pd_d    <- debounce(reactive({
    if (is_3dp()) {
      v <- input$p1.manual
      if (is.null(v) || is.na(v)) input$p1.expected else round(v, 3)
    } else input$p1.expected
  }), 800)
  alpha_d <- debounce(reactive(input$sig.level),   600)
  power_d <- debounce(reactive(input$power),       600)
  
  # ── Core reactives ────────────────────────────────────────────────────────────
  prop_n <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    prop_total_n(pg_d(), pd_d())
  })
  
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
  
  prop_df_power <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    alpha   <- as.numeric(alpha_d())
    target  <- as.numeric(power_d())
    is_sfty <- isTRUE(input$endpoint == "safety")
    pg <- if (is_sfty) 1 - as.numeric(pg_d()) else as.numeric(pg_d())
    pd <- if (is_sfty) 1 - as.numeric(pd_d()) else as.numeric(pd_d())
    
    if (is.na(pg) || is.na(pd) || pg <= 0 || pg >= 1 || pd <= 0 || pd >= 1)
      return(data.frame(x=numeric(0), y=numeric(0), x_crit=integer(0)))
    if (pd <= pg)
      return(data.frame(x=numeric(0), y=numeric(0), x_crit=integer(0)))
    
    z_a      <- qnorm(1 - alpha)
    z_b      <- qnorm(target)
    n_approx <- ceiling((z_a*sqrt(pg*(1-pg)) + z_b*sqrt(pd*(1-pd)))^2 / (pd-pg)^2)
    if (is.na(n_approx) || n_approx <= 0 || is.infinite(n_approx)) n_approx <- 200L
    
    rng   <- if (is.null(input$power_plot_range) || is.na(input$power_plot_range))
      50L else as.integer(input$power_plot_range)
    n_lo  <- max(2L, n_approx - rng)
    n_hi  <- min(n_approx + rng, 5000L)
    n_seq <- seq(n_lo, n_hi, by = 1L)
    
    res <- t(sapply(n_seq, function(n) {
      xc <- qbinom(1 - alpha, n, pg) + 1L
      pw <- if (xc > n) NA_real_ else 1 - pbinom(xc - 1L, n, pd)
      c(power=pw, x_crit=xc)
    }))
    data.frame(x=n_seq, y=res[,"power"], x_crit=as.integer(res[,"x_crit"]))
  })
  
  # ── CI comparison table ───────────────────────────────────────────────────────
  all_compare_methods <- c(
    "Wald (Z-score)"    = "asymptotic",
    "Wilson Score"      = "wilson",
    "Agresti-Coull"     = "ac",
    "Clopper-Pearson"   = "exact",
    "Prop.test"         = "prop.test",
    "Jeffreys"          = "bayes"
  )
  
  compare_df <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
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
    ci_lbl_map <- c(asymptotic="Wald (Z-score)", wilson="Wilson Score",
                    ac="Agresti-Coull", exact="Clopper-Pearson",
                    prop.test="Prop.test", bayes="Jeffreys")
    cm_cur <- if (is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
    method_label <- paste0("CI simulation (", 
                           unname(ci_lbl_map[cm_cur] %||% cm_cur), ")")
    
    if (is.infinite(n_out)) return(box_ui(
      "Required sample size",
      paste0("Not achievable (N = \u221e) — check performance goal and device proportion.  [", method_label, "]")
    ))
    
    n_enrol  <- ceiling(n_out / (1 - dropout_r / 100))
    pg_val   <- as.numeric(pg_d())
    z_a      <- qnorm(1 - alpha_val)
    n_events <- if (is_safety)
      floor(n_out * pg_val   - z_a * sqrt(n_out * pg_val   * (1 - pg_val)))
    else
      ceiling(n_out * pg_val + z_a * sqrt(n_out * pg_val * (1 - pg_val)))
    
    if (!is_safety) {
      h0_txt <- paste0("H\u2080: device proportion \u2264 ", pg_val, " (performance goal)")
      h1_txt <- paste0("H\u2081: device proportion > ", pg_val, " (device meets performance goal)")
    } else {
      h0_txt <- paste0("H\u2080: device proportion \u2265 ", pg_val, " (at or above max acceptable proportion)")
      h1_txt <- paste0("H\u2081: device proportion < ", pg_val, " (device meets performance goal)")
    }
    reject_rule <- if (!is_safety)
      tagList("Reject H\u2080 if ", tags$b(paste0("\u2265 ", n_events)),
              " of ", tags$b(format(n_out, big.mark=",")),
              " patients succeed  (CI lower > ", tags$b(pg_val), ")")
    else
      tagList("Reject H\u2080 if ", tags$b(paste0("\u2264 ", n_events)),
              " of ", tags$b(format(n_out, big.mark=",")),
              " patients experience the event  (CI upper < ", tags$b(pg_val), ")")
    
    actual_pw <- tryCatch({
      df_pw <- prop_df_power()
      if (nrow(df_pw) > 0) {
        df_c <- df_pw[!is.na(df_pw$y), ]
        if (nrow(df_c) > 0) {
          idx <- which.min(abs(df_c$x - n_out))
          if (length(idx) > 0) df_c$y[idx] else NA_real_
        } else NA_real_
      } else NA_real_
    }, error = function(e) NA_real_)
    actual_pw_txt <- if (!is.na(actual_pw))
      paste0(round(actual_pw * 100, 1), "%") else paste0(round(power_d()*100), "%")
    
    tagList(
      box_ui("Required sample size", tagList(
        tags$b(format(n_out, big.mark=",")), " patients",
        tags$span(style="color:#64748b; font-size:11px;",
                  paste0("  [", method_label, "]"))
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
                        tags$span(class="nb-label", "Target power:"),
                        tags$span(class="nb-val",   paste0(round(power_d()*100), "%"))),
               tags$div(class="nb-row",
                        tags$span(class="nb-label", "Actual power at n:"),
                        tags$span(class="nb-val",   tags$b(actual_pw_txt))),
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
    df      <- prop_df_power()
    pc      <- get_plot_colour()
    n_req   <- tryCatch(prop_n(), error = function(e) NA)
    is_sfty <- isTRUE(input$endpoint == "safety")
    
    validate(need(nrow(df) > 0 && !all(is.na(df$y)),
                  "Device proportion must be more favourable than the performance goal to compute a power curve."))
    
    df_c   <- df[!is.na(df$y), ]
    target <- as.numeric(power_d())
    col_target <- "#475569"
    col_n      <- "#16a34a"
    
    n_at <- NA_real_; pw_at <- NA_real_; xc_at <- NA_integer_
    if (!is.na(n_req) && !is.infinite(n_req) && nrow(df_c) > 0) {
      idx   <- which.min(abs(df_c$x - n_req))
      n_at  <- df_c$x[idx]
      pw_at <- df_c$y[idx]
      xc_at <- df_c$x_crit[idx]
    }
    
    dir_sym  <- if (is_sfty) "\u2264" else "\u2265"
    pct_vals <- round(df_c$y * 100, 1)
    y_lo     <- max(0, min(pct_vals, na.rm=TRUE) - 4)
    
    fig <- plot_ly() %>%
      add_trace(
        x=df_c$x, y=pct_vals, type="scatter", mode="lines",
        line=list(color=pc, width=2),
        hovertemplate="n: %{x}<br>Power: %{y}%<extra></extra>",
        name="Power curve", showlegend=FALSE
      ) %>%
      layout(
        title=list(text="Sample size (n) vs Achieved power (%)",
                   font=list(size=17, color="#1a2e35", family="DM Sans, sans-serif"),
                   x=0, xanchor="left"),
        xaxis=list(title="n", gridcolor="#edf0f4", zeroline=FALSE, showline=FALSE),
        yaxis=list(title="%", range=list(y_lo, 100), gridcolor="#edf0f4",
                   zeroline=FALSE, ticksuffix=""),
        hovermode="x unified",
        paper_bgcolor="transparent", plot_bgcolor="white",
        font=list(family="DM Sans, sans-serif"),
        margin=list(t=60, r=20, b=50, l=50),
        shapes=list(list(
          type="line", x0=min(df_c$x), x1=max(df_c$x),
          y0=target*100, y1=target*100,
          line=list(color=col_target, width=1.5, dash="dash")
        )),
        annotations=c(
          list(list(x=max(df_c$x), y=target*100+2.5,
                    text=paste0("Target: ", round(target*100), "%"),
                    showarrow=FALSE, xanchor="right",
                    font=list(color=col_target, size=11),
                    bgcolor="rgba(255,255,255,0)")),
          if (!is.na(n_at)) list(list(
            x=n_at, y=round(pw_at*100,1)+5.5,
            text=paste0("<b>", dir_sym, " ", xc_at, " of ", n_at, "</b> must succeed"),
            showarrow=TRUE, arrowhead=2, arrowsize=0.6, arrowcolor=col_n,
            ax=0, ay=-30, xanchor="center",
            font=list(color=col_n, size=10),
            bgcolor="rgba(255,255,255,0.88)", bordercolor=col_n, borderwidth=1
          )) else NULL,
          if (!is.na(pw_at)) list(list(
            x=n_at, y=round(pw_at*100,1)-5.5,
            text=paste0("Actual power at n=", n_at, ": <b>", round(pw_at*100,1), "%</b>"),
            showarrow=FALSE, xanchor="center",
            font=list(color=col_n, size=10),
            bgcolor="rgba(255,255,255,0.88)", bordercolor=col_n, borderwidth=1
          )) else NULL
        )
      )
    
    if (!is.na(n_at))
      fig <- fig %>% add_trace(
        x=n_at, y=round(pw_at*100,1), type="scatter", mode="markers",
        marker=list(color=col_n, size=9, symbol="circle",
                    line=list(color=col_n, width=2)),
        hovertemplate=paste0("n: ", n_at, "<br>Power: ", round(pw_at*100,1), "%<extra></extra>"),
        showlegend=FALSE
      )
    
    fig %>% config(displaylogo=FALSE, displayModeBar=FALSE)
  })
  
  output$plot2 <- renderPlotly({
    df <- prop_df_sens()
    pc <- get_plot_colour()
    validate(need(nrow(df) > 0 && !all(is.infinite(df$y)),
                  "No achievable sample size in this range — check performance goal and device proportion."))
    pd_val   <- pd_d()
    idx      <- which.min(abs(df$x - pd_val))
    chosen_n <- if (length(idx) > 0) df$y[idx] else Inf
    x_lab <- if (isTRUE(input$endpoint == "safety"))
      "Expected performance (event proportion)" else "Expected performance (success proportion)"
    p <- ggplot(df, aes(x=x, y=y)) +
      geom_line(colour=pc, linewidth=1.1) +
      geom_point(colour=pc, size=2) +
      labs(title="Expected performance vs required sample size", x=x_lab, y="Required sample size (n)") +
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
    df <- compare_df()
    if (is.null(df)) return(NULL)
    n_req_cmp <- tryCatch(as.integer(prop_n()), error = function(e) NA_integer_)
    alpha_v   <- as.numeric(alpha_d())
    is_sfty   <- isTRUE(input$endpoint == "safety")
    
    get_power_at_n <- function(n_val) {
      if (is.na(n_val) || !is.finite(n_val)) return(NA_real_)
      pg <- if (is_sfty) 1 - as.numeric(pg_d()) else as.numeric(pg_d())
      pd <- if (is_sfty) 1 - as.numeric(pd_d()) else as.numeric(pd_d())
      xc <- qbinom(1 - alpha_v, n_val, pg) + 1L
      if (xc > n_val) return(NA_real_)
      1 - pbinom(xc - 1L, n_val, pd)
    }
    
    rows_html <- lapply(seq_len(nrow(df)), function(i) {
      n_i    <- df[i, "Total N"]
      sel    <- df[i, "Selected"] == "\u2713"
      n_num  <- suppressWarnings(as.integer(gsub(",","", n_i)))
      passes <- !is.na(n_num) && !is.na(n_req_cmp) && n_num <= n_req_cmp
      bg      <- if (n_i == "Not achievable") "#fef3c7"
      else if (passes)             "#dcfce7"
      else                         "#fff7ed"
      col_txt <- if (n_i == "Not achievable") "#92400e"
      else if (passes)             "#166534"
      else                         "#9a3412"
      pw_val  <- get_power_at_n(n_num)
      pw_txt  <- if (is.na(pw_val)) "\u2014" else paste0(round(pw_val * 100, 1), "%")
      tags$tr(style=paste0("background:", bg, ";"),
              tags$td(style=paste0("padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;",
                                   "font-weight:", if(sel) "700" else "400", ";color:#1a2e35;"),
                      df[i,"Method"]),
              tags$td(style=paste0("padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;",
                                   "text-align:center;font-family:'DM Mono',monospace;color:", col_txt,
                                   ";font-weight:600;"), n_i),
              tags$td(style=paste0("padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;",
                                   "text-align:center;font-family:'DM Mono',monospace;color:", col_txt, ";"),
                      pw_txt)
      )
    })
    
    tagList(
      tags$hr(),
      tags$p(style="font-size:11px;font-weight:700;color:#64748b;margin:0 0 6px;",
             "CI method comparison"),
      tags$p(style="font-size:11px;color:#94a3b8;margin:0 0 8px;",
             paste0("PG = ", pg_d(), "  \u00b7  expected performance = ", pd_d(),
                    "  \u00b7  \u03b1 = ", input$sig.level, "  \u00b7  power = ", input$power)),
      tags$div(style="overflow-x:auto;",
               tags$table(style="border-collapse:collapse;width:100%;",
                          tags$thead(tags$tr(
                            tags$th(style="padding:5px 10px;background:#f1f5f9;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;", "Method"),
                            tags$th(style="padding:5px 10px;background:#f1f5f9;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;text-align:center;", "Required n"),
                            tags$th(style="padding:5px 10px;background:#f1f5f9;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;text-align:center;", "Achieved power (%)")
                          )),
                          tags$tbody(rows_html)
               )
      ),
      tags$p(style="font-size:10.5px;color:#94a3b8;margin:6px 0 0;",
             "\u25a0 Green = meets or beats required n  \u00b7  Amber = larger n  \u00b7  Bold = currently selected method")
    )
  })
  
  output$dataTable2 <- renderDT({
    if (!isTRUE(input$showTable2)) return(NULL)
    df <- prop_df_sens()
    colnames(df) <- c("Device Proportion", "Required n")
    DT::datatable(df, rownames=FALSE, class="stripe hover compact",
                  options=list(pageLength=15))
  })
  
  # ── CI diagram ───────────────────────────────────────────────────────────────
  ci_diagram_data <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    n_val    <- tryCatch(as.integer(prop_n()), error=function(e) NA_integer_)
    if (is.na(n_val) || !is.finite(n_val)) return(NULL)
    is_sfty  <- isTRUE(input$endpoint == "safety")
    alpha_v  <- as.numeric(alpha_d())
    conf_lev <- 1 - 2 * alpha_v
    pg       <- as.numeric(pg_d())
    pd       <- as.numeric(pd_d())
    x_obs    <- as.integer(round(pd * n_val))
    x_obs    <- max(0L, min(x_obs, n_val))
    
    methods <- if (isTRUE(input$showAllCI))
      all_compare_methods
    else {
      cm <- if (is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
      nm <- names(all_compare_methods)[all_compare_methods == cm]
      if (length(nm)==0) nm <- cm
      setNames(cm, nm)
    }
    
    rows <- lapply(names(methods), function(nm) {
      m  <- methods[[nm]]
      ci <- tryCatch(prop_ci_vec(x_obs, n_val, conf_lev, m),
                     error=function(e) list(lower=NA_real_, upper=NA_real_))
      passes <- if (is_sfty) !is.na(ci$upper) && ci$upper < pg
      else          !is.na(ci$lower) && ci$lower > pg
      data.frame(method=nm, lower=ci$lower, upper=ci$upper,
                 est=pd, pg=pg, n=n_val, x=x_obs,
                 is_sfty=is_sfty, passes=passes, stringsAsFactors=FALSE)
    })
    do.call(rbind, rows)
  })
  
  output$plot_ci_diagram <- renderPlotly({
    df <- ci_diagram_data()
    if (is.null(df) || nrow(df)==0) return(NULL)
    pg       <- df$pg[1]
    is_sfty  <- df$is_sfty[1]
    n_val    <- df$n[1]
    x_obs    <- df$x[1]
    est      <- df$est[1]
    pc       <- get_plot_colour()
    col_pg   <- "#475569"
    alpha_v  <- as.numeric(alpha_d())
    conf_pct <- round((1 - 2*alpha_v)*100, 0)
    nr       <- nrow(df)
    df$col   <- ifelse(df$passes, pc, "#94a3b8")
    
    fig <- plot_ly() %>%
      layout(
        title=list(text=paste0(conf_pct, "% confidence intervals  —  n = ",
                               format(n_val, big.mark=","), ",  x = ", x_obs,
                               "  (", round(est*100, 1), "% observed)"),
                   font=list(size=13, color="#1a2e35", family="DM Sans, sans-serif"),
                   x=0, xanchor="left"),
        xaxis=list(title="Proportion", zeroline=FALSE, gridcolor="#edf0f4", tickformat=".3f"),
        yaxis=list(title="", tickvals=seq_len(nr), ticktext=df$method,
                   autorange="reversed", gridcolor="#edf0f4"),
        hovermode="closest",
        paper_bgcolor="transparent", plot_bgcolor="white",
        font=list(family="DM Sans, sans-serif", size=12),
        margin=list(l=140, r=40, t=65, b=50),
        shapes=list(list(type="line", x0=pg, x1=pg, y0=0.4, y1=nr+0.6,
                         line=list(color=col_pg, width=1.5, dash="dash"))),
        annotations=list(list(x=pg, y=0.4, yanchor="bottom", xanchor="center",
                              text=paste0("PG = ", pg), showarrow=FALSE,
                              font=list(color=col_pg, size=10),
                              bgcolor="rgba(255,255,255,0.85)"))
      )
    
    for (i in seq_len(nr)) {
      row   <- df[i, ]
      col_i <- row$col
      lbl   <- paste0(
        "<b>", row$method, "</b><br>",
        conf_pct, "% CI: [", round(row$lower, 4), ", ", round(row$upper, 4), "]<br>",
        "Point estimate: ", round(row$est, 4), "  (x = ", row$x, " / n = ", row$n, ")<br>",
        if (row$passes) "<b style='color:#16a34a;'>\u2713 Passes</b>"
        else            "<b style='color:#dc2626;'>\u2717 Fails</b>",
        " (PG = ", pg, ")"
      )
      fig <- fig %>%
        add_segments(x=row$lower, xend=row$upper, y=i, yend=i,
                     line=list(color=col_i, width=3.5),
                     hoverinfo="text", text=lbl, showlegend=FALSE) %>%
        add_segments(x=row$lower, xend=row$lower, y=i-0.2, yend=i+0.2,
                     line=list(color=col_i, width=2), hoverinfo="none", showlegend=FALSE) %>%
        add_segments(x=row$upper, xend=row$upper, y=i-0.2, yend=i+0.2,
                     line=list(color=col_i, width=2), hoverinfo="none", showlegend=FALSE) %>%
        add_trace(x=row$est, y=i, type="scatter", mode="markers",
                  marker=list(color="#fff", size=7, symbol="circle",
                              line=list(color=col_i, width=2.5)),
                  hoverinfo="none", showlegend=FALSE)
    }
    fig %>% config(displaylogo=FALSE, displayModeBar=FALSE)
  })
  
  # ── Power vs n table ─────────────────────────────────────────────────────────
  power_table_df <- reactive({
    req(pg_d(), pd_d(), power_d(), alpha_d())
    df   <- prop_df_power()
    if (nrow(df) == 0) return(NULL)
    df_c <- df[!is.na(df$y), ]
    if (nrow(df_c) == 0) return(NULL)
    target <- as.numeric(power_d())
    
    ci_n_map <- vapply(names(all_compare_methods), function(nm) {
      m <- all_compare_methods[nm]
      n <- tryCatch(prop_total_n(pg_d(), pd_d(), ci_method=m, sim_n=400L, seed=1L),
                    error=function(e) Inf)
      if (is.finite(n)) as.integer(n) else NA_integer_
    }, integer(1))
    
    ci_labels_at_n <- sapply(df_c$x, function(n_val) {
      hits <- names(ci_n_map)[!is.na(ci_n_map) & ci_n_map == n_val]
      if (length(hits) == 0) "" else paste(hits, collapse=" / ")
    })
    
    data.frame(
      n                                  = df_c$x,
      `Achieved Power (%)`               = round(df_c$y * 100, 1),
      `Beats target`                     = ifelse(df_c$y >= target, "Yes", "No"),
      `CI method(s) recommending this n` = ci_labels_at_n,
      check.names=FALSE, stringsAsFactors=FALSE
    )
  })
  
  output$power_table_ui <- renderUI({
    df <- power_table_df()
    if (is.null(df)) return(tags$p("No data available.", style="color:#94a3b8;font-size:12px;"))
    target <- as.numeric(power_d())
    
    rows_html <- lapply(seq_len(nrow(df)), function(i) {
      beats  <- df[i, "Beats target"] == "Yes"
      has_ci <- nzchar(df[i, "CI method(s) recommending this n"])
      bg     <- if (beats && has_ci) "#dcfce7" else if (beats) "#f0fdf4" else "#fff"
      border <- if (has_ci) "border-left:3px solid #16a34a;" else ""
      tags$tr(style=paste0("background:", bg, ";", border),
              tags$td(style="padding:4px 10px;font-size:12px;border:1px solid #e2e8f0;font-family:'DM Mono',monospace;text-align:center;", df[i,"n"]),
              tags$td(style=paste0("padding:4px 10px;font-size:12px;border:1px solid #e2e8f0;font-family:'DM Mono',monospace;text-align:center;", if(beats) "color:#166534;font-weight:600;" else "color:#374151;"), paste0(df[i,"Achieved Power (%)"])),
              tags$td(style=paste0("padding:4px 10px;font-size:12px;border:1px solid #e2e8f0;text-align:center;", if(beats) "color:#166534;font-weight:600;" else "color:#9a3412;"), df[i,"Beats target"]),
              tags$td(style="padding:4px 10px;font-size:11px;border:1px solid #e2e8f0;color:#374151;font-style:italic;", df[i,"CI method(s) recommending this n"])
      )
    })
    
    tagList(
      tags$p(style="font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:0.06em;color:#64748b;margin:0 0 6px;", "Power vs n table"),
      tags$p(style="font-size:11px;color:#94a3b8;margin:0 0 8px;",
             paste0("PG = ", pg_d(), "  \u00b7  expected performance = ", pd_d(),
                    "  \u00b7  target power = ", round(target*100), "%  \u00b7  \u03b1 = ", alpha_d())),
      tags$div(style="overflow-x:auto;max-height:400px;overflow-y:auto;",
               tags$table(style="border-collapse:collapse;width:100%;",
                          tags$thead(style="position:sticky;top:0;background:#f1f5f9;",
                                     tags$tr(
                                       tags$th(style="padding:5px 10px;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;text-align:center;", "n"),
                                       tags$th(style="padding:5px 10px;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;text-align:center;", "Achieved Power (%)"),
                                       tags$th(style="padding:5px 10px;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;text-align:center;", paste0("Beats ", round(target*100), "%")),
                                       tags$th(style="padding:5px 10px;border:1px solid #e2e8f0;font-size:11px;font-weight:700;color:#64748b;", "CI method(s) recommending this n")
                                     )
                          ),
                          tags$tbody(rows_html)
               )
      ),
      tags$p(style="font-size:10.5px;color:#94a3b8;margin:6px 0 0;",
             "\u25a0 Green = beats target  \u00b7  Green left border = a CI method recommends this exact n")
    )
  })
  
  output$downloadPowerTable <- downloadHandler(
    filename = function() paste0("PGPower_power_table_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- power_table_df()
      if (is.null(df)) df <- data.frame(Note = "No data available.")
      write.csv(df, file, row.names=FALSE)
    }
  )
  
  # ── NEW: Download power vs n plot ─────────────────────────────────────────────
  output$downloadPlotPower <- downloadHandler(
    filename    = function() paste0("PGPower_power_curve_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      pc      <- get_plot_colour()
      col_n   <- "#16a34a"
      col_tgt <- "#475569"
      df      <- prop_df_power()
      df_c    <- df[!is.na(df$y), ]
      target  <- as.numeric(power_d())
      n_req   <- tryCatch(as.integer(prop_n()), error=function(e) NA_integer_)
      p <- ggplot(df_c, aes(x=x, y=y)) +
        geom_hline(yintercept=target, linetype="dashed", colour=col_tgt, linewidth=0.9) +
        geom_line(colour=pc, linewidth=1.1) +
        scale_y_continuous(labels=scales::percent_format(accuracy=1),
                           limits=c(max(0, min(df_c$y, na.rm=TRUE)-0.04), 1)) +
        labs(title="Sample size (n) vs achieved power", x="n", y="Achieved power") +
        plot_theme_large
      if (!is.na(n_req)) {
        idx <- which.min(abs(df_c$x - n_req))
        if (length(idx) > 0)
          p <- p + geom_point(data=data.frame(x=df_c$x[idx], y=df_c$y[idx]),
                              aes(x=x, y=y), colour=col_n, size=3,
                              shape=21, fill=col_n, alpha=0.95)
      }
      ggsave(file, p, width=7, height=4, dpi=150, bg="white")
    }
  )
  
  # ── NEW: Download CI diagram — selected method ────────────────────────────────
  output$downloadPlotCI <- downloadHandler(
    filename    = function() paste0("PGPower_ci_diagram_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      df_ci <- ci_diagram_data()
      validate(need(!is.null(df_ci) && nrow(df_ci) > 0, "No CI diagram data available."))
      pc      <- get_plot_colour()
      pg_v    <- as.numeric(pg_d())
      is_sfty <- isTRUE(input$endpoint == "safety")
      df_ci$passes <- if (is_sfty) df_ci$upper < pg_v else df_ci$lower > pg_v
      df_ci$col    <- ifelse(df_ci$passes, pc, "#94a3b8")
      df_ci$y      <- seq_len(nrow(df_ci))
      h <- max(2.5, nrow(df_ci) * 0.65)
      p <- ggplot(df_ci) +
        geom_vline(xintercept=pg_v, linetype="dashed", colour="#475569", linewidth=0.8) +
        geom_segment(aes(x=lower, xend=upper, y=y, yend=y, colour=col), linewidth=2.5) +
        geom_point(aes(x=est, y=y, colour=col), size=3) +
        scale_colour_identity() +
        scale_y_reverse(breaks=df_ci$y, labels=df_ci$method) +
        labs(title=paste0("Confidence intervals at n = ", df_ci$n[1]),
             x="Proportion", y=NULL) +
        plot_theme_large +
        theme(axis.text.y=element_text(size=10))
      ggsave(file, p, width=7, height=h, dpi=150, bg="white")
    }
  )
  
  # ── NEW: Download CI diagram — all methods ────────────────────────────────────
  output$downloadPlotCIAll <- downloadHandler(
    filename    = function() paste0("PGPower_ci_diagram_all_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      pg_v     <- as.numeric(pg_d())
      pd_v     <- as.numeric(pd_d())
      alpha_v  <- as.numeric(alpha_d())
      is_sfty  <- isTRUE(input$endpoint == "safety")
      conf_lev <- 1 - 2 * alpha_v
      n_val    <- tryCatch(as.integer(prop_n()), error=function(e) NA_integer_)
      validate(need(!is.na(n_val) && is.finite(n_val), "No valid n available."))
      x_obs <- max(0L, min(as.integer(round(pd_v * n_val)), n_val))
      pc    <- get_plot_colour()
      rows  <- lapply(names(all_compare_methods), function(nm) {
        m  <- all_compare_methods[[nm]]
        ci <- tryCatch(prop_ci_vec(x_obs, n_val, conf_lev, m),
                       error=function(e) list(lower=NA_real_, upper=NA_real_))
        passes <- if (is_sfty) !is.na(ci$upper) && ci$upper < pg_v
        else          !is.na(ci$lower) && ci$lower > pg_v
        data.frame(method=nm, lower=ci$lower, upper=ci$upper,
                   est=pd_v, passes=passes, stringsAsFactors=FALSE)
      })
      df_ci     <- do.call(rbind, rows)
      df_ci$col <- ifelse(df_ci$passes, pc, "#94a3b8")
      df_ci$y   <- seq_len(nrow(df_ci))
      h <- max(2.5, nrow(df_ci) * 0.65)
      p <- ggplot(df_ci) +
        geom_vline(xintercept=pg_v, linetype="dashed", colour="#475569", linewidth=0.8) +
        geom_segment(aes(x=lower, xend=upper, y=y, yend=y, colour=col), linewidth=2.5) +
        geom_point(aes(x=est, y=y, colour=col), size=3) +
        scale_colour_identity() +
        scale_y_reverse(breaks=df_ci$y, labels=df_ci$method) +
        labs(title=paste0("CI diagram \u2014 all methods  (n = ", n_val, ")"),
             x="Proportion", y=NULL) +
        plot_theme_large +
        theme(axis.text.y=element_text(size=10))
      ggsave(file, p, width=7, height=h, dpi=150, bg="white")
    }
  )
  
  # ── Generate Report helpers ───────────────────────────────────────────────────
  output$rpt_calc_summary_ui <- renderUI({
    n_out     <- tryCatch(prop_n(), error=function(e) NA)
    dropout_r <- get_dropout_rate()
    alpha_val <- as.numeric(input$sig.level)
    ci_m      <- if (is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
    is_safety <- isTRUE(input$endpoint == "safety")
    pg_val    <- as.numeric(pg_d())
    pd_val    <- as.numeric(pd_d())
    pwr_val   <- as.numeric(power_d())
    n_fmt     <- if (is.na(n_out) || is.infinite(n_out)) "Not achievable"
    else format(n_out, big.mark=",")
    n_enrol   <- if (is.na(n_out) || is.infinite(n_out)) NA_integer_
    else ceiling(n_out / (1 - dropout_r/100))
    nd_fmt    <- if (is.na(n_enrol)) "\u2014" else format(n_enrol, big.mark=",")
    z_a       <- qnorm(1 - alpha_val)
    n_succ    <- if (is.na(n_out) || is.infinite(n_out)) NA_integer_
    else if (is_safety) floor(n_out*pg_val   - z_a*sqrt(n_out*pg_val*(1-pg_val)))
    else                ceiling(n_out*pg_val + z_a*sqrt(n_out*pg_val*(1-pg_val)))
    n_fail    <- if (is.na(n_succ) || is.na(n_out) || is.infinite(n_out)) NA_integer_
    else as.integer(n_out) - n_succ
    ns_fmt    <- if (is.na(n_succ)) "\u2014" else format(n_succ, big.mark=",")
    nf_fmt    <- if (is.na(n_fail)) "\u2014" else format(n_fail, big.mark=",")
    dr_rule   <- if (is.na(n_succ) || is.na(n_out) || is.infinite(n_out)) "\u2014"
    else if (is_safety) paste0("\u2264 ", n_succ, " events out of ", n_out)
    else paste0("\u2265 ", n_succ, " successes out of ", n_out)
    ci_labels <- c(asymptotic="Wald (Z-score)", wilson="Wilson Score",
                   ac="Agresti-Coull", exact="Clopper-Pearson",
                   prop.test="Prop.test", bayes="Jeffreys")
    ci_lbl    <- unname(ci_labels[ci_m]); if (is.na(ci_lbl)) ci_lbl <- ci_m
    pg_pct    <- round(pg_val * 100, 1)
    pd_pct    <- round(pd_val * 100, 1)
    pwr_pct   <- round(pwr_val * 100)
    
    ins_btn <- function(tag_str, display_val) {
      js <- paste0(
        "var ta=document.getElementById('rpt_interp_text');",
        "if(!ta)return;",
        "var s=ta.selectionStart,e=ta.selectionEnd;",
        "var ins='", tag_str, "';",
        "ta.value=ta.value.substring(0,s)+ins+ta.value.substring(e);",
        "ta.selectionStart=ta.selectionEnd=s+ins.length;",
        "ta.focus();",
        "Shiny.setInputValue('rpt_interp_text',ta.value,{priority:'event'});"
      )
      tags$button(
        class=  "pgp-ins-btn",
        title=  paste0("Insert ", tag_str, " into text"),
        onclick=js,
        tags$span(class="pgp-ins-tag",   tag_str),
        tags$span(class="pgp-ins-arrow", "\u2191")
      )
    }
    
    row <- function(label, value, tag_str=NULL) {
      tags$div(class="pgp-cv-row",
               tags$span(class="pgp-cv-label", label),
               tags$span(class="pgp-cv-value", value),
               if (!is.null(tag_str)) ins_btn(tag_str, value) else tags$span())
    }
    sec <- function(title) tags$div(class="pgp-cv-section", title)
    
    tags$div(class="pgp-calc-values",
             sec("Sample size"),
             row("Required n",            n_fmt,                  "{n}"),
             row("Enrolment w/ dropout",  nd_fmt,                 "{n_dropout}"),
             sec("Decision"),
             row("Decision rule",         dr_rule,                "{decision_rule}"),
             row(tags$b("n successes"),   ns_fmt,                 "{n_successes}"),
             row(tags$b("n failures"),    nf_fmt,                 "{n_failures}"),
             sec("Design parameters"),
             row("Performance goal (PG)", paste0(pg_pct, "%"),    "{pg_pct}"),
             row("Expected performance",  paste0(pd_pct, "%"),    "{pd_pct}"),
             row("Power",                 paste0(pwr_pct, "%"),   "{power_pct}"),
             row("\u03b1 (one-sided)",    as.character(alpha_val),"{alpha}"),
             row("CI method",             ci_lbl,                 "{ci_method}"),
             row("Dropout",               paste0(dropout_r, "%"), "{dropout_pct}")
    )
  })
  
  # ── FIX: render even when accordion is hidden ─────────────────────────────────
  outputOptions(output, "rpt_calc_summary_ui", suspendWhenHidden = FALSE)
  
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
      list(label="Results table",        on=isTRUE(input$rpt_results)),
      list(label="Full n summary",       on=isTRUE(input$rpt_n_box)),
      list(label="Interpretation",       on=isTRUE(input$rpt_interp_inc)),
      list(label="CI method comparison", on=isTRUE(input$rpt_ci_compare)),
      list(label="Definitions",          on=isTRUE(input$rpt_definitions)),
      list(label="Calculation code",     on=isTRUE(input$rpt_calc_code))
    )
    sens_items <- list(
      list(label="CI diagram",                             on=isTRUE(input$rpt_ci_diagram)),
      list(label="Power vs n plot",                        on=isTRUE(input$rpt_plot_power)),
      list(label="Expected performance sensitivity plot",  on=isTRUE(input$rpt_plot_p1)),
      list(label="Expected performance sensitivity table", on=isTRUE(input$rpt_table_p1))
    )
    tags$ul(class="report-contents",
            lapply(general_items, function(i) make_li(i$label, i$on)),
            make_group("Sensitivity Analysis", sens_items))
  })
  
  output$report_download_ui <- renderUI({
    if (isTRUE(input$report_format == "pdf"))
      downloadButton("downloadPDF", "\u2193 Download report (.pdf)",
                     class="btn btn-outline-secondary pgp-btn report-dl-btn")
    else
      downloadButton("downloadWord", "\u2193 Download report (.docx)",
                     class="btn btn-outline-secondary pgp-btn report-dl-btn")
  })
  
  # ── Report data ───────────────────────────────────────────────────────────────
  build_report_data <- function() {
    ci_m  <- if (is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
    dr    <- if (is.null(input$dropout_rate)) 10 else as.numeric(input$dropout_rate)
    if (is.na(dr) || dr < 0 || dr >= 100) dr <- 10
    n_val <- prop_total_n(pg_d(), pd_d(),
                          ci_method=ci_m,
                          sim_n=if(is.null(input$sim_quality)) 1000 else as.numeric(input$sim_quality),
                          seed =if(is.null(input$sim_seed))    1    else as.numeric(input$sim_seed))
    ci_labels <- c(asymptotic="Wald (Z-score)", wilson="Wilson Score",
                   ac="Agresti-Coull", exact="Clopper-Pearson",
                   prop.test="Prop.test", bayes="Jeffreys")
    ci_lbl  <- unname(ci_labels[ci_m]); if (is.na(ci_lbl)) ci_lbl <- ci_m
    is_sfty <- isTRUE(input$endpoint == "safety")
    pg_val  <- as.numeric(pg_d())
    z_a     <- qnorm(1 - as.numeric(input$sig.level))
    n_succ  <- if (is.infinite(n_val)) NA_integer_
    else if (is_sfty) floor(n_val*pg_val   - z_a*sqrt(n_val*pg_val*(1-pg_val)))
    else              ceiling(n_val*pg_val + z_a*sqrt(n_val*pg_val*(1-pg_val)))
    n_drop  <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr/100))
    n_fail  <- if (is.na(n_succ) || is.infinite(n_val)) NA_integer_
    else as.integer(n_val) - n_succ
    dr_rule <- if (is.na(n_succ) || is.infinite(n_val)) "\u2014"
    else if (is_sfty) paste0("\u2264 ", n_succ, " events out of ", n_val)
    else paste0("\u2265 ", n_succ, " successes out of ", n_val)
    list(
      n_val=n_val, ci_label=ci_lbl,
      n_fmt  = if (is.infinite(n_val)) "Not achievable" else format(n_val, big.mark=","),
      ns_fmt = if (is.na(n_succ)) "\u2014" else format(n_succ, big.mark=","),
      nf_fmt = if (is.na(n_fail)) "\u2014" else format(n_fail, big.mark=","),
      nd_fmt = if (is.na(n_drop)) "\u2014" else format(n_drop, big.mark=","),
      dr_rule=dr_rule, n_successes=n_succ, pg_val=pg_val, dropout_r=dr
    )
  }
  
  get_section_order <- function() c("results","n_box","interp","ci_compare",
                                    "definitions","calc_code","ci_diagram",
                                    "plot_power","plot_p1","table_p1")
  
  capture_plots <- function(include_power=FALSE, include_sens=FALSE, include_ci_diag=FALSE) {
    pc        <- get_plot_colour()
    col_target <- "#475569"
    col_n      <- "#16a34a"
    
    pwr_file <- if (include_power) tryCatch({
      df_pw  <- prop_df_power()
      df_c   <- df_pw[!is.na(df_pw$y), ]
      target <- as.numeric(power_d())
      n_req  <- tryCatch(as.integer(prop_n()), error=function(e) NA_integer_)
      p <- ggplot(df_c, aes(x=x, y=y)) +
        geom_hline(yintercept=target, linetype="dashed", colour=col_target, linewidth=0.9) +
        geom_line(colour=pc, linewidth=1.1) +
        scale_y_continuous(labels=scales::percent_format(accuracy=1),
                           limits=c(max(0, min(df_c$y, na.rm=TRUE)-0.04), 1)) +
        labs(title="Sample size (n) vs achieved power", x="n", y="Achieved power") +
        plot_theme_large
      if (!is.na(n_req)) {
        idx <- which.min(abs(df_c$x - n_req))
        if (length(idx) > 0)
          p <- p + geom_point(data=data.frame(x=df_c$x[idx], y=df_c$y[idx]),
                              aes(x=x, y=y), colour=col_n, size=3, shape=21, fill=col_n, alpha=0.95)
      }
      tmp <- tempfile(fileext=".png"); ggsave(tmp, p, width=6, height=3.5, dpi=150, bg="white"); tmp
    }, error=function(e) NULL) else NULL
    
    sens_file <- if (include_sens) tryCatch({
      df  <- prop_df_sens()
      lbl <- if (isTRUE(input$endpoint=="safety")) "Expected performance (event proportion)"
      else "Expected performance (success proportion)"
      p <- ggplot(df, aes(x=x, y=y)) +
        geom_line(colour=pc, linewidth=1.1) + geom_point(colour=pc, size=2) +
        labs(title="Expected performance vs required sample size", x=lbl, y="Required n") +
        plot_theme_large
      tmp <- tempfile(fileext=".png"); ggsave(tmp, p, width=6, height=3.5, dpi=150, bg="white"); tmp
    }, error=function(e) NULL) else NULL
    
    ci_diag_file <- if (include_ci_diag) tryCatch({
      df_ci <- ci_diagram_data()
      if (is.null(df_ci) || nrow(df_ci)==0) NULL else {
        pg_v    <- as.numeric(pg_d())
        is_sfty <- isTRUE(input$endpoint=="safety")
        df_ci$passes <- if (is_sfty) df_ci$upper < pg_v else df_ci$lower > pg_v
        df_ci$col    <- ifelse(df_ci$passes, "#16a34a", "#dc2626")
        df_ci$y      <- seq_len(nrow(df_ci))
        p <- ggplot(df_ci) +
          geom_vline(xintercept=pg_v, linetype="dashed", colour="#475569", linewidth=0.8) +
          geom_segment(aes(x=lower, xend=upper, y=y, yend=y, colour=col), linewidth=2.5) +
          geom_point(aes(x=est, y=y, colour=col), size=3) +
          scale_colour_identity() +
          scale_y_reverse(breaks=df_ci$y, labels=df_ci$method) +
          labs(title=paste0("Confidence intervals at n = ", df_ci$n[1]),
               x="Proportion", y=NULL) +
          plot_theme_large + theme(axis.text.y=element_text(size=10))
        h <- max(2.5, nrow(df_ci)*0.55)
        tmp <- tempfile(fileext=".png"); ggsave(tmp, p, width=6, height=h, dpi=150, bg="white"); tmp
      }
    }, error=function(e) NULL) else NULL
    
    list(power=pwr_file, p1=sens_file, ci_diag=ci_diag_file)
  }
  
  build_interp_text <- function(rd) {
    tpl <- if (!is.null(input$rpt_interp_text) && nchar(trimws(input$rpt_interp_text)) > 0)
      input$rpt_interp_text
    else paste0(
      "A total of {n} evaluable patients are required to demonstrate, with ",
      "{power_pct}% power, that the device proportion meets the performance goal of {pg_pct}%, ",
      "assuming a true expected performance proportion of {pd_pct}%. ",
      "Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. ",
      "The study will be deemed successful if at least {n_successes} out of {n} ",
      "evaluable patients achieve the primary endpoint.")
    pg_pct  <- round(as.numeric(pg_d())*100, 1)
    pd_pct  <- round(as.numeric(pd_d())*100, 1)
    pwr_pct <- round(input$power*100)
    txt <- tpl
    txt <- gsub("{n}",             rd$n_fmt,                   txt, fixed=TRUE)
    txt <- gsub("{n_dropout}",     rd$nd_fmt,                  txt, fixed=TRUE)
    txt <- gsub("{n_successes}",   rd$ns_fmt,                  txt, fixed=TRUE)
    txt <- gsub("{power_pct}",     as.character(pwr_pct),      txt, fixed=TRUE)
    txt <- gsub("{pg_pct}",        as.character(pg_pct),       txt, fixed=TRUE)
    txt <- gsub("{pd_pct}",        as.character(pd_pct),       txt, fixed=TRUE)
    txt <- gsub("{p0_pct}",        as.character(pg_pct),       txt, fixed=TRUE)
    txt <- gsub("{p1_pct}",        as.character(pd_pct),       txt, fixed=TRUE)
    txt <- gsub("{alpha}",         as.character(input$sig.level), txt, fixed=TRUE)
    txt <- gsub("{ci_method}",     rd$ci_label,                txt, fixed=TRUE)
    txt <- gsub("{dropout_pct}",   as.character(rd$dropout_r), txt, fixed=TRUE)
    txt <- gsub("{n_failures}",    rd$nf_fmt,                  txt, fixed=TRUE)
    txt <- gsub("{decision_rule}", rd$dr_rule,                 txt, fixed=TRUE)
    txt
  }
  
  make_sens_table_html <- function(df, col_names, caption, blue, th_fn, td_fn) {
    df2 <- df
    df2[[2]] <- ifelse(is.infinite(df2[[2]])|is.na(df2[[2]]), "\u2014",
                       format(round(df2[[2]]), big.mark=","))
    paste0(
      "<p style='font-size:9px;color:#555;font-style:italic;margin:4px 0 6px;'>", caption, "</p>",
      "<table style='border-collapse:collapse;width:100%;'>",
      "<tr>", paste(sapply(col_names, th_fn), collapse=""), "</tr>",
      paste(apply(df2, 1, function(r) paste0("<tr>", paste(sapply(r, td_fn), collapse=""), "</tr>")), collapse=""),
      "</table>"
    )
  }
  
  # ── PDF download ──────────────────────────────────────────────────────────────
  output$downloadPDF <- downloadHandler(
    filename    = function() paste0("PGPower_", format(Sys.Date(),"%d_%b_%Y"), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      tryCatch({
        rd       <- build_report_data()
        n_val    <- rd$n_val; ci_label <- rd$ci_label
        n_fmt    <- rd$n_fmt; ns_fmt <- rd$ns_fmt; nd_fmt <- rd$nd_fmt
        is_sfty  <- isTRUE(input$endpoint == "safety")
        ep_txt   <- if (is_sfty) "Lower is better" else "Higher is better"
        av       <- as.numeric(input$sig.level)
        pg_val   <- as.numeric(pg_d())
        pd_val   <- as.numeric(pd_d())
        pwr_val  <- as.numeric(power_d())
        dr_r     <- get_dropout_rate()
        
        actual_pw_rpt <- tryCatch({
          df_pw <- prop_df_power(); df_c <- df_pw[!is.na(df_pw$y), ]
          if (nrow(df_c) > 0 && !is.infinite(n_val)) {
            idx <- which.min(abs(df_c$x - n_val)); round(df_c$y[idx] * 100, 1)
          } else NA_real_
        }, error=function(e) NA_real_)
        actual_pw_txt <- if (!is.na(actual_pw_rpt)) paste0(actual_pw_rpt, "%") else "\u2014"
        
        show_results    <- !isTRUE(input$rpt_results    == FALSE)
        show_interp     <- !isTRUE(input$rpt_interp_inc == FALSE) && !is.infinite(n_val) && !is.na(rd$n_successes)
        show_ci_cmp     <- !isTRUE(input$rpt_ci_compare == FALSE)
        show_defs       <- !isTRUE(input$rpt_definitions== FALSE)
        show_code       <- !isTRUE(input$rpt_calc_code  == FALSE)
        show_n_box      <- !isTRUE(input$rpt_n_box      == FALSE)
        show_plot_power <- isTRUE(input$rpt_plot_power)
        show_plot_p1    <- isTRUE(input$rpt_plot_p1)
        show_table_p1   <- isTRUE(input$rpt_table_p1)
        show_ci_diag    <- isTRUE(input$rpt_ci_diagram)
        
        plot_files <- capture_plots(include_power=show_plot_power,
                                    include_sens=show_plot_p1,
                                    include_ci_diag=show_ci_diag)
        
        blue  <- "#1e40af"; grey <- "#64748b"
        lbl_s <- "padding:6px 10px;border:1px solid #e2e8f0;font-size:9px;font-weight:600;color:#374151;white-space:nowrap;background:#f8fafc;"
        val_s <- "padding:6px 10px;border:1px solid #e2e8f0;font-size:9px;color:#1a2e35;"
        th_s  <- paste0("padding:6px 10px;background:",blue,";color:#fff;font-size:9px;font-weight:600;text-align:left;border:1px solid #1e3a8a;")
        td_s  <- "padding:6px 10px;border:1px solid #e2e8f0;font-size:9px;color:#1a2e35;"
        td_c  <- paste0(td_s, "text-align:center;font-family:'Courier New',monospace;")
        th    <- function(v) paste0("<th style='",th_s,"'>",v,"</th>")
        td    <- function(v) paste0("<td style='",td_s,"'>",v,"</td>")
        tdc   <- function(v) paste0("<td style='",td_c,"'>",v,"</td>")
        lbl   <- function(v) paste0("<td style='",lbl_s,"'>",v,"</td>")
        val   <- function(v) paste0("<td style='",val_s,"'>",v,"</td>")
        h2s   <- paste0("color:",blue,";font-size:12px;font-weight:700;margin:22px 0 8px;padding-bottom:4px;border-bottom:2px solid #e2e8f0;")
        hr    <- "<hr style='border:none;border-top:1px solid #e2e8f0;margin:18px 0;'>"
        tbl   <- function(inner) paste0("<table style='border-collapse:collapse;width:100%;margin-bottom:6px;'>",inner,"</table>")
        
        results_html <- if (show_results) paste0(
          "<h2 style='",h2s,"'>Results</h2>",
          "<p style='font-size:9px;color:",grey,";margin:0 0 8px;'>",
          if(is_sfty) "H\u2080: device proportion \u2265 PG &nbsp;\u2014&nbsp; H\u2081: device proportion &lt; PG"
          else        "H\u2080: device proportion \u2264 PG &nbsp;\u2014&nbsp; H\u2081: device proportion &gt; PG",
          "</p>",
          tbl(paste0("<tr>",th("PG"),th("Expected performance"),th("\u03b1"),th("Target power"),
                     th("Required n"),th("Actual power"),th("Events threshold"),
                     th(paste0(dr_r,"% dropout enrolment")),th("Method"),"</tr>",
                     "<tr>",tdc(pg_val),tdc(pd_val),tdc(av),tdc(paste0(round(pwr_val*100),"%")),
                     tdc(n_fmt),tdc(actual_pw_txt),tdc(ns_fmt),tdc(nd_fmt),tdc(ci_label),"</tr>")),hr
        ) else ""
        
        n_box_html <- if (show_n_box) {
          z_nb <- qnorm(1-av)
          n_sn <- if (is.infinite(n_val)) NA_integer_
          else if (is_sfty) floor(n_val*pg_val  - z_nb*sqrt(n_val*pg_val*(1-pg_val)))
          else              ceiling(n_val*pg_val + z_nb*sqrt(n_val*pg_val*(1-pg_val)))
          nd_n <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr_r/100))
          rows_nb <- list(
            c("Endpoint direction",   ep_txt),
            c("Performance goal (PG)",as.character(pg_val)),
            c("Expected performance", as.character(pd_val)),
            c("\u03b1 (one-sided)",   paste0(av," \u2192 ",round((1-2*av)*100,1),"% CI")),
            c("Target power",         paste0(round(pwr_val*100),"%")),
            c("Actual achieved power",actual_pw_txt),
            c("CI method",            ci_label),
            c("Required n",           n_fmt),
            c("Events threshold",     if(is.na(n_sn)) "\u2014" else format(n_sn,big.mark=",")),
            c(paste0("Enrolment (",dr_r,"% dropout)"), if(is.na(nd_n)) "\u2014" else format(nd_n,big.mark=","))
          )
          paste0("<h2 style='",h2s,"'>Sample Size Summary</h2>",
                 tbl(paste0("<tr>",th("Parameter"),th("Value"),"</tr>",
                            paste(sapply(rows_nb, function(r) paste0("<tr>",lbl(r[1]),val(r[2]),"</tr>")),collapse=""))),hr)
        } else ""
        
        interp_html <- if (show_interp) paste0(
          "<h2 style='",h2s,"'>Interpretation</h2>",
          "<p style='font-size:9.5px;line-height:1.75;color:#374151;'>",
          build_interp_text(rd),"</p>",hr) else ""
        
        ci_html <- if (show_ci_cmp) {
          ci_methods <- all_compare_methods
          get_pw <- function(n_v) {
            if (is.na(n_v)||!is.finite(n_v)) return(NA_real_)
            pg_b <- if(is_sfty) 1-pg_val else pg_val
            pd_b <- if(is_sfty) 1-pd_val else pd_val
            xc   <- qbinom(1-av,n_v,pg_b)+1L
            if (xc>n_v) return(NA_real_)
            1-pbinom(xc-1L,n_v,pd_b)
          }
          cur_m <- if(is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
          rows_ci <- lapply(names(ci_methods), function(nm) {
            m     <- ci_methods[[nm]]
            n_m   <- tryCatch(prop_total_n(pg_val,pd_val,ci_method=m,sim_n=400L,seed=1L),error=function(e) Inf)
            n_txt <- if(is.infinite(n_m)) "\u2014" else format(as.integer(n_m),big.mark=",")
            pw_m  <- get_pw(if(is.finite(n_m)) as.integer(n_m) else NA_integer_)
            pw_txt<- if(is.na(pw_m)) "\u2014" else paste0(round(pw_m*100,1),"%")
            sel   <- m==cur_m
            is_ok <- is.finite(n_m)&&!is.na(n_val)&&!is.infinite(n_val)&&as.integer(n_m)<=as.integer(n_val)
            bg    <- if(!is.finite(n_m)) "#fef3c7" else if(is_ok) "#dcfce7" else "#fff7ed"
            fg    <- if(!is.finite(n_m)) "#92400e" else if(is_ok) "#166534" else "#9a3412"
            paste0("<tr>",
                   "<td style='",td_s,"font-weight:",if(sel)"700"else"400","'>",nm,"</td>",
                   "<td style='",td_c,"color:",fg,";font-weight:600;background:",bg,"'>",n_txt,"</td>",
                   "<td style='",td_c,"color:",fg,";background:",bg,"'>",pw_txt,"</td>","</tr>")
          })
          paste0("<h2 style='",h2s,"'>CI method comparison</h2>",
                 "<p style='font-size:9px;color:",grey,";margin:0 0 8px;'>PG = ",pg_val,
                 "  |  expected performance = ",pd_val,"  |  \u03b1 = ",av,
                 "  |  target power = ",round(pwr_val*100),"%</p>",
                 tbl(paste0("<tr>",th("Method"),th("Required n"),th("Achieved power (%)"),"</tr>",
                            paste(rows_ci,collapse=""))),
                 "<p style='font-size:8.5px;color:",grey,";margin:4px 0 0;font-style:italic;'>",
                 "Green = meets or beats required n \u00b7 Amber = larger n \u00b7 Bold = selected method</p>",hr)
        } else ""
        
        defs_list <- list(
          c("n","Minimum number of evaluable patients required."),
          c("Events threshold","Min successes (or max events) needed to meet the decision rule."),
          c("Actual power","Exact binomial power at the required n."),
          c("PG","Performance goal \u2014 pre-specified benchmark rate."),
          c("Expected performance","Anticipated true event/success proportion of the device."),
          c("\u03b1","Probability of a false-positive result (one-sided)."),
          c("CI method","Method used to construct the confidence interval for the decision rule.")
        )
        defs_html <- if (show_defs) paste0(
          "<h2 style='",h2s,"'>Definitions</h2>",
          tbl(paste0("<tr>",th("Term"),th("Definition"),"</tr>",
                     paste(sapply(defs_list,function(d) paste0("<tr>",lbl(d[1]),val(d[2]),"</tr>")),collapse=""))),hr) else ""
        
        code_html <- if (show_code) paste0(
          "<h2 style='",h2s,"'>Calculation code</h2>",
          "<pre style='background:#f1f5f9;border:1px solid #e2e8f0;border-radius:6px;",
          "padding:12px;font-family:\"Courier New\",monospace;font-size:8px;",
          "color:#1a1a2e;line-height:1.75;overflow-x:auto;'>",
          build_calc_code_txt(),"</pre>",hr) else ""
        
        img_tag <- function(f, cap) {
          if (is.null(f)||!file.exists(f)) return("")
          b64 <- base64enc::base64encode(f)
          paste0("<figure style='margin:14px 0 6px;page-break-inside:avoid;'>",
                 "<img src='data:image/png;base64,",b64,"' style='width:100%;max-width:560px;border:1px solid #e2e8f0;border-radius:4px;'>",
                 "<figcaption style='font-size:8.5px;color:",grey,";margin-top:4px;font-style:italic;'>",cap,"</figcaption></figure>")
        }
        
        ci_diag_html    <- if (show_ci_diag && !is.null(plot_files$ci_diag) && file.exists(plot_files$ci_diag))
          paste0("<h2 style='",h2s,"'>Confidence interval diagram</h2>",
                 img_tag(plot_files$ci_diag, paste0("CI at n = ",tryCatch(as.integer(prop_n()),error=function(e)"?"),
                                                    "  (observed proportion = ",pd_val,")")),hr) else ""
        plot_power_html <- if (show_plot_power)
          paste0("<h2 style='",h2s,"'>Power vs n</h2>",
                 img_tag(plot_files$power,"Sample size (n) vs achieved power (%)"),hr) else ""
        plot_p1_html    <- if (show_plot_p1)
          paste0("<h2 style='",h2s,"'>Expected performance sensitivity</h2>",
                 img_tag(plot_files$p1,"Expected performance vs required sample size"),hr) else ""
        
        df_p1_pdf      <- if (show_table_p1) tryCatch(prop_df_sens(),error=function(e) NULL) else NULL
        table_p1_html  <- if (show_table_p1 && !is.null(df_p1_pdf))
          paste0("<h2 style='",h2s,"'>Expected performance sensitivity table</h2>",
                 make_sens_table_html(df_p1_pdf, col_names=c("Expected performance","Required n"),
                                      caption=paste0("PG = ",pg_val,"  |  \u03b1 = ",av,"  |  power = ",round(pwr_val*100),"%"),
                                      blue=blue, th_fn=th, td_fn=td),hr) else ""
        
        section_map <- list(results=results_html, n_box=n_box_html, interp=interp_html,
                            ci_compare=ci_html, definitions=defs_html, calc_code=code_html,
                            ci_diagram=ci_diag_html, plot_power=plot_power_html,
                            plot_p1=plot_p1_html, table_p1=table_p1_html)
        body_html <- paste(sapply(get_section_order(), function(s) section_map[[s]] %||% ""), collapse="")
        
        rpt_title <- if (!is.null(input$rpt_title) && nchar(trimws(input$rpt_title))>0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        sub_parts <- c(
          if (isTRUE(input$rpt_include_author)&&!is.null(input$rpt_author_name)&&
              nchar(trimws(input$rpt_author_name))>0) paste0("Author: ",trimws(input$rpt_author_name)),
          if (!isTRUE(input$rpt_include_date  ==FALSE)) format(Sys.Date(),"%d %B %Y"),
          if (!isTRUE(input$rpt_include_method==FALSE)) paste0("Method: ",ci_label)
        )
        sub_line <- if (length(sub_parts)>0)
          paste0("<p class='sub'>",paste(sub_parts,collapse=" &nbsp;|&nbsp; "),"</p>") else ""
        
        html_out <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'><style>",
          "*,*::before,*::after{box-sizing:border-box;}",
          "body{font-family:'Helvetica Neue',Arial,sans-serif;margin:0;padding:0;",
          "color:#1a2e35;font-size:10px;line-height:1.55;background:#fff;}",
          "h1{color:",blue,";font-size:18px;margin:0 0 3px;font-weight:700;}",
          "h2{color:",blue,";font-size:12px;font-weight:700;margin:22px 0 8px;",
          "padding-bottom:4px;border-bottom:2px solid #e2e8f0;}",
          ".sub{font-size:8.5px;color:#64748b;margin:0;}",
          "table{border-collapse:collapse;width:100%;margin-bottom:6px;}",
          "pre{white-space:pre-wrap;word-break:break-all;}",
          "figure{margin:14px 0 6px;page-break-inside:avoid;}",
          "img{max-width:100%;border:1px solid #e2e8f0;border-radius:4px;}",
          "@page{size:A4;margin:20mm 18mm 24mm 18mm;",
          "@bottom-right{content:'Page ' counter(page) ' of ' counter(pages);",
          "font-size:8px;color:#94a3b8;font-family:'Helvetica Neue',Arial,sans-serif;}",
          "@bottom-left{content:'Generated by PG-Power';",
          "font-size:8px;color:#94a3b8;font-family:'Helvetica Neue',Arial,sans-serif;}}",
          ".page-header{padding:0 0 12px;border-bottom:3px solid ",blue,";margin-bottom:18px;}",
          "</style></head><body>",
          "<div class='page-header'><h1>",rpt_title,"</h1>",sub_line,"</div>",
          body_html,"</body></html>"
        )
        
        tmp_html <- tempfile(fileext=".html")
        writeLines(html_out, con=tmp_html, useBytes=FALSE)
        if (requireNamespace("pagedown", quietly=TRUE)) {
          pagedown::chrome_print(tmp_html, output=file, wait=20); unlink(tmp_html)
        } else if (requireNamespace("webshot2", quietly=TRUE)) {
          webshot2::webshot(tmp_html, file=file, vwidth=794, vheight=1123); unlink(tmp_html)
        } else {
          file.copy(tmp_html, file, overwrite=TRUE); unlink(tmp_html)
          showNotification("PDF engine not found. Downloaded as HTML. Install pagedown for PDF.",
                           type="warning", duration=12)
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
        show_plot_p1_w    <- isTRUE(input$rpt_plot_p1)
        show_table_p1_w   <- isTRUE(input$rpt_table_p1)
        show_plot_power_w <- isTRUE(input$rpt_plot_power)
        show_ci_diag_w    <- isTRUE(input$rpt_ci_diagram)
        plot_files_w <- capture_plots(include_power=show_plot_power_w,
                                      include_sens=show_plot_p1_w,
                                      include_ci_diag=show_ci_diag_w)
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
        rpt_title <- if (!is.null(input$rpt_title)&&nchar(trimws(input$rpt_title))>0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        sub_parts <- c(
          if (isTRUE(input$rpt_include_author)&&!is.null(input$rpt_author_name)&&
              nchar(trimws(input$rpt_author_name))>0) paste0("Author: ",trimws(input$rpt_author_name)),
          if (!isTRUE(input$rpt_include_date  ==FALSE)) format(Sys.Date(),"%d %B %Y"),
          if (!isTRUE(input$rpt_include_method==FALSE)) paste0("Method: ",ci_label)
        )
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(rpt_title, title_fmt), fp_p=tight_p))
        if (length(sub_parts)>0)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(paste(sub_parts,collapse="  |  "),sub_fmt), fp_p=tight_p))
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p=border_p))
        doc <- officer::body_add_par(doc, "", style="Normal")
        
        if (!isTRUE(input$rpt_results==FALSE)) {
          summary_df <- data.frame(
            Power=format(input$power,nsmall=3), n=n_fmt,
            `Events threshold`=ns_fmt, Enrolment=nd_fmt,
            PG=format(pg_d(),nsmall=3), `Expected performance`=format(pd_d(),nsmall=3),
            Alpha=format(as.numeric(input$sig.level)), Method=ci_label,
            check.names=FALSE, stringsAsFactors=FALSE)
          colnames(summary_df)[4] <- paste0(rd$dropout_r,"% dropout")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Results",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(
            if(is_sfty) "H\u2080: device proportion \u2265 PG  vs.  H\u2081: device proportion < PG"
            else        "H\u2080: device proportion \u2264 PG  vs.  H\u2081: device proportion > PG",
            hyp_fmt), fp_p=tight_p))
          doc <- officer::body_add_table(doc, summary_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_n_box==FALSE)) {
          dr_w <- get_dropout_rate(); av_w <- as.numeric(input$sig.level)
          pg_w <- as.numeric(pg_d()); z_w <- qnorm(1-av_w)
          n_sn_w <- if(is.infinite(n_val)) NA_integer_
          else if(is_sfty) floor(n_val*pg_w  -z_w*sqrt(n_val*pg_w*(1-pg_w)))
          else             ceiling(n_val*pg_w +z_w*sqrt(n_val*pg_w*(1-pg_w)))
          nd_w   <- if(is.infinite(n_val)) NA_integer_ else ceiling(n_val/(1-dr_w/100))
          nb_df  <- data.frame(
            Parameter=c("Endpoint direction","Performance goal (PG)","Expected performance",
                        "\u03b1 / CI equivalent","Power","Method","Required n",
                        "Events threshold",paste0("Enrolment (",dr_w,"% dropout)")),
            Value=c(ep_txt,as.character(pg_d()),as.character(pd_d()),
                    paste0(av_w," / ",round((1-2*av_w)*100,1),"% CI"),
                    paste0(round(power_d()*100),"%"),ci_label,n_fmt,
                    if(is.na(n_sn_w)) "\u2014" else format(n_sn_w,big.mark=","),
                    if(is.na(nd_w))   "\u2014" else format(nd_w,big.mark=",")),
            stringsAsFactors=FALSE)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Sample Size Summary",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_table(doc, nb_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!is.infinite(n_val)&&!is.na(rd$n_successes)&&!isTRUE(input$rpt_interp_inc==FALSE)) {
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Interpretation",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(build_interp_text(rd),body_fmt),fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_ci_compare==FALSE)) {
          cur_m  <- if(is.null(input$ci_method_prop)) "exact" else input$ci_method_prop
          m_ns   <- sapply(all_compare_methods, function(m) {
            n <- prop_total_n(pg_d(),pd_d(),ci_method=m,sim_n=400,seed=1)
            if(is.infinite(n)) "\u2014" else format(n,big.mark=",")
          })
          ci_df  <- as.data.frame(matrix(unname(m_ns),nrow=1),stringsAsFactors=FALSE)
          colnames(ci_df) <- names(all_compare_methods)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Sample size by method",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_table(doc, ci_df, align_table="left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_definitions==FALSE)) {
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Definitions",h2_fmt),fp_p=tight_p))
          defs <- list(
            list("Power: ",            "Probability of correctly rejecting H\u2080."),
            list("n: ",                "Minimum evaluable patients required."),
            list("Events threshold: ", "Min/max events required to meet the decision rule (proportion-based CI test)."),
            list("PG: ",               "Performance goal \u2014 pre-specified benchmark rate."),
            list("Expected performance: ", "Anticipated true event/success proportion of the device."),
            list("\u03b1: ",           "Probability of a false-positive result (one-sided)."),
            list("CI Method: ",        "Method used to construct the confidence interval.")
          )
          for (d in defs)
            doc <- officer::body_add_fpar(doc, officer::fpar(
              officer::ftext(paste0("\u2022  ",d[[1]]),def_term),
              officer::ftext(d[[2]],def_body), fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (!isTRUE(input$rpt_calc_code==FALSE)) {
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Calculation",h2_fmt),fp_p=tight_p))
          for (line in strsplit(build_calc_code_txt(),"\n")[[1]])
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(line,mono_fmt),fp_p=tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        
        if (show_ci_diag_w&&!is.null(plot_files_w$ci_diag)&&file.exists(plot_files_w$ci_diag)) {
          n_ci_rows <- tryCatch(nrow(ci_diagram_data()) %||% 1L, error=function(e) 1L)
          h_ci      <- max(2.0, n_ci_rows*0.55)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Confidence interval diagram",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_img(doc, src=plot_files_w$ci_diag, width=5.5, height=h_ci)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        if (show_plot_power_w&&!is.null(plot_files_w$power)&&file.exists(plot_files_w$power)) {
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Power vs n Plot",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_img(doc, src=plot_files_w$power, width=5.5, height=3.2)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        if (show_plot_p1_w&&!is.null(plot_files_w$p1)&&file.exists(plot_files_w$p1)) {
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Device Rate Sensitivity Plot",h2_fmt),fp_p=tight_p))
          doc <- officer::body_add_img(doc, src=plot_files_w$p1, width=5.5, height=3.2)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
          doc <- officer::body_add_par(doc, "", style="Normal")
        }
        if (show_table_p1_w) {
          df_w <- tryCatch(prop_df_sens(),error=function(e) NULL)
          if (!is.null(df_w)) {
            df_w[[2]] <- ifelse(is.infinite(df_w[[2]])|is.na(df_w[[2]]),"\u2014",format(round(df_w[[2]]),big.mark=","))
            df_w[[1]] <- sprintf("%.3f",df_w[[1]])
            colnames(df_w) <- c("Device Rate","Required N")
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Device Rate Sensitivity Table",h2_fmt),fp_p=tight_p))
            doc <- officer::body_add_table(doc, df_w, align_table="left")
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "),fp_p=border_p))
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
                               border.top=officer::fp_border(color="#e2e8f0",width=1))
        ))
        print(doc, target=file)
      }, error=function(e) message("Word ERROR: ", conditionMessage(e)))
    }
  )
  
  # ── Calculation code display ──────────────────────────────────────────────────
  output$calc_code_ui <- renderUI({
    if (!isTRUE(input$show_calc_code)) return(NULL)
    code_block_ui(build_calc_code_txt())
  })
  
  # ── FIX: GitHub popup — only fires when genuinely ticked ON ──────────────────
  observeEvent(input$show_calc_code, {
    req(isTRUE(input$show_calc_code))
    session$sendCustomMessage("showGithubPopup", "https://github.com/FilipMKgit/MDX2526")
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  observeEvent(input$rpt_calc_code, {
    req(isTRUE(input$rpt_calc_code))
    session$sendCustomMessage("showGithubPopup", "https://github.com/FilipMKgit/MDX2526")
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  # ── CI diagram wrapper ────────────────────────────────────────────────────────
  output$ci_diagram_wrapper <- renderUI({
    if (!isTRUE(input$showCIDiagram)) return(NULL)
    df    <- ci_diagram_data()
    n_rows <- if (is.null(df)) 1L else max(1L, nrow(df))
    h_px   <- max(200L, n_rows * 60L + 80L)
    plotlyOutput("plot_ci_diagram", height=paste0(h_px, "px"))
  })
  
  # ── Download handlers ─────────────────────────────────────────────────────────
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
      lbl <- if (isTRUE(input$endpoint=="safety")) "Expected performance (event proportion)"
      else "Expected performance (success proportion)"
      p <- ggplot(df, aes(x=x,y=y)) +
        geom_line(colour=pc, linewidth=1.1) + geom_point(colour=pc, size=2) +
        labs(title="Expected performance vs required sample size", x=lbl, y="Required n") +
        plot_theme_large
      ggsave(file, p, width=7, height=4, dpi=150, bg="white")
    }
  )
  
}