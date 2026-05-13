server <- function(input, output, session) {
  
  # -- Reload app button -------------------------------------------------------
  observeEvent(input$btn_reload_app, {
    session$reload()
  })
  
  # -- Accordion builder helper -----------------------------------------------
  acc_panel <- function(id, heading, open = FALSE, ...) {
    body_class <- if (open) "pgp-accordion-body open" else "pgp-accordion-body"
    hdr_class  <- if (open) "pgp-accordion-header open" else "pgp-accordion-header"
    tags$div(
      class = "pgp-accordion",
      tags$div(
        class = hdr_class,
        tags$span(heading),
        tags$span("\u25be", class = "pgp-accordion-chevron")
      ),
      tags$div(class = body_class, ...)
    )
  }
  
  # -- Endpoint presets -------------------------------------------------------
  endpoint_defaults <- list(
    efficacy = list(p0 = 0.88, p1 = 0.93, delta = 0.05, window = "0.05"),
    safety   = list(p0 = 0.95, p1 = 0.97, delta = 0.02, window = "0.02")
  )
  
  observeEvent(input$endpoint, {
    d <- endpoint_defaults[[input$endpoint]]
    if (is.null(d)) return()
    updateSliderInput(session, "p0.expected",  value    = d$p0)
    updateSliderInput(session, "p1.expected",  value    = d$p1)
    updateSliderInput(session, "p1.tolerable", value    = d$delta)
    updateSelectInput(session, "WindowMargin", selected = d$window)
  }, ignoreInit = TRUE)
  
  # -- Label updates when design changes -------------------------------------
  observeEvent(input$prop_design, {
    is_one <- isTRUE(input$prop_design == "one_arm")
    
    updateSliderInput(session, "p0.expected", label = if (is_one)
      "Benchmark / performance goal (p\u2080):" else "Control event rate (p\u2080):")
    
    updateSliderInput(session, "p1.expected", label = if (is_one)
      "Expected device event rate (p\u2081):" else "Expected experimental event rate (p\u2081):")
    
    updateSliderInput(session, "p1.tolerable", label = if (is_one)
      "Non-inferiority margin (\u0394, vs benchmark):" else "Non-inferiority margin (\u0394, risk difference):")
  }, ignoreInit = FALSE)
  
  # ── Calculator: core N dispatcher ────────────────────────────────────────────
  # prop_total_n() routes to the correct function based on design and CI method.
  # delta == 0 triggers pure-superiority formula (no NI margin).
  
  # -- Core N function -------------------------------------------------------
  n_1arm_z_superiority <- function(p0, p1, alpha, power) {
    if (p1 <= p0) return(Inf)
    z_alpha <- qnorm(1 - alpha); z_beta <- qnorm(power)
    ceiling((z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1)))^2 /
              (p1 - p0)^2)
  }
  
  prop_total_n <- function(p0, p1, delta,
                           ci_method = input$ci_method_prop,
                           sim_n     = as.numeric(input$sim_quality),
                           seed      = as.numeric(input$sim_seed)) {
    alpha <- as.numeric(input$sig.level)
    r_val <- if (isTRUE(input$prop_design == "one_arm")) 1 else as.numeric(input$r)
    
    if (isTRUE(input$prop_design == "one_arm")) {
      if (isTRUE(ci_method == "z_power")) {
        if (isTRUE(all.equal(delta, 0)))
          return(n_1arm_z_superiority(p0, p1, alpha, input$power))
        return(total_sample_size_prop_1arm(p0, p1, delta, alpha, input$power))
      }
      return(total_sample_size_prop_ci_power_1arm(
        p0, p1, delta, alpha, input$power,
        ci_method = ci_method, nsim = sim_n, seed = seed
      ))
    }
    
    if (isTRUE(ci_method == "z_power"))
      return(total_sample_size_prop(p0, p1, delta, alpha, input$power, r = r_val))
    
    total_sample_size_prop_ci_power(
      p0, p1, delta, alpha, input$power,
      r = r_val, ci_method = ci_method, nsim = sim_n, seed = seed
    )
  }
  
  # ── Calculator: reactives ────────────────────────────────────────────────────
  
  # -- Reactives -------------------------------------------------------------
  prop_df_delta <- reactive({
    req(input$p0.expected, input$p1.expected, input$p1.tolerable,
        input$power, input$sig.level, input$WindowMargin)
    
    window <- as.numeric(input$WindowMargin)
    x <- seq(from = max(0.001, input$p1.tolerable - window),
             to   = min(0.200, input$p1.tolerable + window),
             by   = 0.005)
    y <- sapply(x, function(d) prop_total_n(input$p0.expected, input$p1.expected, d))
    data.frame(x = x, y = y)
  })
  
  prop_df_p1 <- reactive({
    req(input$p0.expected, input$p1.expected, input$p1.tolerable,
        input$power, input$sig.level)
    
    x <- seq(from = max(0.001, input$p1.expected - 0.10),
             to   = min(0.999, input$p1.expected + 0.10),
             by   = 0.005)
    y <- sapply(x, function(p1i) prop_total_n(input$p0.expected, p1i, input$p1.tolerable))
    data.frame(x = x, y = y)
  })
  
  prop_n_at_delta <- reactive({
    prop_total_n(input$p0.expected, input$p1.expected, input$p1.tolerable)
  })
  
  # ── Calculator: CI comparison table ──────────────────────────────────────────
  
  # -- CI comparison reactive ------------------------------------------------
  compare_methods <- c(
    "Wilson"        = "wilson",
    "Exact (C-P)"   = "exact",
    "Agresti-Coull" = "ac",
    "Wald"          = "asymptotic"
  )
  
  compare_df <- reactive({
    req(input$showCompare)
    if (!isTRUE(input$showCompare)) return(NULL)
    sim_n <- as.numeric(input$sim_quality)
    seed  <- as.numeric(input$sim_seed)
    ns <- vapply(compare_methods, function(m) {
      prop_total_n(input$p0.expected, input$p1.expected, input$p1.tolerable,
                   ci_method = m, sim_n = sim_n, seed = seed)
    }, numeric(1))
    data.frame(
      Method   = names(compare_methods),
      `Total N` = ifelse(is.infinite(ns), "Not achievable", format(ns, big.mark = ",")),
      check.names = FALSE
    )
  })
  
  # ── Calculator: outputs ──────────────────────────────────────────────────────
  
  # -- N box -----------------------------------------------------------------
  output$n_box_prop <- renderUI({
    show_box <- if (is.null(input$showNBox_prop)) TRUE else isTRUE(input$showNBox_prop)
    if (!show_box) return(NULL)
    
    n_out <- prop_n_at_delta()
    method_label <- if (isTRUE(input$ci_method_prop == "z_power"))
      "Z (power formula)" else paste0("CI + simulation (", input$ci_method_prop, ")")
    
    msg <- if (is.infinite(n_out))
      paste0("Not achievable (N = \u221e) under current assumptions.  [", method_label, "]")
    else
      paste0("Total n at \u0394 = ", sprintf("%.3f", input$p1.tolerable),
             "  \u2192  n = ", format(n_out, big.mark = ","),
             "  [", method_label, "]")
    
    box_ui("Required sample size", msg)
  })
  
  # -- Shared plotly finishing touch -----------------------------------------
  finish_plotly <- function(p) {
    ggplotly(p) %>%
      layout(
        hovermode     = "x unified",
        paper_bgcolor = "transparent",
        plot_bgcolor  = "white",
        font          = list(family = "DM Sans, sans-serif")
      ) %>%
      config(displaylogo = FALSE, displayModeBar = FALSE)
  }
  
  # -- Delta plot ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    df <- prop_df_delta()
    
    validate(need(
      !all(is.infinite(df$y)),
      "No achievable sample size across this \u0394 range. Try widening \u0394 or adjusting p\u2080 / p\u2081."
    ))
    
    chosen_delta <- input$p1.tolerable
    chosen_n     <- prop_total_n(input$p0.expected, input$p1.expected, chosen_delta)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(colour = "#18bdb9", linewidth = 1.1) +
      geom_point(colour = "#18bdb9", size = 2) +
      labs(
        title = "NI margin (\u0394) vs total sample size",
        x = "Non-inferiority margin (\u0394)",
        y = "Total sample size (n)"
      ) +
      plot_theme_large
    
    if (isTRUE(input$showVline))
      p <- p + geom_vline(xintercept = chosen_delta, linetype = "dashed",
                          colour = "#e07b39", linewidth = 0.9)
    
    if (isTRUE(input$showVline) && !is.infinite(chosen_n))
      p <- p + geom_point(
        data = data.frame(x = chosen_delta, y = chosen_n),
        aes(x = x, y = y), colour = "#e07b39", size = 5, shape = 21,
        fill = "#e07b39", alpha = 0.85
      )
    
    finish_plotly(p)
  })
  
  # -- p1 plot ---------------------------------------------------------------
  output$plot2 <- renderPlotly({
    df    <- prop_df_p1()
    is_one <- isTRUE(input$prop_design == "one_arm")
    
    validate(need(
      !all(is.infinite(df$y)),
      "No achievable sample size across this p\u2081 range under current assumptions."
    ))
    
    x_lab <- if (is_one)
      "Expected device event rate (p\u2081)" else "Expected experimental event rate (p\u2081)"
    
    chosen_p1 <- input$p1.expected
    chosen_n  <- prop_total_n(input$p0.expected, chosen_p1, input$p1.tolerable)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(colour = "#18bdb9", linewidth = 1.1) +
      geom_point(colour = "#18bdb9", size = 2) +
      labs(
        title = "Expected event rate (p\u2081) vs total sample size",
        x = x_lab,
        y = "Total sample size (n)"
      ) +
      plot_theme_large
    
    if (isTRUE(input$showVline))
      p <- p + geom_vline(xintercept = chosen_p1, linetype = "dashed",
                          colour = "#e07b39", linewidth = 0.9)
    
    if (isTRUE(input$showVline) && !is.infinite(chosen_n))
      p <- p + geom_point(
        data = data.frame(x = chosen_p1, y = chosen_n),
        aes(x = x, y = y), colour = "#e07b39", size = 5, shape = 21,
        fill = "#e07b39", alpha = 0.85
      )
    
    finish_plotly(p)
  })
  
  # -- CI comparison section -------------------------------------------------
  output$compare_section <- renderUI({
    if (!isTRUE(input$showCompare)) return(NULL)
    df <- compare_df()
    if (is.null(df)) return(NULL)
    
    tagList(
      tags$hr(),
      tags$div(
        class = "compare-header",
        tags$h5("CI method comparison \u2014 required total N at current inputs"),
        tags$p(
          class = "compare-params",
          paste0("p\u2080 = ", input$p0.expected, "  |  p\u2081 = ", input$p1.expected,
                 "  |  \u0394 = ", input$p1.tolerable, "  |  \u03b1 = ", input$sig.level,
                 "  |  power = ", input$power)
        )
      ),
      DT::renderDT(
        DT::datatable(
          df, rownames = FALSE, class = "stripe hover compact",
          options = list(dom = "t", paging = FALSE, searching = FALSE)
        )
      )
    )
  })
  
  # -- DT tables -------------------------------------------------------------
  output$dataTable <- renderDT({
    if (!isTRUE(input$showTable)) return(NULL)
    df <- prop_df_delta()
    colnames(df) <- c("NI Margin (\u0394)", "Total N")
    DT::datatable(df, rownames = FALSE, class = "stripe hover compact",
                  options = list(pageLength = 15))
  })
  
  output$dataTable2 <- renderDT({
    if (!isTRUE(input$showTable2)) return(NULL)
    df <- prop_df_p1()
    colnames(df) <- c("Expected Event Rate (p\u2081)", "Total N")
    DT::datatable(df, rownames = FALSE, class = "stripe hover compact",
                  options = list(pageLength = 15))
  })
  
  # ── Generate Report: UI outputs ──────────────────────────────────────────────
  
  # -- Live report contents list ---------------------------------------------
  output$report_contents_ui <- renderUI({
    # Helper for a regular tick/cross row
    make_li <- function(label, on) {
      if (on) {
        tags$li(tags$span("✓", class = "rc-tick"), tags$span(label))
      } else {
        tags$li(
          tags$span("–", class = "rc-cross"),
          tags$span(label, style = "color:#94a3b8;")
        )
      }
    }
    
    # Helper for a collapsible group (greyed header or expanded with sub-items)
    make_group <- function(heading, items) {
      any_on <- any(sapply(items, function(x) x$on))
      if (!any_on) {
        tags$li(
          tags$span("–", class = "rc-cross"),
          tags$span(heading, style = "color:#94a3b8;")
        )
      } else {
        tagList(
          tags$li(
            tags$span("✓", class = "rc-tick"),
            tags$span(heading, style = "font-weight:600;")
          ),
          tags$ul(
            style = "list-style:none; padding:0 0 0 20px; margin:0;",
            lapply(items, function(item) make_li(item$label, item$on))
          )
        )
      }
    }
    
    # General items (always shown individually)
    general_items <- list(
      list(label = "Results table",              on = isTRUE(input$rpt_results)),
      list(label = "Interpretation paragraph",   on = isTRUE(input$rpt_interp_inc)),
      list(label = "CI method comparison table", on = isTRUE(input$rpt_ci_compare)),
      list(label = "Definitions glossary",       on = isTRUE(input$rpt_definitions)),
      list(label = "Calculation code",           on = isTRUE(input$rpt_calc_code))
    )
    
    # Sensitivity group
    sens_items <- list(
      list(label = "Δ plot",   on = isTRUE(input$rpt_plot_delta)),
      list(label = "p₁ plot",  on = isTRUE(input$rpt_plot_p1)),
      list(label = "Δ table",  on = isTRUE(input$rpt_table_delta)),
      list(label = "p₁ table", on = isTRUE(input$rpt_table_p1))
    )
    
    # Interim group
    interim_items <- list(
      list(label = "Data summary",   on = isTRUE(input$rpt_interim_summ)),
      list(label = "Interpretation", on = isTRUE(input$rpt_interim_interp)),
      list(label = "CI comparison",  on = isTRUE(input$rpt_interim_ci)),
      list(label = "Position plot",  on = isTRUE(input$rpt_interim_plot))
    )
    
    tags$ul(
      class = "report-contents",
      lapply(general_items, function(item) make_li(item$label, item$on)),
      make_group("Sensitivity Analysis", sens_items),
      make_group("Interim Analysis", interim_items)
    )
  })
  
  # -- Download button UI ----------------------------------------------------
  output$report_download_ui <- renderUI({
    if (isTRUE(input$report_format == "pdf")) {
      downloadButton("downloadPDF", "\u2193 Download summary (.pdf)",
                     class = "btn-sm btn-outline-secondary pgp-btn report-dl-btn")
    } else {
      downloadButton("downloadWord", "\u2193 Download summary (.docx)",
                     class = "btn-sm btn-outline-secondary pgp-btn report-dl-btn")
    }
  })
  
  # ── Generate Report: data helpers ────────────────────────────────────────────
  
  # -- Shared helper: build report data --------------------------------------
  build_report_data <- function() {
    ci_method_used <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    n_val <- prop_total_n(
      input$p0.expected, input$p1.expected, input$p1.tolerable,
      ci_method = ci_method_used,
      sim_n     = if (is.null(input$sim_quality)) 1000 else as.numeric(input$sim_quality),
      seed      = if (is.null(input$sim_seed))    1    else as.numeric(input$sim_seed)
    )
    ci_labels <- c(
      z_power = "Z (Power Formula)", wilson = "Wilson Score Interval",
      exact = "Clopper-Pearson (Exact)", ac = "Agresti-Coull",
      asymptotic = "Asymptotic (Wald)", prop.test = "prop.test",
      bayes = "Bayes", logit = "Logit", cloglog = "Cloglog", probit = "Probit"
    )
    ci_label    <- unname(ci_labels[ci_method_used])
    if (is.na(ci_label)) ci_label <- ci_method_used
    p_thr       <- as.numeric(input$p0.expected) - as.numeric(input$p1.tolerable)
    z_alpha_c   <- qnorm(1 - as.numeric(input$sig.level))
    n_successes <- if (is.infinite(n_val)) NA_integer_ else
      ceiling(n_val * p_thr + z_alpha_c * sqrt(n_val * p_thr * (1 - p_thr)))
    n_dropout   <- if (is.infinite(n_val)) NA_integer_ else ceiling(n_val / 0.9)
    n_fmt  <- if (is.infinite(n_val)) "Not achievable" else format(n_val, big.mark = ",")
    ns_fmt <- if (is.na(n_successes)) "\u2014" else format(n_successes, big.mark = ",")
    nd_fmt <- if (is.na(n_dropout))   "\u2014" else format(n_dropout,   big.mark = ",")
    list(
      n_val = n_val, ci_label = ci_label, n_fmt = n_fmt, ns_fmt = ns_fmt,
      nd_fmt = nd_fmt, n_successes = n_successes, p_thr = p_thr
    )
  }
  
  get_section_order <- function() {
    c("results", "interp", "ci_compare", "definitions", "calc_code",
      "plot_delta", "plot_p1", "table_delta", "table_p1",
      "interim_summ", "interim_interp", "interim_ci", "interim_plot")
  }
  
  # -- Capture plots as PNG for report ---------------------------------------
  capture_plots <- function() {
    list(
      delta = tryCatch({
        df <- prop_df_delta()
        p <- ggplot(df, aes(x = x, y = y)) +
          geom_line(colour = "#18bdb9", linewidth = 1.1) +
          geom_point(colour = "#18bdb9", size = 2) +
          labs(title = "NI margin (\u0394) vs total sample size",
               x = "Non-inferiority margin (\u0394)", y = "Total sample size (n)") +
          plot_theme_large
        tmp <- tempfile(fileext = ".png")
        ggsave(tmp, p, width = 6, height = 3.5, dpi = 150, bg = "white")
        tmp
      }, error = function(e) NULL),
      p1 = tryCatch({
        df <- prop_df_p1()
        p <- ggplot(df, aes(x = x, y = y)) +
          geom_line(colour = "#18bdb9", linewidth = 1.1) +
          geom_point(colour = "#18bdb9", size = 2) +
          labs(title = "Expected event rate (p\u2081) vs total sample size",
               x = "Expected event rate (p\u2081)", y = "Total sample size (n)") +
          plot_theme_large
        tmp <- tempfile(fileext = ".png")
        ggsave(tmp, p, width = 6, height = 3.5, dpi = 150, bg = "white")
        tmp
      }, error = function(e) NULL)
    )
  }
  
  # -- Interpretation text builder -------------------------------------------
  build_interp_text <- function(rd) {
    n_val       <- rd$n_val
    n_fmt       <- rd$n_fmt
    ns_fmt      <- rd$ns_fmt
    nd_fmt      <- rd$nd_fmt
    
    template <- if (!is.null(input$rpt_interp_text) && nchar(trimws(input$rpt_interp_text)) > 0)
      input$rpt_interp_text
    else
      paste0(
        "A total of {n} evaluable patients are required to demonstrate, with ",
        "{power_pct}% power, that the device success rate exceeds the performance ",
        "goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. ",
        "Allowing for 10% dropout, the study should enrol {n_dropout} patients. ",
        "The study will be deemed successful if at least {n_successes} out of {n} ",
        "evaluable patients are free from a major adverse event at 12 months."
      )
    
    p0_pct  <- round(as.numeric(input$p0.expected) * 100)
    p1_pct  <- round(as.numeric(input$p1.expected) * 100)
    pwr_pct <- round(input$power * 100)
    
    txt <- template
    txt <- gsub("{n}",           n_fmt,                         txt, fixed = TRUE)
    txt <- gsub("{n_dropout}",   nd_fmt,                        txt, fixed = TRUE)
    txt <- gsub("{n_successes}", ns_fmt,                        txt, fixed = TRUE)
    txt <- gsub("{power_pct}",   as.character(pwr_pct),         txt, fixed = TRUE)
    txt <- gsub("{p0_pct}",      as.character(p0_pct),          txt, fixed = TRUE)
    txt <- gsub("{p1_pct}",      as.character(p1_pct),          txt, fixed = TRUE)
    txt <- gsub("{alpha}",       as.character(input$sig.level), txt, fixed = TRUE)
    txt <- gsub("{delta}",       sprintf("%.3f", input$p1.tolerable), txt, fixed = TRUE)
    txt <- gsub("{ci_method}",   rd$ci_label,                   txt, fixed = TRUE)
    txt
  }
  
  # -- Helper: sensitivity table HTML (for PDF) ------------------------------
  make_sens_table_html <- function(df, col_names, caption, blue, th_fn, td_fn) {
    df2 <- df
    # Replace Inf with em-dash in Total N column
    df2[[2]] <- ifelse(is.infinite(df2[[2]]) | is.na(df2[[2]]), "\u2014",
                       format(round(df2[[2]]), big.mark = ","))
    header_row <- paste(sapply(col_names, th_fn), collapse = "")
    data_rows  <- paste(apply(df2, 1, function(row) {
      paste0("<tr>", paste(sapply(row, td_fn), collapse = ""), "</tr>")
    }), collapse = "")
    paste0(
      "<p style='font-size:9px;color:#555;font-style:italic;margin:4px 0 6px;'>", caption, "</p>",
      "<table style='border-collapse:collapse;width:100%;'>",
      "<tr>", header_row, "</tr>",
      data_rows,
      "</table>"
    )
  }
  
  # ── Generate Report: downloads ───────────────────────────────────────────────
  
  # -- PDF summary download --------------------------------------------------
  output$downloadPDF <- downloadHandler(
    filename    = function() paste0("PGPower_Summary_", format(Sys.Date(), "%d_%b_%Y"), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      tryCatch({
        rd          <- build_report_data()
        n_val       <- rd$n_val
        ci_label    <- rd$ci_label
        n_fmt       <- rd$n_fmt
        ns_fmt      <- rd$ns_fmt
        nd_fmt      <- rd$nd_fmt
        n_successes <- rd$n_successes
        
        show_results  <- !isTRUE(input$rpt_results    == FALSE)
        show_interp   <- !isTRUE(input$rpt_interp_inc == FALSE) &&
          !is.infinite(n_val) && !is.na(n_successes)
        show_ci_cmp   <- !isTRUE(input$rpt_ci_compare == FALSE)
        show_defs     <- !isTRUE(input$rpt_definitions == FALSE)
        show_code     <- !isTRUE(input$rpt_calc_code   == FALSE)
        show_plot_delta   <- isTRUE(input$rpt_plot_delta)
        show_plot_p1      <- isTRUE(input$rpt_plot_p1)
        show_table_delta  <- isTRUE(input$rpt_table_delta)
        show_table_p1     <- isTRUE(input$rpt_table_p1)
        show_interim_summ <- isTRUE(input$rpt_interim_summ)
        show_interim_interp <- isTRUE(input$rpt_interim_interp)
        show_interim_ci   <- isTRUE(input$rpt_interim_ci)
        show_interim_plot <- isTRUE(input$rpt_interim_plot)
        section_order <- get_section_order()
        
        plot_files <- if (show_plot_delta || show_plot_p1) capture_plots() else list(delta = NULL, p1 = NULL)
        
        blue <- "#2E74B5"
        td <- function(v) paste0("<td style='padding:5px 9px;border:1px solid #dde3ea;font-size:9.5px;'>", v, "</td>")
        th <- function(v) paste0("<th style='padding:5px 9px;background:#eef3f8;border:1px solid #dde3ea;text-align:left;font-size:9.5px;'>", v, "</th>")
        h2s <- paste0("color:", blue, ";font-size:13px;margin:20px 0 6px;")
        
        # Results table
        results_html <- if (show_results) paste0(
          "<h2 style='", h2s, "'>Results</h2>",
          "<p style='font-size:9.5px;color:#444;'>",
          "H&#8320;: p &le; (p&#8320; &minus; &Delta;) &nbsp; vs. &nbsp; ",
          "H&#8321;: p &gt; (p&#8320; &minus; &Delta;) &nbsp;&mdash;&nbsp; One-Sided</p>",
          "<table style='border-collapse:collapse;width:100%;'>",
          "<tr>", th("Power"), th("n"), th("n-succ."), th("10% dropout"),
          th("p&#8320;"), th("p&#8321;"), th("&Delta;"), th("&alpha;"), th("CI Method"), "</tr>",
          "<tr>",
          td(format(input$power, nsmall = 2)), td(n_fmt), td(ns_fmt), td(nd_fmt),
          td(input$p0.expected), td(input$p1.expected),
          td(sprintf("%.3f", input$p1.tolerable)),
          td(input$sig.level), td(ci_label),
          "</tr></table><hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # Interpretation
        interp_text <- if (show_interp) build_interp_text(rd) else NULL
        interp_html <- if (show_interp) paste0(
          "<h2 style='", h2s, "'>Interpretation</h2>",
          "<p style='font-size:9.5px;line-height:1.7;'>", interp_text, "</p>",
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # CI method comparison
        all_methods <- c(
          "Z (Power)" = "z_power", "Wilson" = "wilson", "Exact (C-P)" = "exact",
          "Agresti-Coull" = "ac", "Wald" = "asymptotic", "prop.test" = "prop.test",
          "Bayes" = "bayes", "Logit" = "logit", "Cloglog" = "cloglog", "Probit" = "probit"
        )
        method_ns <- sapply(all_methods, function(m) {
          n <- prop_total_n(input$p0.expected, input$p1.expected, input$p1.tolerable,
                            ci_method = m, sim_n = 400, seed = 1)
          if (is.infinite(n)) "\u2014" else format(n, big.mark = ",")
        })
        ci_header_row <- paste(sapply(names(all_methods), th), collapse = "")
        ci_data_row   <- paste(sapply(unname(method_ns),  td), collapse = "")
        ci_html <- if (show_ci_cmp) paste0(
          "<h2 style='", h2s, "'>Sample size by CI method</h2>",
          "<p style='font-size:9px;color:#555;'>",
          "p&#8320; = ", input$p0.expected, " | p&#8321; = ", input$p1.expected,
          " | &Delta; = ", input$p1.tolerable, " | &alpha; = ", input$sig.level,
          " | Power = ", input$power, "</p>",
          "<table style='border-collapse:collapse;width:100%;'>",
          "<tr>", ci_header_row, "</tr>",
          "<tr>", ci_data_row,   "</tr>",
          "</table><hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # Definitions
        defs_list <- list(
          c("Power",               "Probability of correctly rejecting a false null hypothesis."),
          c("n",                   "Minimum number of evaluable patients required."),
          c("n-successes",         "Minimum number of successful outcomes required."),
          c("p&#8320; (PG)",       "The benchmark proportion the device must exceed."),
          c("p&#8321;",            "Anticipated true success rate of the device."),
          c("&Delta; (NI Margin)", "The maximum allowable shortfall below the performance goal."),
          c("&alpha;",             "Probability of a false positive result."),
          c("CI Method",           "Method used to estimate the confidence interval.")
        )
        defs_rows <- paste(sapply(defs_list, function(d) paste0(
          "<tr>",
          "<td style='padding:4px 9px;font-weight:600;border:1px solid #dde3ea;font-size:9px;white-space:nowrap;'>", d[1], "</td>",
          "<td style='padding:4px 9px;border:1px solid #dde3ea;font-size:9px;'>", d[2], "</td>",
          "</tr>"
        )), collapse = "")
        defs_html <- if (show_defs) paste0(
          "<h2 style='", h2s, "'>Definitions</h2>",
          "<table style='border-collapse:collapse;width:100%;'>", defs_rows, "</table>",
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # Calculation code
        code_lines <- c(
          "# One-arm performance goal test",
          "# H0: p &lt;= p0 - delta   vs   H1: p &gt; p0 - delta",
          "total_sample_size_prop_1arm &lt;- function(p0, p1, delta, sig.level, power) {",
          "&nbsp;&nbsp;p_thr   &lt;- p0 - delta",
          "&nbsp;&nbsp;z_alpha &lt;- qnorm(1 - sig.level)",
          "&nbsp;&nbsp;z_beta  &lt;- qnorm(power)",
          "&nbsp;&nbsp;ceiling(",
          "&nbsp;&nbsp;&nbsp;&nbsp;(z_alpha * sqrt(p_thr * (1 - p_thr)) +",
          "&nbsp;&nbsp;&nbsp;&nbsp; z_beta  * sqrt(p1   * (1 - p1  )))^2 /",
          "&nbsp;&nbsp;&nbsp;&nbsp;(p1 - p_thr)^2",
          "&nbsp;&nbsp;)",
          "}"
        )
        code_html <- if (show_code) paste0(
          "<h2 style='", h2s, "'>Calculation</h2>",
          "<pre style='background:#f1f5f9;border-radius:6px;padding:12px;",
          "font-family:\"Courier New\",monospace;font-size:8.5px;color:#1a1a2e;line-height:1.75;'>",
          paste(code_lines, collapse = "\n"),
          "</pre>"
        ) else ""
        
        # Sensitivity plots (separate)
        img_tag <- function(f, cap) {
          if (is.null(f) || !file.exists(f)) return("")
          b64 <- base64enc::base64encode(f)
          paste0(
            "<figure style='margin:12px 0;'>",
            "<img src='data:image/png;base64,", b64,
            "' style='width:100%;max-width:600px;'>",
            "<figcaption style='font-size:9px;color:#555;margin-top:4px;'>",
            cap, "</figcaption></figure>"
          )
        }
        plot_delta_html <- if (show_plot_delta) paste0(
          "<h2 style='", h2s, "'>Δ Sensitivity Plot</h2>",
          img_tag(plot_files$delta, "NI margin (Δ) vs total sample size (n)"),
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        plot_p1_html <- if (show_plot_p1) paste0(
          "<h2 style='", h2s, "'>p₁ Sensitivity Plot</h2>",
          img_tag(plot_files$p1, "Expected event rate (p₁) vs total sample size (n)"),
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # -- Sensitivity tables (separate, PDF) -------------------------------
        df_delta_pdf <- if (show_table_delta) tryCatch(prop_df_delta(), error = function(e) NULL) else NULL
        df_p1_pdf    <- if (show_table_p1)    tryCatch(prop_df_p1(),    error = function(e) NULL) else NULL
        
        table_delta_html <- if (show_table_delta && !is.null(df_delta_pdf)) paste0(
          "<h2 style='", h2s, "'>Δ Sensitivity Table</h2>",
          make_sens_table_html(
            df_delta_pdf,
            col_names = c("NI Margin (Δ)", "Total N"),
            caption   = paste0("p₀ = ", input$p0.expected, ", p₁ = ", input$p1.expected),
            blue = blue, th_fn = th, td_fn = td
          ),
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        table_p1_html <- if (show_table_p1 && !is.null(df_p1_pdf)) paste0(
          "<h2 style='", h2s, "'>p₁ Sensitivity Table</h2>",
          make_sens_table_html(
            df_p1_pdf,
            col_names = c("Expected Event Rate (p₁)", "Total N"),
            caption   = paste0("p₀ = ", input$p0.expected, ", Δ = ", input$p1.tolerable),
            blue = blue, th_fn = th, td_fn = td
          ),
          "<hr style='border-color:", blue, ";margin:14px 0;'>"
        ) else ""
        
        # -- Interim analysis sections (PDF) ----------------------------------
        # Helper: build interim values once if any interim section needed
        iv_pdf <- if (show_interim_summ || show_interim_interp || show_interim_ci)
          tryCatch(interim_vals(), error = function(e) NULL)
        else NULL
        
        # 1. Data summary table
        interim_summ_html <- if (show_interim_summ && !is.null(iv_pdf)) {
          iv <- iv_pdf
          status_col_hex <- if (iv$status == "On Track") "#18bdb9" else "#c0392b"
          ep_txt   <- if (iv$is_safety) "Safety (lower is better)" else "Efficacy (higher is better)"
          des_txt  <- if (iv$is_two_arm) "Two-arm (risk difference)" else "Single-arm (vs benchmark)"
          ci_pct   <- round(iv$conf_level * 100, 1)
          dec_bnd  <- if (iv$is_safety) round(iv$ci_hi, 3) else round(iv$ci_lo, 3)
          bnd_lbl  <- if (iv$is_safety) "CI upper" else "CI lower"
          dir_word <- if (iv$is_safety) "below" else "above"
          enrol_txt <- if (!is.infinite(iv$n_planned))
            paste0(iv$n, " of planned ", format(iv$n_planned, big.mark = ","),
                   " (", round(100 * iv$n / iv$n_planned, 1), "%)")
          else as.character(iv$n)
          rows_i <- list(
            c("Design",           des_txt),
            c("Endpoint",         ep_txt),
            c("Enrolled (n)",     as.character(iv$n)),
            c("Events observed",  as.character(iv$x)),
            c("Observed estimate",as.character(round(iv$p_hat, 4))),
            c(paste0(ci_pct, "% CI"),
              paste0("[", round(iv$ci_lo, 3), ",  ", round(iv$ci_hi, 3), "]")),
            c("NI boundary",      as.character(round(iv$boundary, 3))),
            c("Decision bound",   paste0(bnd_lbl, " = ", dec_bnd,
                                         " (must be ", dir_word, " ", round(iv$boundary, 3), ")")),
            c("Enrolment",        enrol_txt),
            c("Status",           iv$status)
          )
          hdr_row_i    <- paste0("<tr>", th("Parameter"), th("Value"), "</tr>")
          data_rows_i  <- paste(sapply(rows_i, function(r)
            paste0("<tr>", td(r[1]), td(r[2]), "</tr>")), collapse = "")
          paste0(
            "<h2 style='", h2s, "'>Interim Analysis \u2014 Data Summary</h2>",
            "<p style='font-size:9px;color:", status_col_hex,
            ";font-weight:700;margin:0 0 6px;'>Status: ", iv$status, "</p>",
            "<table style='border-collapse:collapse;width:100%;'>",
            hdr_row_i, data_rows_i, "</table>",
            "<hr style='border-color:", blue, ";margin:14px 0;'>"
          )
        } else ""
        
        # 2. Interpretation paragraph
        interim_interp_html <- if (show_interim_interp && !is.null(iv_pdf)) {
          iv <- iv_pdf
          ci_pct <- round(iv$conf_level * 100, 1)
          bnd_lbl  <- if (iv$is_safety) "upper" else "lower"
          dir_word <- if (iv$is_safety) "below" else "above"
          dec_bnd  <- if (iv$is_safety) round(iv$ci_hi, 3) else round(iv$ci_lo, 3)
          enrol_pct <- if (!is.infinite(iv$n_planned))
            paste0(round(100 * iv$n / iv$n_planned, 1), "% of the planned sample")
          else paste0("n = ", iv$n, " patients")
          ni_word  <- if (iv$status == "On Track") "has been formally demonstrated" else "has not yet been demonstrated"
          interp_txt <- paste0(
            "At this interim assessment, ", enrol_pct, " has been enrolled (n = ", iv$n, "). ",
            "The observed ", if (iv$is_two_arm) "risk difference" else "event rate",
            " is ", round(iv$p_hat, 3),
            " (", ci_pct, "% CI: ", round(iv$ci_lo, 3), " \u2013 ", round(iv$ci_hi, 3), "). ",
            "The non-inferiority boundary is ", round(iv$boundary, 3), ". ",
            "Non-inferiority ", ni_word, ": the CI ", bnd_lbl, " bound (",
            dec_bnd, ") is ", if (iv$status == "On Track") "" else "not yet ",
            dir_word, " the NI boundary (", round(iv$boundary, 3), ")."
          )
          paste0(
            "<h2 style='", h2s, "'>Interim Analysis \u2014 Interpretation</h2>",
            "<p style='font-size:9.5px;line-height:1.7;'>", interp_txt, "</p>",
            "<hr style='border-color:", blue, ";margin:14px 0;'>"
          )
        } else ""
        
        # 3. CI method comparison table (single-arm only)
        interim_ci_html <- if (show_interim_ci && !is.null(iv_pdf) && !iv_pdf$is_two_arm) {
          iv <- iv_pdf
          ci_methods_i <- c("Wilson"="wilson","Exact (C-P)"="exact",
                            "Agresti-Coull"="ac","Wald"="asymptotic",
                            "Logit"="logit","Bayes"="bayes")
          thresh_rows <- lapply(names(ci_methods_i), function(m_lbl) {
            m_key <- ci_methods_i[[m_lbl]]
            thr <- tryCatch(
              interim_x_threshold(iv$n, iv$boundary, iv$conf_level, m_key, iv$is_safety),
              error = function(e) NA_integer_)
            rate <- if (is.na(thr)) "\u2014"
            else paste0(thr, " / ", iv$n, " = ", round(thr/iv$n, 3))
            pass <- if (is.na(thr)) "\u2014"
            else if (iv$is_safety) {
              if (iv$x <= thr) "\u2713" else "\u2717"
            } else {
              if (iv$x >= thr) "\u2713" else "\u2717"
            }
            paste0("<tr>", td(m_lbl), td(if(is.na(thr)) "\u2014" else as.character(thr)),
                   td(rate), td(pass), "</tr>")
          })
          hdr_ci <- paste0("<tr>", th("CI Method"),
                           th(if(iv$is_safety) "Max events" else "Min events"),
                           th("Rate"), th(paste0("x=",iv$x," passes?")), "</tr>")
          col_hdr <- if (iv$is_safety)
            paste0("Max events (x) to keep CI upper < ", round(iv$boundary,3))
          else
            paste0("Min events (x) for CI lower > ", round(iv$boundary,3))
          paste0(
            "<h2 style='", h2s, "'>Interim Analysis \u2014 CI Method Comparison</h2>",
            "<p style='font-size:9px;color:#555;'>n = ", iv$n,
            "  |  boundary = ", round(iv$boundary,3),
            "  |  ", round(iv$conf_level*100,1), "% CI</p>",
            "<table style='border-collapse:collapse;width:100%;'>",
            hdr_ci, paste(thresh_rows, collapse=""), "</table>",
            "<hr style='border-color:", blue, ";margin:14px 0;'>"
          )
        } else if (show_interim_ci && !is.null(iv_pdf) && iv_pdf$is_two_arm) {
          paste0("<h2 style='", h2s, "'>Interim Analysis \u2014 CI Method Comparison</h2>",
                 "<p style='font-size:9.5px;color:#94a3b8;'>",
                 "CI method comparison is available for single-arm designs only.</p>",
                 "<hr style='border-color:", blue, ";margin:14px 0;'>")
        } else ""
        
        # -- Interim position plot (PDF) --------------------------------------
        interim_plot_html <- if (show_interim_plot) {
          iv_plot <- if (!is.null(iv_pdf)) iv_pdf
          else tryCatch(interim_vals(), error = function(e) NULL)
          if (is.null(iv_plot)) "" else {
            # Capture the interim position plot as a PNG
            tryCatch({
              iv <- iv_plot
              if (iv$is_two_arm) {
                xlo <- min(iv$boundary - 0.15, iv$ci_lo - 0.05, -0.25)
                xhi <- max(iv$boundary + 0.15, iv$ci_hi + 0.05,  0.25)
              } else {
                xlo <- max(0, min(iv$boundary - 0.15, iv$ci_lo - 0.05))
                xhi <- min(1, max(iv$p0 + 0.10, iv$ci_hi + 0.05))
              }
              good_lo <- if (iv$is_safety) xlo        else iv$boundary
              good_hi <- if (iv$is_safety) iv$boundary else xhi
              y_label <- if (iv$is_two_arm) "Difference" else "Current rate"
              x_label <- if (iv$is_two_arm) "Risk difference" else "Proportion"
              df_pt   <- data.frame(label = y_label, est = iv$p_hat,
                                    lo = iv$ci_lo, hi = iv$ci_hi)
              p_interim <- ggplot(df_pt, aes(x = est, y = label)) +
                annotate("rect", xmin = good_lo, xmax = good_hi,
                         ymin = -Inf, ymax = Inf, fill = "#18bdb9", alpha = 0.08) +
                geom_vline(xintercept = iv$boundary, linetype = "dashed",
                           colour = "#e07b39", linewidth = 0.9) +
                geom_vline(xintercept = if (iv$is_two_arm) 0 else iv$p0,
                           linetype = "dashed", colour = "#718096", linewidth = 0.7) +
                geom_errorbar(aes(xmin = lo, xmax = hi, y = label),
                              width = 0.15, colour = "#1a2e35", linewidth = 0.9,
                              orientation = "y") +
                geom_point(size = 4, colour = "#18bdb9") +
                scale_x_continuous(limits = c(xlo, xhi)) +
                labs(title = if (iv$is_two_arm)
                  "Observed risk difference vs NI boundary"
                  else
                    "Current observed rate vs NI boundary",
                  x = x_label, y = NULL) +
                plot_theme_large +
                theme(axis.text.y = element_text(size = 12))
              tmp_plot <- tempfile(fileext = ".png")
              ggsave(tmp_plot, p_interim, width = 5.5, height = 2.5, dpi = 150, bg = "white")
              b64 <- base64enc::base64encode(tmp_plot)
              unlink(tmp_plot)
              paste0(
                "<h2 style='", h2s, "'>Interim Analysis — Position Plot</h2>",
                "<figure style='margin:10px 0;'>",
                "<img src='data:image/png;base64,", b64,
                "' style='width:100%;max-width:560px;'>",
                "<figcaption style='font-size:9px;color:#555;margin-top:4px;'>",
                "Observed estimate (point) with CI (bars) vs NI boundary (orange dashed)",
                "</figcaption></figure>",
                "<hr style='border-color:", blue, ";margin:14px 0;'>"
              )
            }, error = function(e) "")
          }
        } else ""
        
        section_html_map <- list(
          results        = results_html,
          interp         = interp_html,
          ci_compare     = ci_html,
          definitions    = defs_html,
          calc_code      = code_html,
          plot_delta     = plot_delta_html,
          plot_p1        = plot_p1_html,
          table_delta    = table_delta_html,
          table_p1       = table_p1_html,
          interim_summ   = interim_summ_html,
          interim_interp = interim_interp_html,
          interim_ci     = interim_ci_html,
          interim_plot   = interim_plot_html
        )
        body_html <- paste(sapply(section_order, function(s) {
          h <- section_html_map[[s]]
          if (is.null(h)) "" else h
        }), collapse = "")
        
        rpt_title_val <- if (!is.null(input$rpt_title) && nchar(trimws(input$rpt_title)) > 0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        author_name <- if (!is.null(input$rpt_author_name) &&
                           isTRUE(input$rpt_include_author) &&
                           nchar(trimws(input$rpt_author_name)) > 0)
          trimws(input$rpt_author_name) else NULL
        sub_parts <- c(
          if (!is.null(author_name))
            paste0("Author: ", author_name) else NULL,
          if (isTRUE(input$rpt_include_date   != FALSE))
            format(Sys.Date(), "%d %B %Y") else NULL,
          if (isTRUE(input$rpt_include_method != FALSE))
            paste0("Method: ", ci_label) else NULL
        )
        sub_line <- if (length(sub_parts) > 0)
          paste0("<p class='sub'>", paste(sub_parts, collapse = " &nbsp;|&nbsp; "), "</p>")
        else ""
        
        html_out <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
          "<style>",
          "body{font-family:'Helvetica Neue',Arial,sans-serif;margin:40px;color:#1a2e35;",
          "font-size:10px;line-height:1.5;}",
          "h1{color:", blue, ";font-size:17px;margin:0 0 4px;}",
          ".sub{font-size:9px;color:#666;margin:0 0 10px;}",
          "hr.top{border:none;border-top:2px solid ", blue, ";margin:10px 0 16px;}",
          "@page{size:A4;margin:18mm 18mm 22mm 18mm;}","@page{@bottom-center{content:counter(page) ' / ' counter(pages);","font-family:'Helvetica Neue',Arial,sans-serif;font-size:8px;color:#94a3b8;}}",
          "</style></head><body>",
          "<h1>", rpt_title_val, "</h1>",
          sub_line,
          "<hr class='top'>",
          body_html,
          "<p style='margin-top:32px;font-size:8.5px;color:#94a3b8;font-style:italic;border-top:1px solid #e2e8f0;padding-top:10px;'>This report was generated by PG-Power.</p>",
          "</body></html>"
        )
        
        tmp_html <- tempfile(fileext = ".html")
        writeLines(html_out, con = tmp_html, useBytes = FALSE)
        
        if (requireNamespace("pagedown", quietly = TRUE)) {
          pagedown::chrome_print(tmp_html, output = file, wait = 15)
          unlink(tmp_html)
        } else if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(tmp_html, file = file, vwidth = 794, vheight = 1123)
          unlink(tmp_html)
        } else {
          # No PDF engine available — serve as a clean, printable HTML file
          # (filename function above already returns .html in this case)
          file.copy(tmp_html, file, overwrite = TRUE)
          unlink(tmp_html)
          showNotification(
            paste0("PDF engine not found. Downloaded as HTML instead. ",
                   "Open the file in a browser and use Ctrl+P / Cmd+P -> Save as PDF. ",
                   "To enable direct PDF: install.packages('pagedown')"),
            type = "warning", duration = 12
          )
        }
        
      }, error = function(e) {
        message("downloadPDF ERROR: ", conditionMessage(e))
      })
    }
  )
  
  # -- Downloads (CSV tables) ------------------------------------------------
  output$downloadData_plot1 <- downloadHandler(
    filename = function() paste0("PGPower_NI_margin_table_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- prop_df_delta()
      colnames(df) <- c("NI Margin (Delta)", "Total N")
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$downloadData_plot2 <- downloadHandler(
    filename = function() paste0("PGPower_p1_table_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- prop_df_p1()
      colnames(df) <- c("Expected Event Rate (p1)", "Total N")
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # -- Word summary download -------------------------------------------------
  output$downloadWord <- downloadHandler(
    filename    = function() paste0("PGPower_Summary_", format(Sys.Date(), "%d_%b_%Y"), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content     = function(file) {
      tryCatch({
        if (!requireNamespace("officer", quietly = TRUE))
          stop("Please install the 'officer' package: install.packages('officer')")
        
        rd          <- build_report_data()
        n_val       <- rd$n_val
        ci_label    <- rd$ci_label
        n_fmt       <- rd$n_fmt
        ns_fmt      <- rd$ns_fmt
        nd_fmt      <- rd$nd_fmt
        n_successes <- rd$n_successes
        
        show_plot_delta_w   <- isTRUE(input$rpt_plot_delta)
        show_plot_p1_w      <- isTRUE(input$rpt_plot_p1)
        show_table_delta_w  <- isTRUE(input$rpt_table_delta)
        show_table_p1_w     <- isTRUE(input$rpt_table_p1)
        show_interim_summ_w <- isTRUE(input$rpt_interim_summ)
        show_interim_interp_w <- isTRUE(input$rpt_interim_interp)
        show_interim_ci_w   <- isTRUE(input$rpt_interim_ci)
        show_interim_plot_w <- isTRUE(input$rpt_interim_plot)
        plot_files_w <- if (show_plot_delta_w || show_plot_p1_w)
          capture_plots() else list(delta = NULL, p1 = NULL)
        
        summary_df <- data.frame(
          "Power"       = format(input$power, nsmall = 2),
          "n"           = n_fmt,
          "n-successes" = ns_fmt,
          "10% dropout" = nd_fmt,
          "p0"          = format(input$p0.expected, nsmall = 2),
          "p1"          = format(input$p1.expected, nsmall = 2),
          "Delta"       = sprintf("%.3f", input$p1.tolerable),
          "Alpha"       = format(as.numeric(input$sig.level)),
          "CI Method"   = ci_label,
          check.names      = FALSE,
          stringsAsFactors = FALSE
        )
        colnames(summary_df) <- c("Power", "n", "n-successes", "10% dropout",
                                  "p\u2080", "p\u2081", "\u0394", "\u03b1", "CI Method")
        
        blue_col  <- "#2E74B5"
        title_fmt <- officer::fp_text(bold = TRUE,  font.size = 12, color = blue_col)
        h2_fmt    <- officer::fp_text(bold = FALSE, font.size = 10, color = blue_col)
        sub_fmt   <- officer::fp_text(font.size = 8,  color = "#444444")
        body_fmt  <- officer::fp_text(font.size = 9)
        mono_fmt  <- officer::fp_text(font.family = "Courier New", font.size = 8, color = "#1a1a2e")
        def_term  <- officer::fp_text(bold = TRUE,  font.size = 9)
        def_body  <- officer::fp_text(bold = FALSE, font.size = 9)
        hyp_fmt   <- officer::fp_text(font.size = 9, color = "#444444")
        tight_p   <- officer::fp_par(text.align = "left")
        border_p  <- officer::fp_par(
          text.align = "left",
          border.bottom = officer::fp_border(color = blue_col, width = 1)
        )
        
        doc <- officer::read_docx()
        
        # Title block
        word_title <- if (!is.null(input$rpt_title) && nchar(trimws(input$rpt_title)) > 0)
          input$rpt_title else "PG-Power \u2014 Sample Size Report"
        author_name_w <- if (!is.null(input$rpt_author_name) &&
                             isTRUE(input$rpt_include_author) &&
                             nchar(trimws(input$rpt_author_name)) > 0)
          trimws(input$rpt_author_name) else NULL
        sub_word_parts <- c(
          if (!is.null(author_name_w))
            paste0("Author: ", author_name_w) else NULL,
          if (isTRUE(input$rpt_include_date   != FALSE))
            format(Sys.Date(), "%d %B %Y") else NULL,
          if (isTRUE(input$rpt_include_method != FALSE))
            paste0("Method: ", ci_label) else NULL
        )
        sub_word <- paste(sub_word_parts, collapse = "  |  ")
        
        doc <- officer::body_add_fpar(doc,
                                      officer::fpar(officer::ftext(word_title, title_fmt), fp_p = tight_p))
        if (nchar(sub_word) > 0)
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext(sub_word, sub_fmt), fp_p = tight_p))
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
        doc <- officer::body_add_par(doc, "", style = "Normal")
        
        # Results
        if (!isTRUE(input$rpt_results == FALSE)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Results", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(
                                          officer::ftext("H\u2080: p \u2264 (p\u2080 \u2212 \u0394)  vs.  H\u2081: p > (p\u2080 \u2212 \u0394)  \u2014  One-Sided",
                                                         hyp_fmt),
                                          fp_p = tight_p))
          doc <- officer::body_add_table(doc, summary_df, align_table = "left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # Interpretation
        if (!is.infinite(n_val) && !is.na(n_successes) &&
            !isTRUE(input$rpt_interp_inc == FALSE)) {
          interp <- build_interp_text(rd)
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Interpretation", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext(interp, body_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # CI method comparison
        if (!isTRUE(input$rpt_ci_compare == FALSE)) {
          all_methods <- c(
            "Z (Power)"     = "z_power",
            "Wilson"        = "wilson",
            "Exact (C-P)"   = "exact",
            "Agresti-Coull" = "ac",
            "Wald"          = "asymptotic",
            "prop.test"     = "prop.test",
            "Bayes"         = "bayes",
            "Logit"         = "logit",
            "Cloglog"       = "cloglog",
            "Probit"        = "probit"
          )
          method_ns <- sapply(all_methods, function(m) {
            n <- prop_total_n(
              input$p0.expected, input$p1.expected, input$p1.tolerable,
              ci_method = m, sim_n = 400, seed = 1
            )
            if (is.infinite(n)) "\u2014" else format(n, big.mark = ",")
          })
          ci_wide_df <- as.data.frame(matrix(unname(method_ns), nrow = 1),
                                      stringsAsFactors = FALSE)
          colnames(ci_wide_df) <- names(all_methods)
          
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Sample size by CI method", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(
                                          officer::ftext(
                                            paste0("p\u2080 = ", input$p0.expected,
                                                   "  |  p\u2081 = ", input$p1.expected,
                                                   "  |  \u0394 = ", input$p1.tolerable,
                                                   "  |  \u03b1 = ", input$sig.level,
                                                   "  |  Power = ", input$power),
                                            hyp_fmt),
                                          fp_p = tight_p))
          doc <- officer::body_add_table(doc, ci_wide_df, align_table = "left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # Definitions
        include_defs <- if (is.null(input$rpt_definitions)) TRUE else isTRUE(input$rpt_definitions)
        if (include_defs) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Definitions", h2_fmt), fp_p = tight_p))
          defs <- list(
            list("Power: ",                       "Probability of correctly rejecting a false null hypothesis."),
            list("n: ",                           "Minimum number of evaluable patients required."),
            list("n-successes: ",                 "Minimum number of successful outcomes required to meet the primary endpoint."),
            list("p\u2080: Performance Goal: ",        "The benchmark proportion the device must exceed."),
            list("p\u2081: ",                          "Anticipated true success rate of the device."),
            list("\u0394: Non-Inferiority Margin: ",   "The maximum allowable shortfall below the performance goal."),
            list("\u03b1: Significance Level: ",       "Probability of a false positive result."),
            list("CI Method: ",                   "Method used to estimate the confidence interval.")
          )
          for (d in defs)
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(
                                            officer::ftext(paste0("\u2022  ", d[[1]]), def_term),
                                            officer::ftext(d[[2]], def_body),
                                            fp_p = tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # Calculation code
        include_code <- if (is.null(input$rpt_calc_code)) TRUE else isTRUE(input$rpt_calc_code)
        if (include_code) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Calculation", h2_fmt), fp_p = tight_p))
          code_lines <- c(
            "# One-arm performance goal test",
            "# H0: p <= p0 - delta   vs   H1: p > p0 - delta",
            "total_sample_size_prop_1arm <- function(p0, p1, delta, sig.level, power) {",
            "  p_thr   <- p0 - delta",
            "  z_alpha <- qnorm(1 - sig.level)",
            "  z_beta  <- qnorm(power)",
            "  ceiling(",
            "    (z_alpha * sqrt(p_thr * (1 - p_thr)) +",
            "     z_beta  * sqrt(p1   * (1 - p1  )))^2 /",
            "    (p1 - p_thr)^2",
            "  )",
            "}"
          )
          for (line in code_lines)
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(line, mono_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # Sensitivity plots (Word, separate)
        if (show_plot_delta_w && !is.null(plot_files_w$delta) && file.exists(plot_files_w$delta)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Δ Sensitivity Plot", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_img(doc, src = plot_files_w$delta, width = 5.5, height = 3.2)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        if (show_plot_p1_w && !is.null(plot_files_w$p1) && file.exists(plot_files_w$p1)) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("p₁ Sensitivity Plot", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_img(doc, src = plot_files_w$p1, width = 5.5, height = 3.2)
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # -- Sensitivity tables (separate, Word) ------------------------------
        if (show_table_delta_w) {
          df_dw <- tryCatch(prop_df_delta(), error = function(e) NULL)
          if (!is.null(df_dw)) {
            df_dw[[2]] <- ifelse(is.infinite(df_dw[[2]]) | is.na(df_dw[[2]]),
                                 "—", format(round(df_dw[[2]]), big.mark = ","))
            df_dw[[1]] <- sprintf("%.3f", df_dw[[1]])
            colnames(df_dw) <- c("NI Margin (Δ)", "Total N")
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext("Δ Sensitivity Table", h2_fmt), fp_p = tight_p))
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(
                                            paste0("p₀ = ", input$p0.expected, ", p₁ = ", input$p1.expected),
                                            hyp_fmt), fp_p = tight_p))
            doc <- officer::body_add_table(doc, df_dw, align_table = "left")
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        }
        if (show_table_p1_w) {
          df_p1w <- tryCatch(prop_df_p1(), error = function(e) NULL)
          if (!is.null(df_p1w)) {
            df_p1w[[2]] <- ifelse(is.infinite(df_p1w[[2]]) | is.na(df_p1w[[2]]),
                                  "—", format(round(df_p1w[[2]]), big.mark = ","))
            df_p1w[[1]] <- sprintf("%.3f", df_p1w[[1]])
            colnames(df_p1w) <- c("Expected Event Rate (p₁)", "Total N")
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext("p₁ Sensitivity Table", h2_fmt), fp_p = tight_p))
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(
                                            paste0("p₀ = ", input$p0.expected, ", Δ = ", input$p1.tolerable),
                                            hyp_fmt), fp_p = tight_p))
            doc <- officer::body_add_table(doc, df_p1w, align_table = "left")
            doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        }
        
        # -- Interim analysis sections (Word) --------------------------------
        iv_word <- if (show_interim_summ_w || show_interim_interp_w || show_interim_ci_w)
          tryCatch(interim_vals(), error = function(e) NULL)
        else NULL
        
        # 1. Data summary table
        if (show_interim_summ_w && !is.null(iv_word)) {
          iw <- iv_word
          ep_w  <- if (iw$is_safety) "Safety (lower is better)" else "Efficacy (higher is better)"
          des_w <- if (iw$is_two_arm) "Two-arm (risk difference)" else "Single-arm (vs benchmark)"
          ci_w  <- round(iw$conf_level * 100, 1)
          enr_w <- if (!is.infinite(iw$n_planned))
            paste0(iw$n, " of planned ", format(iw$n_planned, big.mark=","),
                   " (", round(100*iw$n/iw$n_planned,1), "%)") else as.character(iw$n)
          int_df <- data.frame(
            Parameter = c("Design","Endpoint","Enrolled (n)","Events observed",
                          "Observed estimate", paste0(ci_w,"% CI"),
                          "NI boundary","Enrolment","Status"),
            Value = c(des_w, ep_w, as.character(iw$n), as.character(iw$x),
                      as.character(round(iw$p_hat,4)),
                      paste0("[",round(iw$ci_lo,3),",  ",round(iw$ci_hi,3),"]"),
                      as.character(round(iw$boundary,3)), enr_w, iw$status),
            stringsAsFactors = FALSE)
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Interim Analysis — Data Summary", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_table(doc, int_df, align_table = "left")
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # 2. Interpretation paragraph
        if (show_interim_interp_w && !is.null(iv_word)) {
          iw <- iv_word
          ci_w     <- round(iw$conf_level * 100, 1)
          bnd_lbl  <- if (iw$is_safety) "upper" else "lower"
          dir_word <- if (iw$is_safety) "below" else "above"
          dec_bnd  <- if (iw$is_safety) round(iw$ci_hi,3) else round(iw$ci_lo,3)
          enrol_pct <- if (!is.infinite(iw$n_planned))
            paste0(round(100*iw$n/iw$n_planned,1), "% of the planned sample")
          else paste0("n = ", iw$n, " patients")
          ni_word <- if (iw$status == "On Track") "has been formally demonstrated"
          else "has not yet been demonstrated"
          interp_w <- paste0(
            "At this interim assessment, ", enrol_pct, " has been enrolled (n = ", iw$n, "). ",
            "The observed ", if (iw$is_two_arm) "risk difference" else "event rate",
            " is ", round(iw$p_hat, 3),
            " (", ci_w, "% CI: ", round(iw$ci_lo,3), " – ", round(iw$ci_hi,3), "). ",
            "The non-inferiority boundary is ", round(iw$boundary,3), ". ",
            "Non-inferiority ", ni_word, ": the CI ", bnd_lbl, " bound (",
            dec_bnd, ") is ", if (iw$status=="On Track") "" else "not yet ",
            dir_word, " the NI boundary (", round(iw$boundary,3), ").")
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Interim Analysis — Interpretation", h2_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext(interp_w, body_fmt), fp_p = tight_p))
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # 3. CI method comparison (single-arm only)
        if (show_interim_ci_w && !is.null(iv_word)) {
          iw <- iv_word
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Interim Analysis — CI Method Comparison", h2_fmt), fp_p = tight_p))
          if (iw$is_two_arm) {
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(
                                            "CI method comparison is available for single-arm designs only.",
                                            hyp_fmt), fp_p = tight_p))
          } else {
            ci_methods_w <- c("Wilson"="wilson","Exact (C-P)"="exact",
                              "Agresti-Coull"="ac","Wald"="asymptotic",
                              "Logit"="logit","Bayes"="bayes")
            ci_comp_rows <- lapply(names(ci_methods_w), function(m_lbl) {
              m_key <- ci_methods_w[[m_lbl]]
              thr   <- tryCatch(
                interim_x_threshold(iw$n, iw$boundary, iw$conf_level, m_key, iw$is_safety),
                error = function(e) NA_integer_)
              rate  <- if (is.na(thr)) "—"
              else paste0(thr," / ",iw$n," = ",round(thr/iw$n,3))
              pass  <- if (is.na(thr)) "—"
              else if (iw$is_safety) { if(iw$x<=thr) "✓" else "✗" }
              else { if(iw$x>=thr) "✓" else "✗" }
              c(m_lbl, if(is.na(thr)) "—" else as.character(thr), rate, pass)
            })
            ci_comp_df <- as.data.frame(do.call(rbind, ci_comp_rows), stringsAsFactors=FALSE)
            colnames(ci_comp_df) <- c("CI Method",
                                      if(iw$is_safety) "Max events" else "Min events", "Rate",
                                      paste0("x=",iw$x," passes?"))
            doc <- officer::body_add_table(doc, ci_comp_df, align_table = "left")
          }
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # -- Interim position plot (Word) ------------------------------------
        if (show_interim_plot_w) {
          iv_plot_w <- if (!is.null(iv_word)) iv_word
          else tryCatch(interim_vals(), error = function(e) NULL)
          if (!is.null(iv_plot_w)) {
            tryCatch({
              iv <- iv_plot_w
              if (iv$is_two_arm) {
                xlo <- min(iv$boundary - 0.15, iv$ci_lo - 0.05, -0.25)
                xhi <- max(iv$boundary + 0.15, iv$ci_hi + 0.05,  0.25)
              } else {
                xlo <- max(0, min(iv$boundary - 0.15, iv$ci_lo - 0.05))
                xhi <- min(1, max(iv$p0 + 0.10, iv$ci_hi + 0.05))
              }
              good_lo <- if (iv$is_safety) xlo        else iv$boundary
              good_hi <- if (iv$is_safety) iv$boundary else xhi
              y_label <- if (iv$is_two_arm) "Difference" else "Current rate"
              x_label <- if (iv$is_two_arm) "Risk difference" else "Proportion"
              df_pt   <- data.frame(label = y_label, est = iv$p_hat,
                                    lo = iv$ci_lo, hi = iv$ci_hi)
              p_iw <- ggplot(df_pt, aes(x = est, y = label)) +
                annotate("rect", xmin = good_lo, xmax = good_hi,
                         ymin = -Inf, ymax = Inf, fill = "#18bdb9", alpha = 0.08) +
                geom_vline(xintercept = iv$boundary, linetype = "dashed",
                           colour = "#e07b39", linewidth = 0.9) +
                geom_vline(xintercept = if (iv$is_two_arm) 0 else iv$p0,
                           linetype = "dashed", colour = "#718096", linewidth = 0.7) +
                geom_errorbar(aes(xmin = lo, xmax = hi, y = label),
                              width = 0.15, colour = "#1a2e35", linewidth = 0.9,
                              orientation = "y") +
                geom_point(size = 4, colour = "#18bdb9") +
                scale_x_continuous(limits = c(xlo, xhi)) +
                labs(title = if (iv$is_two_arm)
                  "Observed risk difference vs NI boundary"
                  else
                    "Current observed rate vs NI boundary",
                  x = x_label, y = NULL) +
                plot_theme_large +
                theme(axis.text.y = element_text(size = 12))
              tmp_iw <- tempfile(fileext = ".png")
              ggsave(tmp_iw, p_iw, width = 5.5, height = 2.5, dpi = 150, bg = "white")
              doc <- officer::body_add_fpar(doc,
                                            officer::fpar(officer::ftext(
                                              "Interim Analysis — Position Plot", h2_fmt), fp_p = tight_p))
              doc <- officer::body_add_img(doc, src = tmp_iw, width = 5.5, height = 2.5)
              unlink(tmp_iw)
              doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }, error = function(e) NULL)
          }
        }
        
        # Footer (with page numbers added below)
        footer_fmt <- officer::fp_text(font.size = 8, color = "#94a3b8", italic = TRUE)
        doc <- officer::body_add_par(doc, "", style = "Normal")
        
        # Footer with page numbers using Word field codes
        page_fmt <- officer::fp_text(font.size = 8, color = "#94a3b8")
        doc <- officer::body_add_fpar(doc,
                                      officer::fpar(
                                        officer::ftext("This report was generated by PG-Power.   ", footer_fmt),
                                        officer::run_word_field(field = "PAGE",     prop = page_fmt),
                                        officer::ftext(" / ", page_fmt),
                                        officer::run_word_field(field = "NUMPAGES", prop = page_fmt),
                                        fp_p = officer::fp_par(
                                          text.align = "left",
                                          border.top = officer::fp_border(color = "#e2e8f0", width = 1)
                                        )
                                      )
        )
        
        print(doc, target = file)
        
      }, error = function(e) {
        message("downloadWord ERROR: ", conditionMessage(e))
      })
    }
  )
  
  # ── Interim Analysis: reactive + outputs ────────────────────────────────────────────────────────────────────────────────────────────
  
  interim_vals <- reactive({
    req(input$interim_n, input$interim_x,
        input$p0.expected, input$p1.tolerable)
    req(input$interim_n > 0)
    
    is_two_arm <- isTRUE(input$prop_design == "two_arm")
    is_safety  <- isTRUE(input$endpoint   == "safety")
    
    n     <- as.integer(input$interim_n)
    x_trt <- max(0L, min(as.integer(input$interim_x), n))
    p0    <- input$p0.expected
    delta <- input$p1.tolerable
    n_planned <- prop_n_at_delta()
    
    ci_method_raw <- input$ci_method_prop
    ci_method     <- if (is.null(ci_method_raw) || ci_method_raw == "z_power")
      "wilson" else ci_method_raw
    
    alpha_used <- as.numeric(input$sig.level)
    conf_level <- 1 - 2 * alpha_used
    
    if (is_two_arm) {
      r_alloc <- as.numeric(input$r)
      if (is.na(r_alloc) || r_alloc <= 0) r_alloc <- 1
      n_ctrl  <- max(1L, as.integer(round(n / r_alloc)))
      
      x_ctrl_raw <- if (!is.null(input$interim_x_control) &&
                        !is.na(input$interim_x_control))
        as.integer(input$interim_x_control) else 0L
      x_ctrl <- max(0L, min(x_ctrl_raw, n_ctrl))
      
      p_hat1   <- x_trt  / n
      p_hat0   <- x_ctrl / n_ctrl
      obs_diff <- p_hat1 - p_hat0
      
      se_diff <- sqrt(p_hat1 * (1 - p_hat1) / n +
                        p_hat0 * (1 - p_hat0) / n_ctrl)
      z95     <- qnorm(0.975)
      ci_lo   <- obs_diff - z95 * se_diff
      ci_hi   <- obs_diff + z95 * se_diff
      
      boundary <- if (is_safety) delta else -delta
      
      status <- if (is_safety) {
        if (ci_hi < boundary) "On Track" else "Concern"
      } else {
        if (ci_lo > boundary) "On Track" else "Concern"
      }
      
      list(n = n, n_ctrl = n_ctrl, x = x_trt, x_ctrl = x_ctrl,
           p_hat = obs_diff, p_hat1 = p_hat1, p_hat0 = p_hat0,
           ci_lo = ci_lo, ci_hi = ci_hi,
           p0 = p0, delta = delta, boundary = boundary,
           n_planned = n_planned, status = status,
           is_two_arm = TRUE, is_safety = is_safety,
           ci_method = ci_method, conf_level = conf_level,
           alpha_used = alpha_used, r_alloc = r_alloc,
           x_thresh = NA_integer_,
           ci_thresh_lo = NA_real_, ci_thresh_hi = NA_real_)
      
    } else {
      p_hat    <- x_trt / n
      boundary <- if (is_safety) p0 + delta else p0 - delta
      ci       <- prop_ci_vec(x_trt, n, conf_level, ci_method)
      
      status <- if (is_safety) {
        if (ci$upper < boundary) "On Track" else "Concern"
      } else {
        if (ci$lower > boundary) "On Track" else "Concern"
      }
      
      x_thresh <- tryCatch(
        interim_x_threshold(n, boundary, conf_level, ci_method, is_safety),
        error = function(e) NA_integer_
      )
      ci_thresh_lo <- NA_real_; ci_thresh_hi <- NA_real_
      if (!is.na(x_thresh)) {
        ct <- tryCatch(
          prop_ci_vec(x_thresh, n, conf_level, ci_method),
          error = function(e) list(lower = NA_real_, upper = NA_real_)
        )
        ci_thresh_lo <- ct$lower
        ci_thresh_hi <- ct$upper
      }
      
      list(n = n, x = x_trt, x_ctrl = NULL,
           p_hat = p_hat, p_hat1 = p_hat, p_hat0 = NULL,
           ci_lo = ci$lower, ci_hi = ci$upper,
           p0 = p0, delta = delta, boundary = boundary,
           n_planned = n_planned, status = status,
           is_two_arm = FALSE, is_safety = is_safety,
           ci_method = ci_method, conf_level = conf_level,
           alpha_used = alpha_used,
           x_thresh = x_thresh,
           ci_thresh_lo = ci_thresh_lo, ci_thresh_hi = ci_thresh_hi)
    }
  })
  
  output$interim_orientation_text <- renderUI({
    is_two_arm <- isTRUE(input$prop_design == "two_arm")
    design_txt <- if (is_two_arm) "treatment vs control arms" else "single arm vs benchmark"
    tags$div(
      style = "font-size:12px; color:#718096; background:#f8fafc;
               border:1px solid #e2e8f0; border-radius:8px;
               padding:10px 14px; line-height:1.65;",
      HTML(paste0(
        "Compares the <strong>current observed event rate</strong> against the ",
        "<strong>non-inferiority boundary</strong> (p\u2080 \u00b1 \u0394) using a ",
        design_txt, " design. ",
        "p\u2080, \u0394 and planned N are pulled automatically \u2014 ",
        "set them in the <strong>Calculator tab</strong> first."
      ))
    )
  })
  
  output$interim_sidebar_label <- renderUI({
    is_safety <- isTRUE(input$endpoint == "safety")
    ep_text   <- if (is_safety) "Safety  \u00b7  lower is better"
    else           "Efficacy  \u00b7  higher is better"
    tags$p(ep_text,
           style = "font-size:11px; font-weight:700; text-transform:uppercase;
                    letter-spacing:0.06em; color:#18bdb9; margin:0 0 10px;")
  })
  
  output$interim_pulled_vals <- renderUI({
    v        <- interim_vals()
    bnd_sign <- if (v$is_safety) "+" else "\u2212"
    bnd_text <- if (v$is_two_arm) {
      paste0("NI boundary (diff) = ", bnd_sign, round(abs(v$boundary), 3))
    } else {
      paste0("NI boundary = p\u2080 ", bnd_sign, " \u0394 = ", round(v$boundary, 3))
    }
    ci_label <- switch(v$ci_method,
                       wilson     = "Wilson",
                       exact      = "Exact (Clopper-Pearson)",
                       ac         = "Agresti-Coull",
                       asymptotic = "Asymptotic (Wald)",
                       logit      = "Logit", cloglog = "Cloglog", probit = "Probit",
                       bayes      = "Bayes", prop.test = "prop.test",
                       v$ci_method
    )
    tags$div(
      style = "font-size:12px; color:#4a5568; line-height:1.8;",
      tags$p(paste0("p\u2080 = ", v$p0, "  \u00b7  \u0394 = ", v$delta)),
      tags$p(bnd_text),
      tags$p(paste0("CI method: ", ci_label, "  (", round(v$conf_level * 100, 1), "% CI)")),
      tags$p(paste0("Planned N = ",
                    if (is.infinite(v$n_planned)) "Not achievable"
                    else format(v$n_planned, big.mark = ",")))
    )
  })
  
  output$interim_status_box <- renderUI({
    v <- interim_vals()
    col <- if (v$status == "On Track") "#18bdb9" else "#c0392b"
    
    ep_badge  <- if (v$is_safety) "Safety endpoint  \u00b7  lower is better"
    else             "Efficacy endpoint  \u00b7  higher is better"
    est_label <- if (v$is_two_arm) "Observed difference p\u0302\u2081 \u2212 p\u0302\u2080"
    else              "Observed rate p\u0302"
    bnd_label <- if (v$is_two_arm) "NI boundary (diff)" else "NI boundary (p\u2080 \u00b1 \u0394)"
    ci_pct    <- round(v$conf_level * 100, 1)
    
    deciding_bound <- if (v$is_safety) round(v$ci_hi, 3) else round(v$ci_lo, 3)
    bound_label    <- if (v$is_safety) "CI upper" else "CI lower"
    direction_word <- if (v$is_safety) "below" else "above"
    
    ni_verdict <- if (v$status == "On Track") {
      paste0("NI formally demonstrated  \u2014  ",
             bound_label, " (", deciding_bound, ") is ",
             direction_word, " boundary (", round(v$boundary, 3), ")")
    } else {
      paste0("Currently failing NI  \u2014  ",
             bound_label, " (", deciding_bound, ") must be ",
             direction_word, " boundary (", round(v$boundary, 3), ") to pass")
    }
    
    tags$div(
      style = paste0("background:", col, "15;border:1px solid ", col,
                     ";border-left-width:4px;border-radius:10px;padding:14px 18px;"),
      tags$div(style = paste0("color:", col, ";font-weight:700;font-size:11px;",
                              "letter-spacing:0.1em;text-transform:uppercase;margin-bottom:4px;"),
               v$status),
      tags$div(style = paste0("font-size:11px;color:", col, ";margin-bottom:6px;font-style:italic;"),
               ni_verdict),
      tags$div(style = "font-size:11px;color:#718096;margin-bottom:8px;", ep_badge),
      tags$div(
        style = "font-size:13px;color:#1a2e35;line-height:1.8;",
        paste0(est_label, " = ", round(v$p_hat, 3),
               "   (", ci_pct, "% CI: ", round(v$ci_lo, 3), " \u2013 ", round(v$ci_hi, 3), ")"),
        tags$br(),
        paste0(bnd_label, " = ", round(v$boundary, 3)),
        tags$br(),
        if (!v$is_two_arm && !is.na(v$x_thresh)) {
          thresh_type <- if (v$is_safety) "Max events allowed" else "Min events needed"
          paste0(thresh_type, " at n = ", v$n, ":  ", v$x_thresh,
                 "   CI at threshold: [", round(v$ci_thresh_lo, 3), ",  ",
                 round(v$ci_thresh_hi, 3), "]")
        } else if (!v$is_two_arm) {
          "Threshold not achievable at current n"
        } else {
          paste0("Margin to boundary: ", round(v$p_hat - v$boundary, 3),
                 if (v$is_safety) "  (\u2212 = safe side)" else "  (+ = safe side)")
        },
        tags$br(),
        if (!is.infinite(v$n_planned))
          paste0("Enrolled: ", v$n, " of planned ",
                 format(v$n_planned, big.mark = ","),
                 "  (", round(100 * v$n / v$n_planned, 1), "%)")
        else paste0("Enrolled: ", v$n)
      )
    )
  })
  
  output$interim_position_plot <- renderPlotly({
    v <- interim_vals()
    
    if (v$is_two_arm) {
      xlo <- min(v$boundary - 0.15, v$ci_lo - 0.05, -0.25)
      xhi <- max(v$boundary + 0.15, v$ci_hi + 0.05,  0.25)
    } else {
      xlo <- max(0, min(v$boundary - 0.15, v$ci_lo - 0.05))
      xhi <- min(1, max(v$p0 + 0.10, v$ci_hi + 0.05))
    }
    
    good_lo <- if (v$is_safety) xlo        else v$boundary
    good_hi <- if (v$is_safety) v$boundary else xhi
    
    y_label   <- if (v$is_two_arm) "Difference (p\u0302\u2081 \u2212 p\u0302\u2080)" else "Current p\u0302"
    x_label   <- if (v$is_two_arm) "Risk difference" else "Proportion"
    plt_title <- if (v$is_two_arm) "Observed risk difference vs NI boundary"
    else              "Current observed rate vs NI boundary"
    ref_x     <- if (v$is_two_arm) 0 else v$p0
    ref_label <- if (v$is_two_arm) "No difference (0)" else paste0("p\u2080 = ", v$p0)
    
    df_pt <- data.frame(label = y_label, est = v$p_hat, lo = v$ci_lo, hi = v$ci_hi)
    
    p <- ggplot(df_pt, aes(x = est, y = label)) +
      annotate("rect", xmin = good_lo, xmax = good_hi,
               ymin = -Inf, ymax = Inf, fill = "#18bdb9", alpha = 0.08) +
      geom_vline(xintercept = v$boundary, linetype = "dashed",
                 colour = "#e07b39", linewidth = 0.9) +
      geom_vline(xintercept = ref_x, linetype = "dashed",
                 colour = "#718096", linewidth = 0.7) +
      geom_errorbar(aes(xmin = lo, xmax = hi, y = label),
                    width = 0.15, colour = "#1a2e35", linewidth = 0.9,
                    orientation = "y") +
      geom_point(size = 4, colour = "#18bdb9") +
      scale_x_continuous(limits = c(xlo, xhi)) +
      labs(title = plt_title, x = x_label, y = NULL) +
      plot_theme_large +
      theme(axis.text.y = element_text(size = 12))
    
    bnd_anchor <- if (v$is_safety) "right" else "left"
    
    ggplotly(p) %>%
      layout(
        hovermode     = "x unified",
        paper_bgcolor = "transparent",
        plot_bgcolor  = "white",
        font          = list(family = "DM Sans, sans-serif"),
        annotations   = list(
          list(x = v$boundary, y = 1.08, yref = "paper",
               text = paste0("NI boundary (", round(v$boundary, 3), ")"),
               showarrow = FALSE, xanchor = bnd_anchor,
               font = list(color = "#e07b39", size = 11)),
          list(x = ref_x, y = 0.80, yref = "paper",
               text = ref_label, showarrow = FALSE, xanchor = "left",
               font = list(color = "#718096", size = 11))
        )
      ) %>%
      config(displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  output$interim_calc_table <- renderUI({
    if (!isTRUE(input$show_interim_calctbl)) return(NULL)
    v        <- interim_vals()
    bnd_sign <- if (v$is_safety) "+" else "\u2212"
    
    rows <- if (v$is_two_arm) {
      list(
        c(paste0("Treatment enrolled (n\u2081)  [r = ", v$r_alloc, ":1]"), as.character(v$n)),
        c("Control enrolled (n\u2080 = n\u2081 \u00f7 r)",                  as.character(v$n_ctrl)),
        c("Treatment events (x\u2081)",                                     as.character(v$x)),
        c("Control events (x\u2080)",                                       as.character(v$x_ctrl)),
        c("p\u0302\u2081 = x\u2081 \u00f7 n\u2081",
          paste0(v$x, " \u00f7 ", v$n, " = ", round(v$p_hat1, 4))),
        c("p\u0302\u2080 = x\u2080 \u00f7 n\u2080",
          paste0(v$x_ctrl, " \u00f7 ", v$n_ctrl, " = ", round(v$p_hat0, 4))),
        c("Difference = p\u0302\u2081 \u2212 p\u0302\u2080",
          paste0(round(v$p_hat1, 4), " \u2212 ", round(v$p_hat0, 4), " = ", round(v$p_hat, 4))),
        c(paste0(round(v$conf_level * 100, 1), "% CI (Wald, difference)"),
          paste0("[", round(v$ci_lo, 4), ",  ", round(v$ci_hi, 4), "]")),
        c(paste0("NI boundary = ", bnd_sign, "\u0394"),
          paste0(bnd_sign, round(abs(v$boundary), 4))),
        c(paste0("Margin = estimate \u2212 boundary  (",
                 if (v$is_safety) "\u2212 = safe)" else "+ = safe)"),
          paste0(round(v$p_hat, 4), " \u2212 (", round(v$boundary, 4),
                 ") = ", round(v$p_hat - v$boundary, 4)))
      )
    } else {
      list(
        c("Enrolled (n)",    as.character(v$n)),
        c("Events (x)",      as.character(v$x)),
        c("p\u0302 = x \u00f7 n",
          paste0(v$x, " \u00f7 ", v$n, " = ", round(v$p_hat, 4))),
        c(paste0(round(v$conf_level * 100, 1), "% CI  (", v$ci_method, ")"),
          paste0("[", round(v$ci_lo, 4), ",  ", round(v$ci_hi, 4), "]")),
        c("p\u2080",  as.character(v$p0)),
        c("\u0394",   as.character(v$delta)),
        c(paste0("NI boundary = p\u2080 ", bnd_sign, " \u0394"),
          paste0(v$p0, " ", bnd_sign, " ", v$delta, " = ", round(v$boundary, 4))),
        c(paste0("Margin = p\u0302 \u2212 boundary  (",
                 if (v$is_safety) "\u2212 = safe)" else "+ = safe)"),
          paste0(round(v$p_hat, 4), " \u2212 ", round(v$boundary, 4),
                 " = ", round(v$p_hat - v$boundary, 4)))
      )
    }
    
    tbl_rows <- lapply(rows, function(r) {
      tags$tr(
        tags$td(style = "padding:5px 10px;font-weight:600;font-size:12px;
                         border:1px solid #e2e8f0;white-space:nowrap;
                         font-family:'DM Mono',monospace;color:#1a2e35;", r[1]),
        tags$td(style = "padding:5px 10px;font-size:12px;
                         border:1px solid #e2e8f0;color:#4a5568;
                         font-family:'DM Mono',monospace;", r[2])
      )
    })
    
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        style = "border-collapse:collapse;width:100%;",
        tags$thead(tags$tr(
          tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                           font-size:11px;letter-spacing:0.06em;text-transform:uppercase;
                           color:#64748b;", "Calculation"),
          tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                           font-size:11px;letter-spacing:0.06em;text-transform:uppercase;
                           color:#64748b;", "Value")
        )),
        tags$tbody(tbl_rows)
      )
    )
  })
  
  output$interim_ci_threshold_table <- renderUI({
    if (!isTRUE(input$show_interim_citbl)) return(NULL)
    v <- interim_vals()
    
    if (v$is_two_arm) {
      return(tags$div(
        style = "font-size:12px;color:#94a3b8;margin-top:6px;padding:8px 12px;
                 background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;",
        "CI method comparison is available for single-arm designs only.
         The two-arm analysis uses a Wald CI on the risk difference."
      ))
    }
    
    if (v$n < 2) return(NULL)
    
    compare_methods <- c(
      "Wilson"            = "wilson",
      "Exact (C-P)"       = "exact",
      "Agresti-Coull"     = "ac",
      "Asymptotic (Wald)" = "asymptotic",
      "Logit"             = "logit",
      "Bayes"             = "bayes"
    )
    
    col_header <- if (v$is_safety)
      paste0("Max events allowed (\u2264 x) to keep CI upper < ", round(v$boundary, 3))
    else
      paste0("Min events needed (\u2265 x) to get CI lower > ", round(v$boundary, 3))
    
    tbl_rows <- lapply(seq_along(compare_methods), function(i) {
      m_label <- names(compare_methods)[i]
      m_key   <- compare_methods[i]
      thresh  <- tryCatch(
        interim_x_threshold(v$n, v$boundary, v$conf_level, m_key, v$is_safety),
        error = function(e) NA_integer_
      )
      
      passes <- if (is.na(thresh)) NA
      else if (v$is_safety) v$x <= thresh
      else                  v$x >= thresh
      
      status_txt <- if (is.na(thresh)) "\u2014"
      else if (isTRUE(passes)) "\u2713"
      else "\u2717"
      status_col <- if (is.na(thresh)) "#94a3b8"
      else if (isTRUE(passes)) "#18bdb9"
      else "#c0392b"
      
      rate_txt <- if (is.na(thresh)) "\u2014"
      else paste0(thresh, " / ", v$n, " = ", round(thresh / v$n, 3))
      
      ci_txt <- if (is.na(thresh)) "\u2014" else {
        ct <- tryCatch(
          prop_ci_vec(thresh, v$n, v$conf_level, m_key),
          error = function(e) list(lower = NA_real_, upper = NA_real_)
        )
        if (is.na(ct$lower)) "\u2014"
        else paste0("[", round(ct$lower, 3), ",  ", round(ct$upper, 3), "]")
      }
      
      is_selected <- (m_key == v$ci_method)
      row_bg <- if (is_selected) "background:#f0fbfb;" else ""
      
      tags$tr(
        style = row_bg,
        tags$td(style = "padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;
                         font-family:'DM Mono',monospace;color:#1a2e35;white-space:nowrap;",
                if (is_selected) tags$strong(paste0(m_label, " \u2713")) else m_label),
        tags$td(style = "padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;
                         font-family:'DM Mono',monospace;color:#4a5568;text-align:center;",
                if (is.na(thresh)) "\u2014" else as.character(thresh)),
        tags$td(style = "padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;
                         font-family:'DM Mono',monospace;color:#4a5568;", rate_txt),
        tags$td(style = "padding:5px 10px;font-size:12px;border:1px solid #e2e8f0;
                         font-family:'DM Mono',monospace;color:#4a5568;", ci_txt),
        tags$td(style = paste0("padding:5px 10px;font-size:14px;border:1px solid #e2e8f0;
                                font-weight:700;text-align:center;color:", status_col, ";"),
                status_txt)
      )
    })
    
    tags$div(
      style = "margin-top:10px;",
      tags$p(style = "font-size:11px;font-weight:700;letter-spacing:0.06em;
                      text-transform:uppercase;color:#64748b;margin-bottom:6px;",
             paste0("Effect of CI method on NI decision  (n = ", v$n,
                    ",  ", round(v$conf_level * 100, 1), "% CI)")),
      tags$p(style = "font-size:11px;color:#718096;margin-bottom:8px;",
             if (v$is_safety)
               paste0("Safety: lower is better. Shows the most events you can observe ",
                      "and still have the CI upper bound below the NI boundary.")
             else
               paste0("Efficacy: higher is better. Shows the fewest events needed ",
                      "for the CI lower bound to exceed the NI boundary.")),
      tags$div(
        style = "overflow-x:auto;",
        tags$table(
          style = "border-collapse:collapse;width:100%;",
          tags$thead(tags$tr(
            tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                             font-size:11px;letter-spacing:0.05em;text-transform:uppercase;
                             color:#64748b;", "CI Method"),
            tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                             font-size:11px;letter-spacing:0.05em;text-transform:uppercase;
                             color:#64748b;text-align:center;",
                    if (v$is_safety) "Max events (x)" else "Min events (x)"),
            tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                             font-size:11px;letter-spacing:0.05em;text-transform:uppercase;
                             color:#64748b;", col_header),
            tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                             font-size:11px;letter-spacing:0.05em;text-transform:uppercase;
                             color:#64748b;",
                    if (v$is_safety) "CI at max x [lower,  upper]"
                    else             "CI at min x [lower,  upper]"),
            tags$th(style = "padding:5px 10px;background:#eef3f8;border:1px solid #e2e8f0;
                             font-size:11px;letter-spacing:0.05em;text-transform:uppercase;
                             color:#64748b;text-align:center;",
                    paste0("x = ", v$x, " passes?"))
          )),
          tags$tbody(tbl_rows)
        )
      )
    )
  })
  # ── Calculation code outputs ─────────────────────────────────────────────────
  
  # -- Calculator code output --------------------------------------------------
  output$calc_code_ui <- renderUI({
    if (!isTRUE(input$show_calc_code)) return(NULL)
    is_one    <- isTRUE(input$prop_design == "one_arm")
    is_safety <- isTRUE(input$endpoint   == "safety")
    ci_m      <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
    use_z     <- isTRUE(ci_m == "z_power")
    alpha     <- as.numeric(input$sig.level)
    pwr       <- input$power
    p0        <- input$p0.expected
    p1        <- input$p1.expected
    delta     <- input$p1.tolerable
    
    if (is_one && use_z) {
      code_txt <- paste(c(
        "# Single-arm NI  —  Z (power formula)",
        paste0("# H₀: p ≤ p₀ − Δ   vs.   H₁: p > p₀ − Δ"),
        "",
        paste0("p0        <- ", p0, "   # benchmark / performance goal"),
        paste0("p1        <- ", p1, "   # expected device rate"),
        paste0("delta     <- ", delta,  "   # NI margin"),
        paste0("sig.level <- ", alpha,  "   # one-sided α"),
        paste0("power     <- ", pwr,    "   # desired power"),
        "",
        "p_thr   <- p0 - delta",
        "z_alpha <- qnorm(1 - sig.level)",
        "z_beta  <- qnorm(power)",
        "n       <- ceiling(",
        "  (z_alpha * sqrt(p_thr * (1 - p_thr)) +",
        "   z_beta  * sqrt(p1   * (1 - p1  )))^2 /",
        "  (p1 - p_thr)^2",
        ")"
      ), collapse = "
")
    } else if (is_one && !use_z) {
      conf_lev <- round(1 - 2 * alpha, 3)
      code_txt <- paste(c(
        "# Single-arm NI  —  CI simulation search",
        paste0("# CI method: ", ci_m, "   conf.level: ", conf_lev),
        paste0("# H₀: p ≤ p₀ − Δ   vs.   H₁: p > p₀ − Δ"),
        "",
        paste0("p0 <- ", p0, ";  p1 <- ", p1, ";  delta <- ", delta),
        paste0("boundary <- p0 - delta   # = ", p0 - delta),
        paste0("conf_level <- ", conf_lev, "   # = 1 - 2α"),
        "",
        "# At each candidate n, simulate nsim trials and check CI lower > boundary",
        paste0('prop_ci_vec(x, n, conf_level, method = "', ci_m, '")'),
        "# Declare NI if ci$lower > boundary",
        "# Binary search finds smallest n achieving target power"
      ), collapse = "
")
    } else if (!is_one && use_z) {
      r_val <- as.numeric(input$r)
      code_txt <- paste(c(
        "# Two-arm NI  —  Z (power formula)",
        paste0("# H₀: p₁ − p₀ ≤ −Δ   vs.   H₁: p₁ − p₀ > −Δ"),
        "",
        paste0("p0 <- ", p0, ";  p1 <- ", p1, ";  delta <- ", delta),
        paste0("r  <- ", r_val, "   # allocation ratio (treatment:control)"),
        paste0("sig.level <- ", alpha, ";  power <- ", pwr),
        "",
        "z_alpha <- qnorm(1 - sig.level)",
        "z_beta  <- qnorm(power)",
        "eff     <- p1 - p0 + delta",
        "p_bar   <- (p0 + r * p1) / (1 + r)",
        "v0      <- (1 + 1/r) * p_bar * (1 - p_bar)",
        "v1      <- p0*(1-p0) + (1/r)*p1*(1-p1)",
        "n0      <- (z_alpha*sqrt(v0) + z_beta*sqrt(v1))^2 / eff^2",
        "n_total <- ceiling(n0) + ceiling(r * n0)"
      ), collapse = "
")
    } else {
      conf_lev <- round(1 - 2 * alpha, 3)
      r_val <- as.numeric(input$r)
      code_txt <- paste(c(
        "# Two-arm NI  —  CI simulation search",
        paste0("# CI method: ", ci_m, "   conf.level: ", conf_lev),
        paste0("# H₀: p₁ − p₀ ≤ −Δ   vs.   H₁: p₁ − p₀ > −Δ"),
        "",
        paste0("p0 <- ", p0, ";  p1 <- ", p1, ";  delta <- ", delta),
        paste0("r  <- ", r_val, "   # allocation ratio"),
        paste0("conf_level <- ", conf_lev),
        "",
        "# At each candidate n0, simulate nsim two-arm trials:",
        paste0('ci0 <- prop_ci_vec(x0, n0, conf_level, "', ci_m, '")'),
        paste0('ci1 <- prop_ci_vec(x1, n1, conf_level, "', ci_m, '")'),
        "# Declare NI if (ci1$lower - ci0$upper) > -delta",
        "# Binary search finds smallest n0 achieving target power"
      ), collapse = "
")
    }
    
    code_block_ui(code_txt)
  })
  
  # -- Interim calculation code output ----------------------------------------
  output$interim_code_ui <- renderUI({
    if (!isTRUE(input$show_interim_code)) return(NULL)
    is_one <- isTRUE(input$prop_design == "one_arm")
    is_safety <- isTRUE(input$endpoint == "safety")
    
    if (is_one) {
      code_txt <- paste(c(
        "# Single-arm NI — CI lower vs boundary",
        paste0("# boundary = p₀ ", if(is_safety) "+" else "-", " Δ = ",
               input$p0.expected, if(is_safety) " + " else " - ", input$p1.tolerable,
               " = ", if(is_safety) input$p0.expected + input$p1.tolerable
               else input$p0.expected - input$p1.tolerable),
        "",
        "p_hat   <- x / n",
        paste0("ci      <- prop_ci_vec(x, n, conf_level = ",
               round(1 - 2 * as.numeric(input$sig.level), 3), ', method = "',
               if(is.null(input$ci_method_prop) || input$ci_method_prop == "z_power")
                 "wilson" else input$ci_method_prop, '")'),
        if (is_safety)
          paste0("# NI if ci$upper < ", round(input$p0.expected + input$p1.tolerable, 3))
        else
          paste0("# NI if ci$lower > ", round(input$p0.expected - input$p1.tolerable, 3))
      ), collapse = "
")
    } else {
      code_txt <- paste(c(
        "# Two-arm NI — Wald CI on risk difference",
        paste0("# boundary = ", if(is_safety) "+" else "-", "Δ = ",
               if(is_safety) input$p1.tolerable else -input$p1.tolerable),
        "",
        "p_hat1   <- x1 / n1",
        "p_hat0   <- x0 / n0",
        "obs_diff <- p_hat1 - p_hat0",
        "se_diff  <- sqrt(p_hat1*(1-p_hat1)/n1 + p_hat0*(1-p_hat0)/n0)",
        paste0("ci_lo    <- obs_diff - qnorm(0.975) * se_diff  # Wald 95% CI"),
        paste0("ci_hi    <- obs_diff + qnorm(0.975) * se_diff"),
        if (is_safety)
          paste0("# NI if ci_hi < ", input$p1.tolerable)
        else
          paste0("# NI if ci_lo > ", -input$p1.tolerable)
      ), collapse = "
")
    }
    
    code_block_ui(code_txt, margin_top = "14px")
  })
  
  # ── Interim Analysis: CSV downloads ──────────────────────────────────────────
  
  # -- Interim CSV downloads -------------------------------------------------
  output$download_interim_calc_csv <- downloadHandler(
    filename = function() paste0("PGPower_Interim_Calc_", Sys.Date(), ".csv"),
    content  = function(file) {
      v <- tryCatch(interim_vals(), error = function(e) NULL)
      if (is.null(v)) { write.csv(data.frame(Error = "No interim data"), file, row.names = FALSE); return() }
      bnd_sign <- if (v$is_safety) "+" else "-"
      if (v$is_two_arm) {
        df <- data.frame(
          Parameter = c("Treatment enrolled (n1)","Control enrolled (n0)",
                        "Treatment events (x1)","Control events (x0)",
                        "p_hat1","p_hat0","Difference",
                        paste0(round(v$conf_level*100,1),"% CI lower"),
                        paste0(round(v$conf_level*100,1),"% CI upper"),
                        "NI boundary","Margin","Status"),
          Value = c(v$n, v$n_ctrl, v$x, v$x_ctrl,
                    round(v$p_hat1,4), round(v$p_hat0,4), round(v$p_hat,4),
                    round(v$ci_lo,4), round(v$ci_hi,4),
                    round(v$boundary,4), round(v$p_hat - v$boundary,4),
                    v$status),
          stringsAsFactors = FALSE)
      } else {
        df <- data.frame(
          Parameter = c("Enrolled (n)","Events (x)","p_hat",
                        paste0(round(v$conf_level*100,1),"% CI lower"),
                        paste0(round(v$conf_level*100,1),"% CI upper"),
                        "NI boundary","Margin","Status"),
          Value = c(v$n, v$x, round(v$p_hat,4),
                    round(v$ci_lo,4), round(v$ci_hi,4),
                    round(v$boundary,4), round(v$p_hat - v$boundary,4),
                    v$status),
          stringsAsFactors = FALSE)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_interim_ci_csv <- downloadHandler(
    filename = function() paste0("PGPower_Interim_CI_", Sys.Date(), ".csv"),
    content  = function(file) {
      v <- tryCatch(interim_vals(), error = function(e) NULL)
      if (is.null(v) || v$is_two_arm || v$n < 2) {
        write.csv(data.frame(Note = "CI comparison available for single-arm only"),
                  file, row.names = FALSE); return()
      }
      methods <- c("Wilson"="wilson","Exact (C-P)"="exact","Agresti-Coull"="ac",
                   "Wald"="asymptotic","Logit"="logit","Bayes"="bayes")
      rows <- lapply(names(methods), function(lbl) {
        key <- methods[[lbl]]
        thr <- tryCatch(
          interim_x_threshold(v$n, v$boundary, v$conf_level, key, v$is_safety),
          error = function(e) NA_integer_)
        rate <- if (is.na(thr)) NA_real_ else round(thr / v$n, 4)
        pass <- if (is.na(thr)) NA
        else if (v$is_safety) v$x <= thr else v$x >= thr
        data.frame(Method = lbl,
                   Threshold_x = if(is.na(thr)) NA_integer_ else thr,
                   Rate = rate,
                   Current_x_passes = pass,
                   stringsAsFactors = FALSE)
      })
      write.csv(do.call(rbind, rows), file, row.names = FALSE)
    }
  )
  
}