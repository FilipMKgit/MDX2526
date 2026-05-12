server <- function(input, output, session) {
  
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
  
  # -- Live report contents list ---------------------------------------------
  output$report_contents_ui <- renderUI({
    all_items <- list(
      list(label = "Results table",              on = isTRUE(input$rpt_results),    soon = FALSE),
      list(label = "Interpretation paragraph",   on = isTRUE(input$rpt_interp_inc), soon = FALSE),
      list(label = "CI method comparison table", on = isTRUE(input$rpt_ci_compare), soon = FALSE),
      list(label = "Definitions glossary",       on = isTRUE(input$rpt_definitions),soon = FALSE),
      list(label = "Calculation code",           on = isTRUE(input$rpt_calc_code),  soon = FALSE),
      list(label = "Sensitivity plots",          on = isTRUE(input$rpt_plots),      soon = FALSE),
      list(label = "Sensitivity tables",         on = isTRUE(input$rpt_tables),     soon = FALSE),
      list(label = "PG-Power footer",            on = TRUE,                         soon = FALSE),
      list(label = "CI bands on plots",          on = FALSE,                        soon = TRUE)
    )
    
    make_li <- function(item) {
      if (item$soon) {
        tags$li(
          tags$span("\u2013", class = "rc-cross"),
          tags$span(item$label),
          tags$span("coming soon", class = "rc-soon")
        )
      } else if (item$on) {
        tags$li(tags$span("\u2713", class = "rc-tick"), tags$span(item$label))
      } else {
        tags$li(
          tags$span("\u2013", class = "rc-cross"),
          tags$span(item$label, style = "color:#94a3b8;")
        )
      }
    }
    
    tags$ul(class = "report-contents", lapply(all_items, make_li))
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
    c("results", "interp", "ci_compare", "definitions", "calc_code", "plots", "tables")
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
  
  # -- PDF summary download --------------------------------------------------
  output$downloadPDF <- downloadHandler(
    filename    = function() paste0("PGPower_summary_", Sys.Date(), ".pdf"),
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
        show_plots    <- isTRUE(input$rpt_plots)
        show_tables   <- isTRUE(input$rpt_tables)
        section_order <- get_section_order()
        
        plot_files <- if (show_plots) capture_plots() else list(delta = NULL, p1 = NULL)
        
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
        
        # Sensitivity plots
        plots_html <- if (show_plots) {
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
          paste0(
            "<h2 style='", h2s, "'>Sensitivity Plots</h2>",
            img_tag(plot_files$delta, "NI margin (\u0394) vs total sample size (n)"),
            img_tag(plot_files$p1,    "Expected event rate (p\u2081) vs total sample size (n)"),
            "<hr style='border-color:", blue, ";margin:14px 0;'>"
          )
        } else ""
        
        # -- Sensitivity tables -----------------------------------------------
        tables_html <- if (show_tables) {
          df_delta <- tryCatch(prop_df_delta(), error = function(e) NULL)
          df_p1    <- tryCatch(prop_df_p1(),    error = function(e) NULL)
          
          tbl_delta <- if (!is.null(df_delta)) {
            make_sens_table_html(
              df_delta,
              col_names = c("NI Margin (\u0394)", "Total N"),
              caption   = paste0("Sensitivity: NI margin vs total sample size  (p\u2080 = ",
                                 input$p0.expected, ", p\u2081 = ", input$p1.expected, ")"),
              blue      = blue, th_fn = th, td_fn = td
            )
          } else ""
          
          tbl_p1 <- if (!is.null(df_p1)) {
            make_sens_table_html(
              df_p1,
              col_names = c("Expected Event Rate (p\u2081)", "Total N"),
              caption   = paste0("Sensitivity: expected event rate vs total sample size  (p\u2080 = ",
                                 input$p0.expected, ", \u0394 = ", input$p1.tolerable, ")"),
              blue      = blue, th_fn = th, td_fn = td
            )
          } else ""
          
          paste0(
            "<h2 style='", h2s, "'>Sensitivity Tables</h2>",
            tbl_delta,
            "<br>",
            tbl_p1,
            "<hr style='border-color:", blue, ";margin:14px 0;'>"
          )
        } else ""
        
        section_html_map <- list(
          results     = results_html,
          interp      = interp_html,
          ci_compare  = ci_html,
          definitions = defs_html,
          calc_code   = code_html,
          plots       = plots_html,
          tables      = tables_html
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
            paste0("Generated: ", format(Sys.Date(), "%d %B %Y")) else NULL,
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
          "@page{size:A4;margin:18mm;}",
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
        } else if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(tmp_html, file = file, vwidth = 794, vheight = 1123)
        } else {
          file.copy(tmp_html, file, overwrite = TRUE)
          showNotification(
            "Install 'pagedown' for true PDF output: install.packages('pagedown')",
            type = "warning", duration = 8
          )
        }
        unlink(tmp_html)
        
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
    filename    = function() paste0("PGPower_summary_", Sys.Date(), ".docx"),
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
        
        show_plots_w  <- isTRUE(input$rpt_plots)
        show_tables_w <- isTRUE(input$rpt_tables)
        plot_files_w  <- if (show_plots_w) capture_plots() else list(delta = NULL, p1 = NULL)
        
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
            paste0("Generated: ", format(Sys.Date(), "%d %B %Y")) else NULL,
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
        
        # Sensitivity plots (Word)
        if (show_plots_w) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Sensitivity Plots", h2_fmt), fp_p = tight_p))
          for (plot_file in list(plot_files_w$delta, plot_files_w$p1)) {
            if (!is.null(plot_file) && file.exists(plot_file)) {
              doc <- officer::body_add_img(doc, src = plot_file, width = 5.5, height = 3.2)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # -- Sensitivity tables (Word) ----------------------------------------
        if (show_tables_w) {
          doc <- officer::body_add_fpar(doc,
                                        officer::fpar(officer::ftext("Sensitivity Tables", h2_fmt), fp_p = tight_p))
          
          # Delta table
          df_delta <- tryCatch(prop_df_delta(), error = function(e) NULL)
          if (!is.null(df_delta)) {
            df_delta_w <- df_delta
            df_delta_w[[2]] <- ifelse(is.infinite(df_delta_w[[2]]) | is.na(df_delta_w[[2]]),
                                      "\u2014", format(round(df_delta_w[[2]]), big.mark = ","))
            df_delta_w[[1]] <- sprintf("%.3f", df_delta_w[[1]])
            colnames(df_delta_w) <- c("NI Margin (\u0394)", "Total N")
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(
                                            paste0("NI margin sensitivity  (p\u2080 = ", input$p0.expected,
                                                   ", p\u2081 = ", input$p1.expected, ")"),
                                            hyp_fmt), fp_p = tight_p))
            doc <- officer::body_add_table(doc, df_delta_w, align_table = "left")
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          
          # p1 table
          df_p1 <- tryCatch(prop_df_p1(), error = function(e) NULL)
          if (!is.null(df_p1)) {
            df_p1_w <- df_p1
            df_p1_w[[2]] <- ifelse(is.infinite(df_p1_w[[2]]) | is.na(df_p1_w[[2]]),
                                   "\u2014", format(round(df_p1_w[[2]]), big.mark = ","))
            df_p1_w[[1]] <- sprintf("%.3f", df_p1_w[[1]])
            colnames(df_p1_w) <- c("Expected Event Rate (p\u2081)", "Total N")
            doc <- officer::body_add_fpar(doc,
                                          officer::fpar(officer::ftext(
                                            paste0("Event rate sensitivity  (p\u2080 = ", input$p0.expected,
                                                   ", \u0394 = ", input$p1.tolerable, ")"),
                                            hyp_fmt), fp_p = tight_p))
            doc <- officer::body_add_table(doc, df_p1_w, align_table = "left")
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(" "), fp_p = border_p))
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        
        # Footer
        footer_fmt <- officer::fp_text(font.size = 8, color = "#94a3b8", italic = TRUE)
        footer_p   <- officer::fp_par(
          text.align = "left",
          border.top = officer::fp_border(color = "#e2e8f0", width = 1)
        )
        doc <- officer::body_add_par(doc, "", style = "Normal")
        doc <- officer::body_add_fpar(doc,
                                      officer::fpar(
                                        officer::ftext("This report was generated by PG-Power.", footer_fmt),
                                        fp_p = footer_p
                                      ))
        
        print(doc, target = file)
        
      }, error = function(e) {
        message("downloadWord ERROR: ", conditionMessage(e))
      })
    }
  )
}