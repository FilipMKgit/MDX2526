server <- function(input, output, session) {

  # ── Endpoint presets ─────────────────────────────────────────────────────────
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

  # ── Dark mode ────────────────────────────────────────────────────────────────
  observeEvent(input$dark_mode_on, {
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode_on)) dark_mode else default_mode
    )
  }, ignoreInit = TRUE)

  # ── Label updates when design changes ────────────────────────────────────────
  observeEvent(input$prop_design, {
    is_one <- isTRUE(input$prop_design == "one_arm")

    updateSliderInput(session, "p0.expected", label = if (is_one)
      "Benchmark / performance goal (p₀):" else "Control event rate (p₀):")

    updateSliderInput(session, "p1.expected", label = if (is_one)
      "Expected device event rate (p₁):" else "Expected experimental event rate (p₁):")

    updateSliderInput(session, "p1.tolerable", label = if (is_one)
      "Non-inferiority margin (Δ, vs benchmark):" else "Non-inferiority margin (Δ, risk difference):")
  }, ignoreInit = FALSE)

  # ── Equation modals ──────────────────────────────────────────────────────────
  observeEvent(input$eq_prop1, {
    is_one <- isTRUE(input$prop_design == "one_arm")
    showModal(modalDialog(
      title = "Proportions: Δ vs total sample size",
      if (!is_one) tagList(
        tags$p("Hypotheses (risk difference):"),
        tags$pre("H0: (p₁ − p₀) ≤ −Δ\nH1: (p₁ − p₀) > −Δ"),
        tags$p("N computation:"),
        tags$ul(
          tags$li("Z (power formula): analytic z-approximation."),
          tags$li("CI methods: simulate trials at each N; increase N until simulated power ≥ target.")
        ),
        tags$p("CI decision rule (simulation methods):"),
        tags$pre("Lower(RD) = Lower(p₁) − Upper(p₀)\nDeclare NI if Lower(RD) > −Δ")
      ) else tagList(
        tags$p("Hypotheses (single-arm vs benchmark):"),
        tags$pre("H0: p ≤ (p₀ − Δ)\nH1: p > (p₀ − Δ)"),
        tags$p("CI decision rule:"),
        tags$pre("Declare NI if Lower(p) > (p₀ − Δ)")
      ),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$eq_prop2, {
    is_one <- isTRUE(input$prop_design == "one_arm")
    showModal(modalDialog(
      title = "Proportions: p₁ vs total sample size",
      if (!is_one) tagList(
        tags$p("Hypotheses (risk difference):"),
        tags$pre("H0: (p₁ − p₀) ≤ −Δ\nH1: (p₁ − p₀) > −Δ"),
        tags$p("What varies: p₁ swept ±0.10 around chosen value; Δ and p₀ held fixed."),
        tags$p("Decision rule:"),
        tags$pre("Lower(RD) = Lower(p₁) − Upper(p₀)\nDeclare NI if Lower(RD) > −Δ")
      ) else tagList(
        tags$p("Hypotheses (single-arm vs benchmark):"),
        tags$pre("H0: p ≤ (p₀ − Δ)\nH1: p > (p₀ − Δ)"),
        tags$p("What varies: p₁ swept ±0.10 around chosen value; Δ and p₀ held fixed."),
        tags$p("Decision rule:"),
        tags$pre("Declare NI if Lower(p) > (p₀ − Δ)")
      ),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$eq_ci_prop, {
    showModal(modalDialog(
      title = "Confidence interval methods",
      tags$p("These affect the CI bounds used in simulation-based sizing (More Options → Method)."),
      tags$hr(),
      tags$h4("Common methods"),
      tags$ul(
        tags$li(tags$b("Wilson:"), "Good coverage, behaves well for small n and p near 0/1. Recommended default."),
        tags$li(tags$b("Exact (Clopper–Pearson):"), "Inverts the binomial test. Conservative (wider CI → larger N)."),
        tags$li(tags$b("Agresti–Coull:"), "Adds a small correction ('add 2 successes + 2 failures'). Close to Wilson."),
        tags$li(tags$b("Asymptotic (Wald):"), "Normal approximation p ± z·SE. Avoid at small n or extreme p.")
      ),
      tags$h4("Test-based"),
      tags$ul(tags$li(tags$b("prop.test:"), "Score/chi-squared based; generally better than Wald.")),
      tags$h4("Model-based"),
      tags$ul(
        tags$li(tags$b("Logit:"), "CI on log-odds scale, back-transformed."),
        tags$li(tags$b("Probit:"), "Same idea, probit link."),
        tags$li(tags$b("Cloglog:"), "Complementary log-log link.")
      ),
      tags$h4("Bayesian-style"),
      tags$ul(tags$li(tags$b("Bayes:"), "Bayesian credible interval. Not a frequentist CI.")),
      tags$hr(),
      tags$p(tags$b("Rule-of-thumb:")),
      tags$ul(
        tags$li("Conservative planning → Exact (Clopper–Pearson)."),
        tags$li("Balanced default → Wilson or Agresti–Coull."),
        tags$li("Avoid Wald when n is small or p is near 0 or 1.")
      ),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"
    ))
  })

  observeEvent(input$eq_compare, {
    showModal(modalDialog(
      title = "CI method comparison",
      tags$p("The comparison table (enable via More Options → 'Show CI method comparison table')
              shows required total N for Wilson, Exact, Agresti–Coull, and Wald side-by-side
              at your current inputs."),
      tags$p("Key insight: method choice matters most when:"),
      tags$ul(
        tags$li("Event rates are close to 0 or 1."),
        tags$li("Sample sizes are small (N < ~50 per arm)."),
        tags$li("The NI margin is tight relative to p₁ − p₀.")
      ),
      easyClose = TRUE, footer = modalButton("Close"), size = "m"
    ))
  })

  # ── Core N function ───────────────────────────────────────────────────────────
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

  # ── Reactives ─────────────────────────────────────────────────────────────────
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

  # ── CI comparison reactive ─────────────────────────────────────────────────
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

  # ── N box ──────────────────────────────────────────────────────────────────
  output$n_box_prop <- renderUI({
    show_box <- if (is.null(input$showNBox_prop)) TRUE else isTRUE(input$showNBox_prop)
    if (!show_box) return(NULL)

    n_out <- prop_n_at_delta()
    method_label <- if (isTRUE(input$ci_method_prop == "z_power"))
      "Z (power formula)" else paste0("CI + simulation (", input$ci_method_prop, ")")

    msg <- if (is.infinite(n_out))
      paste0("Not achievable (N = ∞) under current assumptions.  [", method_label, "]")
    else
      paste0("Total N at Δ = ", sprintf("%.3f", input$p1.tolerable),
             "  →  N = ", format(n_out, big.mark = ","),
             "  [", method_label, "]")

    box_ui("Required sample size", msg)
  })

  # ── Shared plotly finishing touch ──────────────────────────────────────────
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

  # ── Δ plot ─────────────────────────────────────────────────────────────────
  output$plot1 <- renderPlotly({
    df <- prop_df_delta()

    validate(need(
      !all(is.infinite(df$y)),
      "No achievable sample size across this Δ range. Try widening Δ or adjusting p₀ / p₁."
    ))

    chosen_delta <- input$p1.tolerable
    chosen_n     <- prop_total_n(input$p0.expected, input$p1.expected, chosen_delta)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(colour = "#18bdb9", linewidth = 1.1) +
      geom_point(colour = "#18bdb9", size = 2) +
      geom_vline(xintercept = chosen_delta, linetype = "dashed",
                 colour = "#e07b39", linewidth = 0.9) +
      labs(
        title = "NI margin (Δ) vs total sample size",
        x = "Non-inferiority margin (Δ)",
        y = "Total sample size (N)"
      ) +
      plot_theme_large

    if (!is.infinite(chosen_n))
      p <- p + geom_point(
        data = data.frame(x = chosen_delta, y = chosen_n),
        aes(x = x, y = y), colour = "#e07b39", size = 5, shape = 21,
        fill = "#e07b39", alpha = 0.85
      )

    finish_plotly(p)
  })

  # ── p₁ plot ────────────────────────────────────────────────────────────────
  output$plot2 <- renderPlotly({
    df    <- prop_df_p1()
    is_one <- isTRUE(input$prop_design == "one_arm")

    validate(need(
      !all(is.infinite(df$y)),
      "No achievable sample size across this p₁ range under current assumptions."
    ))

    x_lab <- if (is_one)
      "Expected device event rate (p₁)" else "Expected experimental event rate (p₁)"

    chosen_p1 <- input$p1.expected
    chosen_n  <- prop_total_n(input$p0.expected, chosen_p1, input$p1.tolerable)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(colour = "#18bdb9", linewidth = 1.1) +
      geom_point(colour = "#18bdb9", size = 2) +
      geom_vline(xintercept = chosen_p1, linetype = "dashed",
                 colour = "#e07b39", linewidth = 0.9) +
      labs(
        title = "Expected event rate (p₁) vs total sample size",
        x = x_lab,
        y = "Total sample size (N)"
      ) +
      plot_theme_large

    if (!is.infinite(chosen_n))
      p <- p + geom_point(
        data = data.frame(x = chosen_p1, y = chosen_n),
        aes(x = x, y = y), colour = "#e07b39", size = 5, shape = 21,
        fill = "#e07b39", alpha = 0.85
      )

    finish_plotly(p)
  })

  # ── CI comparison section ──────────────────────────────────────────────────
  output$compare_section <- renderUI({
    if (!isTRUE(input$showCompare)) return(NULL)
    df <- compare_df()
    if (is.null(df)) return(NULL)

    tagList(
      tags$hr(),
      tags$div(
        class = "compare-header",
        tags$h5("CI method comparison — required total N at current inputs"),
        tags$p(
          class = "compare-params",
          paste0("p₀ = ", input$p0.expected, "  |  p₁ = ", input$p1.expected,
                 "  |  Δ = ", input$p1.tolerable, "  |  α = ", input$sig.level,
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

  # ── DT tables ─────────────────────────────────────────────────────────────
  output$dataTable <- renderDT({
    if (!isTRUE(input$showTable)) return(NULL)
    df <- prop_df_delta()
    colnames(df) <- c("NI Margin (Δ)", "Total N")
    DT::datatable(df, rownames = FALSE, class = "stripe hover compact",
                  options = list(pageLength = 15))
  })

  output$dataTable2 <- renderDT({
    if (!isTRUE(input$showTable2)) return(NULL)
    df <- prop_df_p1()
    colnames(df) <- c("Expected Event Rate (p₁)", "Total N")
    DT::datatable(df, rownames = FALSE, class = "stripe hover compact",
                  options = list(pageLength = 15))
  })

  # ── Downloads ──────────────────────────────────────────────────────────────
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

  # ── Word summary download ──────────────────────────────────────────────────
  output$downloadWord <- downloadHandler(
    filename    = function() paste0("PGPower_summary_", Sys.Date(), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content     = function(file) {
      tryCatch({
      if (!requireNamespace("officer", quietly = TRUE))
        stop("Please install the 'officer' package: install.packages('officer')")

      ci_method_used <- if (is.null(input$ci_method_prop)) "wilson" else input$ci_method_prop
      n_val <- prop_total_n(
        input$p0.expected, input$p1.expected, input$p1.tolerable,
        ci_method = ci_method_used,
        sim_n     = if (is.null(input$sim_quality)) 1000 else as.numeric(input$sim_quality),
        seed      = if (is.null(input$sim_seed))    1    else as.numeric(input$sim_seed)
      )

      ci_labels <- c(
        z_power    = "Z (Power Formula)",
        wilson     = "Wilson Score Interval",
        exact      = "Clopper-Pearson (Exact)",
        ac         = "Agresti-Coull",
        asymptotic = "Asymptotic (Wald)",
        prop.test  = "prop.test",
        bayes      = "Bayes",
        logit      = "Logit",
        cloglog    = "Cloglog",
        probit     = "Probit"
      )
      ci_label <- unname(ci_labels[ci_method_used])
      if (is.na(ci_label)) ci_label <- input$ci_method_prop

      summary_df <- data.frame(
        "p0"        = format(input$p0.expected, nsmall = 2),
        "p1"        = format(input$p1.expected, nsmall = 2),
        "Delta"     = sprintf("%.3f", input$p1.tolerable),
        "Alpha"     = format(as.numeric(input$sig.level)),
        "Power"     = format(input$power, nsmall = 2),
        "N"         = if (is.infinite(n_val)) "Not achievable" else format(n_val, big.mark = ","),
        "CI Method" = ci_label,
        check.names      = FALSE,
        stringsAsFactors = FALSE
      )
      colnames(summary_df) <- c("p₀", "p₁", "Δ", "α", "Power", "N", "CI Method")

      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "PG-Power — Trial Summary", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.Date(), "%d %B %Y")), style = "centered")
      doc <- officer::body_add_par(doc, paste("CI Method:", ci_label), style = "centered")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_table(doc, summary_df, align_table = "center")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_par(doc, "Represented Values:", style = "heading 3")
      doc <- officer::body_add_par(doc, "•  p₀: Required proportion of favorable outcomes in the control group. The trial must outperform this value.", style = "Normal")
      doc <- officer::body_add_par(doc, "•  p₁: Expected proportion of favorable outcomes in the trial.", style = "Normal")
      doc <- officer::body_add_par(doc, "•  Δ NI Margin: The pre-specified maximum allowable difference in efficacy between a new treatment and an active comparator.", style = "Normal")
      doc <- officer::body_add_par(doc, "•  α: Significance Level: The trial’s risk of a false positive.", style = "Normal")
      doc <- officer::body_add_par(doc, "•  Power: The trial’s ability to demonstrate efficacy, if correct.", style = "Normal")
      doc <- officer::body_add_par(doc, "•  N: Minimal Sample Size required to correctly power the trial.", style = "Normal")

      print(doc, target = file)
      }, error = function(e) {
        message("downloadWord ERROR: ", conditionMessage(e))
      })
    }
  )
}

