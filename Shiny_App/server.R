server <- function(input, output, session) {
  
  endpoint_defaults <- list(
    efficacy = list(p0 = 0.88, p1 = 0.93, delta = 0.05, window = 0.05),
    safety   = list(p0 = 0.95, p1 = 0.97, delta = 0.02, window = 0.02)
  )
  
  observeEvent(input$endpoint, {
    d <- endpoint_defaults[[input$endpoint]]
    if (is.null(d)) return(NULL)
    
    updateSliderInput(session, "p0.expected", value = d$p0)
    updateSliderInput(session, "p1.expected", value = d$p1)
    updateSliderInput(session, "p1.tolerable", value = d$delta)
    updateSelectInput(session, "WindowMargin", selected = as.character(d$window))
  }, ignoreInit = TRUE)
  
  observeEvent(input$dark_mode_on, {
    if (isTRUE(input$dark_mode_on)) {
      session$setCurrentTheme(dark_mode)
    } else {
      session$setCurrentTheme(default_mode)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$eq_prop1, {
    is_one <- isTRUE(input$prop_design == "one_arm")
    
    showModal(modalDialog(
      title = "Proportions: Δ vs total sample size",
      if (!is_one) {
        tagList(
          tags$p("Hypotheses (risk difference):"),
          tags$pre("H0: (p1 − p0) ≤ −Δ\nH1: (p1 − p0) > −Δ"),
          tags$p("How N is computed in this app:"),
          tags$ul(
            tags$li("If Method = 'Z (power formula)': analytic z approximation using the power slider."),
            tags$li("Otherwise: CI-rule + simulation to hit target power (power slider).")
          ),
          tags$p("CI-rule used for simulation methods (conservative):"),
          tags$pre("Compute CI(p0) and CI(p1) using chosen method.\nLower(RD) = Lower(p1) − Upper(p0).\nDeclare NI if Lower(RD) > −Δ.")
        )
      } else {
        tagList(
          tags$p("Hypotheses (single-arm vs benchmark):"),
          tags$pre("H0: p ≤ (p0 − Δ)\nH1: p > (p0 − Δ)"),
          tags$p("How N is computed in this app:"),
          tags$ul(
            tags$li("If Method = 'Z (power formula)': analytic z approximation using the power slider."),
            tags$li("Otherwise: CI-rule + simulation to hit target power (power slider).")
          ),
          tags$p("Decision rule for simulation methods:"),
          tags$pre("Compute CI(p) using chosen method.\nDeclare NI if Lower(p) > (p0 − Δ).")
        )
      },
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$eq_prop2, {
    is_one <- isTRUE(input$prop_design == "one_arm")
    
    showModal(modalDialog(
      title = "Proportions: p₁ vs total sample size",
      if (!is_one) {
        tagList(
          tags$p("Hypotheses (risk difference):"),
          tags$pre("H0: (p1 − p0) ≤ −Δ\nH1: (p1 − p0) > −Δ"),
          tags$p("What changes in this plot:"),
          tags$pre("p1 varies; Δ and p0 are held fixed."),
          tags$p("Decision rule for simulation methods:"),
          tags$pre("Lower(RD) = Lower(p1) − Upper(p0).\nDeclare NI if Lower(RD) > −Δ.")
        )
      } else {
        tagList(
          tags$p("Hypotheses (single-arm vs benchmark):"),
          tags$pre("H0: p ≤ (p0 − Δ)\nH1: p > (p0 − Δ)"),
          tags$p("What changes in this plot:"),
          tags$pre("p1 varies; Δ and p0 are held fixed."),
          tags$p("Decision rule for simulation methods:"),
          tags$pre("Declare NI if Lower(p) > (p0 − Δ).")
        )
      },
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$eq_ci_prop, {
    showModal(modalDialog(
      title = "Proportions: confidence interval methods (what they mean)",
      tags$p("These affect the CI bounds used in the simulation-based sizing methods found under 'more options' in tab 2."),
      tags$hr(),
      
      tags$h4("Common methods"),
      tags$ul(
        tags$li(tags$b("wilson:"), "Wilson score interval. Usually a strong default: good coverage, behaves well for small n and p near 0/1."),
        tags$li(tags$b("exact (Clopper–Pearson):"), "Inverts the binomial test. Guaranteed conservative (often wider CI → larger N)."),
        tags$li(tags$b("ac (Agresti–Coull):"), "Adds a small correction ('add 2 successes + 2 failures'). Often close to Wilson, more stable than Wald."),
        tags$li(tags$b("asymptotic (Wald):"), "Simple normal approximation p ± z*SE. Can be inaccurate for small n or extreme p (often too narrow).")
      ),
      
      tags$h4("Test-based interval"),
      tags$ul(
        tags$li(tags$b("prop.test:"), "Score/chi-squared based interval (like R’s prop.test). Generally better than Wald; still approximation-based.")
      ),
      
      tags$h4("Model-based intervals"),
      tags$ul(
        tags$li(tags$b("logit:"), "CI formed on log-odds scale then transformed back. Can behave better near 0/1."),
        tags$li(tags$b("probit:"), "Same idea but using probit link."),
        tags$li(tags$b("cloglog:"), "Complementary log-log link. Sometimes used for skewed risk shapes; can behave differently near extremes.")
      ),
      
      tags$h4("Bayesian-style option"),
      tags$ul(
        tags$li(tags$b("bayes:"), "Uses a Bayesian credible interval (depends on the prior used by the method). Often reasonable, but it’s not a frequentist CI.")
      ),
      
      tags$hr(),
      tags$p(tags$b("Rule-of-thumb:")),
      tags$ul(
        tags$li("If you want conservative planning: use Exact."),
        tags$li("If you want a balanced default: use Wilson (or Agresti–Coull)."),
        tags$li("Avoid Wald when n is small or p is near 0 or 1.")
      ),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  observeEvent(input$eq_mean1, {
    showModal(modalDialog(
      title = "Continuous: Δ vs total sample size",
      tags$p("Hypotheses (mean difference):"),
      tags$pre("H0: (μ1 − μ0) ≤ −Δ\nH1: (μ1 − μ0) > −Δ"),
      tags$p("Sample size formula (common SD):"),
      tags$pre(paste0(
        "Let δ = (μ1 − μ0)\n",
        "eff = δ + Δ\n\n",
        "zα = qnorm(1 − α)\n",
        "zβ = qnorm(power)\n\n",
        "n0 = ((1 + 1/r) * σ² * (zα + zβ)²) / eff²\n",
        "n1 = r * n0\n",
        "Total N = ceil(n0) + ceil(n1)\n\n",
        "If eff ≤ 0 then NI not achievable → N = Inf"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$eq_mean2, {
    showModal(modalDialog(
      title = "Continuous: μ₁ vs total sample size",
      tags$p("Hypotheses (mean difference):"),
      tags$pre("H0: (μ1 − μ0) ≤ −Δ\nH1: (μ1 − μ0) > −Δ"),
      tags$p("What changes in this plot:"),
      tags$pre("μ1 varies; Δ, μ0 and σ are held fixed."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$prop_design, {
    p0_lab <- if (isTRUE(input$prop_design == "one_arm")) {
      "Benchmark / performance goal (p0):"
    } else {
      "Control event rate (p0):"
    }
    
    updateSliderInput(session, "p0.expected", label = p0_lab)
    
    delta_lab <- if (isTRUE(input$prop_design == "one_arm")) {
      "Non-inferiority margin (Δ, against benchmark):"
    } else {
      "Non-inferiority margin (Δ, risk difference):"
    }
    
    updateSliderInput(session, "p1.tolerable", label = delta_lab)
  }, ignoreInit = FALSE)
  
  n_1arm_z_superiority <- function(p0, p1, alpha, power) {
    if (p1 <= p0) return(Inf)
    z_alpha <- qnorm(1 - alpha)
    z_beta  <- qnorm(power)
    num <- (z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1)))^2
    den <- (p1 - p0)^2
    ceiling(num / den)
  }
  
  prop_total_n <- function(p0, p1, delta) {
    if (isTRUE(input$prop_design == "one_arm")) {
      if (isTRUE(input$ci_method_prop == "z_power")) {
        alpha <- as.numeric(input$sig.level)
        
        if (isTRUE(all.equal(delta, 0))) {
          return(n_1arm_z_superiority(
            p0 = p0,
            p1 = p1,
            alpha = alpha,
            power = input$power
          ))
        }
        
        return(total_sample_size_prop_1arm(
          p0 = p0,
          p1 = p1,
          delta = delta,
          sig.level = alpha,
          power = input$power
        ))
      }
      
      return(total_sample_size_prop_ci_power_1arm(
        p0 = p0,
        p1 = p1,
        delta = delta,
        alpha = as.numeric(input$sig.level),
        power = input$power,
        ci_method = input$ci_method_prop,
        nsim = as.numeric(input$sim_quality),
        seed = as.numeric(input$sim_seed)
      ))
    }
    
    if (isTRUE(input$ci_method_prop == "z_power")) {
      return(total_sample_size_prop(
        p0 = p0,
        p1 = p1,
        delta = delta,
        sig.level = as.numeric(input$sig.level),
        power = input$power,
        r = as.numeric(input$r)
      ))
    }
    
    total_sample_size_prop_ci_power(
      p0 = p0,
      p1 = p1,
      delta = delta,
      alpha = as.numeric(input$sig.level),
      power = input$power,
      r = as.numeric(input$r),
      ci_method = input$ci_method_prop,
      nsim = as.numeric(input$sim_quality),
      seed = as.numeric(input$sim_seed)
    )
  }
  
  box_ui <- function(title, msg) {
    tags$div(
      style = "border:1px solid #999; border-radius:12px; padding:12px; margin-top:10px; margin-bottom:14px;",
      tags$div(style = "font-weight:700; font-size:16px;", title),
      tags$div(style = "margin-top:6px; font-size:15px;", msg)
    )
  }
  
  prop_df_delta <- reactive({
    window <- as.numeric(input$WindowMargin)
    x_min <- max(0.00, input$p1.tolerable - window)
    x_max <- min(0.20, input$p1.tolerable + window)
    x <- seq(from = x_min, to = x_max, by = 0.005)
    y <- sapply(x, function(d) prop_total_n(input$p0.expected, input$p1.expected, d))
    data.frame(x = x, y = y)
  })
  
  prop_df_p1 <- reactive({
    x_min <- max(0.001, input$p1.expected - 0.10)
    x_max <- min(0.999, input$p1.expected + 0.10)
    x <- seq(from = x_min, to = x_max, by = 0.005)
    y <- sapply(x, function(p1i) prop_total_n(input$p0.expected, p1i, input$p1.tolerable))
    data.frame(x = x, y = y)
  })
  
  prop_n_at_delta <- reactive({
    prop_total_n(input$p0.expected, input$p1.expected, input$p1.tolerable)
  })
  
  output$n_box_prop <- renderUI({
    show_box <- if (is.null(input$showNBox_prop)) TRUE else isTRUE(input$showNBox_prop)
    if (!show_box) return(NULL)
    
    n_out <- prop_n_at_delta()
    
    method_label <- if (isTRUE(input$ci_method_prop == "z_power")) {
      "Z (power formula)"
    } else {
      paste0("CI + simulation (", input$ci_method_prop, ")")
    }
    
    msg <- if (is.infinite(n_out)) {
      paste0("Not achievable (N = Inf) under current assumptions.  [", method_label, "]")
    } else {
      paste0(
        "Total sample size at Δ = ",
        sprintf("%.3f", input$p1.tolerable),
        " is:  N = ",
        format(n_out, big.mark = ","),
        "  [", method_label, "]"
      )
    }
    
    box_ui("N at chosen margin (Δ)", msg)
  })
  
  output$plot1 <- renderPlotly({
    df <- prop_df_delta()
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Δ vs total sample size",
        x = "Non-inferiority margin (Δ)",
        y = "Total sample size"
      ) +
      plot_theme_large
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE)
  })
  
  output$plot2 <- renderPlotly({
    df <- prop_df_p1()
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_point() +
      labs(
        title = "p₁ vs total sample size",
        x = "Expected experimental event rate (p₁)",
        y = "Total sample size"
      ) +
      plot_theme_large
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE)
  })
  
  output$dataTable <- renderDT({
    if (!isTRUE(input$showTable)) return(NULL)
    
    df <- prop_df_delta()
    colnames(df) <- c("NI Margin (Δ)", "Total Sample Size (N)")
    
    DT::datatable(
      df,
      options = list(stripe = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
    )
  })
  
  output$dataTable2 <- renderDT({
    if (!isTRUE(input$showTable2)) return(NULL)
    
    df <- prop_df_p1()
    colnames(df) <- c("Expected Experimental Event Rate (p₁)", "Total Sample Size (N)")
    
    DT::datatable(
      df,
      options = list(stripe = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
    )
  })
  
  output$downloadData_plot1 <- downloadHandler(
    filename = function() paste0("NI_margin_table_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- prop_df_delta()
      colnames(df) <- c("NI Margin (Δ)", "Total Sample Size (N)")
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$downloadData_plot2 <- downloadHandler(
    filename = function() paste0("expected_performance_table_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- prop_df_p1()
      colnames(df) <- c("Expected Experimental Event Rate (p₁)", "Total Sample Size (N)")
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  cont_df_delta <- reactive({
    req(input$mu0, input$mu1, input$sd, input$delta, input$power, input$sig.level, input$r)
    
    delta_seq <- seq(from = input$delta - 0.20, to = input$delta, by = 0.05)
    delta_seq <- delta_seq[delta_seq > 0]
    
    y <- sapply(delta_seq, function(d) {
      total_sample_size_mean(
        mu0 = input$mu0,
        mu1 = input$mu1,
        sd = input$sd,
        delta = d,
        sigma = as.numeric(input$sig.level),
        power = input$power,
        r = as.numeric(input$r)
      )
    })
    
    data.frame(delta = delta_seq, total_n = y)
  })
  
  cont_df_mu1 <- reactive({
    req(input$mu0, input$mu1, input$sd, input$delta, input$power, input$sig.level, input$r)
    
    mu1_seq <- seq(from = input$mu1 - 5, to = input$mu1 + 5, by = 1)
    
    y <- sapply(mu1_seq, function(m1) {
      total_sample_size_mean(
        mu0 = input$mu0,
        mu1 = m1,
        sd = input$sd,
        delta = input$delta,
        sigma = as.numeric(input$sig.level),
        power = input$power,
        r = as.numeric(input$r)
      )
    })
    
    data.frame(mu1 = mu1_seq, total_n = y)
  })
  
  cont_n_at_delta <- reactive({
    req(input$mu0, input$mu1, input$sd, input$delta, input$power, input$sig.level, input$r)
    total_sample_size_mean(
      mu0 = input$mu0,
      mu1 = input$mu1,
      sd = input$sd,
      delta = input$delta,
      sigma = as.numeric(input$sig.level),
      power = input$power,
      r = as.numeric(input$r)
    )
  })
  
  output$n_box_mean <- renderUI({
    if (!isTRUE(input$showNBox_mean)) return(NULL)
    
    n_out <- cont_n_at_delta()
    
    msg <- if (is.infinite(n_out)) {
      "Not achievable (N = Inf) under current assumptions."
    } else {
      paste0(
        "Total sample size at Δ = ",
        sprintf("%.3f", input$delta),
        " is:  N = ",
        format(n_out, big.mark = ",")
      )
    }
    
    box_ui("N at chosen margin (Δ)", msg)
  })
  
  output$plot_mean <- renderPlotly({
    df <- cont_df_delta()
    
    p <- ggplot(df, aes(x = delta, y = total_n)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Δ vs total sample size",
        x = "Non-inferiority margin (Δ)",
        y = "Total sample size (N)"
      ) +
      plot_theme_large
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE)
  })
  
  output$plot_mean2 <- renderPlotly({
    df <- cont_df_mu1()
    
    p <- ggplot(df, aes(x = mu1, y = total_n)) +
      geom_line() +
      geom_point() +
      labs(
        title = "μ₁ vs total sample size",
        x = "Assumed experimental mean (μ₁)",
        y = "Total sample size (N)"
      ) +
      plot_theme_large
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE)
  })
  
  output$dataTable_mean <- renderDT({
    if (!isTRUE(input$showTable_mean)) return(NULL)
    
    df <- cont_df_delta()
    colnames(df) <- c("Non-Inferiority Margin (Δ)", "Total Sample Size (N)")
    
    DT::datatable(
      df,
      options = list(stripe = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
    )
  })
  
  output$dataTable_mean2 <- renderDT({
    if (!isTRUE(input$showTable_mean2)) return(NULL)
    
    df <- cont_df_mu1()
    colnames(df) <- c("Assumed Experimental Mean (μ₁)", "Total Sample Size (N)")
    
    DT::datatable(
      df,
      options = list(stripe = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
    )
  })
  
  output$downloadData_mean1 <- downloadHandler(
    filename = function() paste0("mean_NI_margin_table_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cont_df_delta()
      colnames(df) <- c("Non-Inferiority Margin (Δ)", "Total Sample Size (N)")
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$downloadData_mean2 <- downloadHandler(
    filename = function() paste0("assumed_mean_table_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cont_df_mu1()
      colnames(df) <- c("Assumed Experimental Mean (μ₁)", "Total Sample Size (N)")
      write.csv(df, file, row.names = FALSE)
    }
  )
}
