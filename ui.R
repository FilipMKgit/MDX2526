ui <- fluidPage(
  theme = default_mode,
  

  #header
 
  tags$div(
    style = "
      display:flex;
      align-items:center;
      gap:14px;
      margin-bottom:6px;
    ",
    tags$img(src = "margin-jinn_logo_2.png", height = "100px"),
    tags$div(
      tags$h1(
        "Margin-Jinn",
        style = "
          color:#18bdb9;
          font-size:38px;
          font-weight:600;
          letter-spacing:1px;
          margin-bottom:4px;
        "
      ),
      tags$h4(
        "Your wish for small N — granted.",
        style = "
          color:#666666;
          font-weight:300;
          margin-top:0;
        "
      )
    )
  ),
  

  tabsetPanel(
    
    
    #tab 1: set up
    ################################################################
    tabPanel(
      title = "Set up",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("dark_mode_on", "Dark mode", value = FALSE),
          tags$hr(),
          h4("Trial design settings"),
          selectInput(
            "prop_design",
            "Design",
            choices = c(
              "Two-arm NI (treatment vs control)" = "two_arm",
              "Single-arm NI (treatment vs benchmark)" = "one_arm"
            ),
            selected = "two_arm"
          ),
          
          selectInput(
            "endpoint",
            "Endpoint",
            choices = c(
              "Efficacy" = "efficacy",
              "Safety" = "safety"
            ),
            selected = "efficacy"
          ),
          
          
          
          selectInput(
            "sig.level",
            "Significance level (one-sided α)",
            choices = c("0.025" = 0.025, "0.04" = 0.04, "0.05" = 0.05),
            selected = 0.025
          ),
          sliderInput(
            "power",
            "Power",
            min = 0.80, max = 0.95, step = 0.05,
            value = 0.90
          ),
          selectInput(
            "r",
            "Allocation ratio (treatment : control)",
            choices = c("1:1" = 1, "2:1" = 2, "3:1" = 3, "4:1" = 4, "5:1" = 5),
            selected = 1
          ),
          selectInput(
            "WindowMargin",
            "Sensitivity window for NI margin (±)",
            choices = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05),
            selected = 0.05
          )
        ),
        mainPanel(
          h3("Overview"),
          p("Margin-Jinn explores sample size requirements for non-inferiority trials using sensitivity plots."),
          p("This tab sets trial-level choices (one-sided α, power, allocation ratio) that apply across both the Proportions and Continuous tabs."),
          
          h4("How to use"),
          tags$ol(
            tags$li("Choose α, power, and allocation ratio on the left."),
            tags$li("Pick a sensitivity window (±) to control how wide the Δ curve is around your chosen margin."),
            tags$li("Go to Proportions (binary) or Continuous (means) to enter outcome assumptions and view plots/tables."),
            tags$li("In Proportions → More Options, choose a method. 'Z (power formula)' uses an analytic approximation; the CI methods use simulation.")
          ),
          
          h4("What the Proportions methods mean"),
          tags$ul(
            tags$li(tags$b("Z (power formula): "), "Uses a standard z-approximation sample size formula for a non-inferiority test on the risk-difference scale. This is fast and uses the Power slider directly."),
            tags$li(tags$b("CI methods (exact, Wilson, AC, etc.): "), "Use the chosen confidence interval method for each group proportion and then estimate the power by simulation. The app increases N until the simulated probability of concluding non-inferiority reaches the target Power.")
          ),
          
          h4("Confidence intervals in this app (Proportions)"),
          p("For CI-based methods, the app constructs a conservative lower bound for the risk difference (RD = p₁ − p₀) using two separate proportion confidence intervals:"),
          tags$pre("Lower(RD) = Lower(p₁) − Upper(p₀)"),
          p("Non-inferiority is concluded when this lower bound is above the non-inferiority boundary:"),
          tags$pre("Conclude NI if  Lower(RD) > −Δ"),
          p("This approach makes the selected CI method matter, because different CI methods produce slightly different lower/upper bounds (especially at small sample sizes or extreme event rates)."),
          
          h4("Interpretation"),
          tags$ul(
            tags$li("Δ plots show how tightening/loosening the NI margin changes the required total sample size."),
            tags$li("p₁ / μ₁ plots show how sensitive results are to assumed experimental performance."),
            tags$li("For Proportions, switching the CI method can change N, especially when p is near 0/1 or when N is small.")
          ),
          
          h4("Equations"),
          tags$div(
            style = "display:flex; flex-wrap:wrap; gap:10px;",
            actionButton("eq_ci_prop", "Proportions: CI methods", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            actionButton("eq_prop1", "Proportions: Δ plot", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            actionButton("eq_prop2", "Proportions: p₁ plot", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            actionButton("eq_mean1", "Continuous: Δ plot", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            actionButton("eq_mean2", "Continuous: μ₁ plot", class = "btn-sm btn-outline-primary", style = "width:220px;")
          ),
          
          tags$hr(),
          
          h4("Note"),
          tags$ul(
            tags$li("Outputs are for planning and sensitivity exploration."),
            tags$li("CI-based methods use simulation, so results can vary slightly with the simulation seed and number of simulations."),
            tags$li("Final design choices should be justified clinically and statistically.")
          ),
          
          tags$hr(),
          tags$p(
            style = "font-size:13px; color:#777777; line-height:1.4;",
            HTML("Developed by <strong>Filip Kłosowski</strong> and <strong>Áine Glynn</strong><br/>University of Galway")
          ),
          tags$p(
            style = "font-size:13px;",
            tags$a(
              href = "https://github.com/FilipMKgit/Margin-Jinn",
              target = "_blank",
              "View the Margin-Jinn GitHub repository (open source)"
            )
          )
        )
      )
    ),
    
    
    #tab 2: proportions
    ################################################################
    tabPanel(
      title = "Proportions",
      sidebarLayout(
        sidebarPanel(
          
          sliderInput(
            "p0.expected",
            "Control event rate (p0):",
            min = 0.00, max = 1.00, step = 0.01,
            value = 0.90
          ),
          sliderInput("p1.expected", "Expected experimental event rate (p1):", min = 0.00, max = 1.00, step = 0.01, value = 0.80),
          sliderInput(
            "p1.tolerable",
            "Non-inferiority margin (Δ):",
            min = 0.00, max = 0.20, step = 0.01,
            value = 0.05
          ),
          
          helpText("Note: Alpha, power and allocation ratio can be changed in the set up tab."),
          
          tags$div(
            checkboxInput("showExtras_prop", "More Options", value = FALSE),
            style = "margin-top:15px;"
          ),
          
          conditionalPanel(
            condition = "input.showExtras_prop == true",
            tags$hr(),
            selectInput(
              "ci_method_prop",
              "Method (drives the sample size)",
              choices = c(
                "Z (power formula)" = "z_power",
                "exact (Clopper-Pearson)" = "exact",
                "ac (Agresti-Coull)" = "ac",
                "asymptotic (Wald, binom)" = "asymptotic",
                "wilson" = "wilson",
                "prop.test" = "prop.test",
                "bayes" = "bayes",
                "logit" = "logit",
                "cloglog" = "cloglog",
                "probit" = "probit"
              ),
              selected = "wilson"
            ),
            conditionalPanel(
              condition = "input.ci_method_prop != 'z_power'",
              selectInput(
                "sim_quality",
                "Simulation quality",
                choices = c("Fast (400 sims)" = 400, "Normal (1000 sims)" = 1000, "Accurate (3000 sims)" = 3000),
                selected = 1000
              ),
              numericInput("sim_seed", "Simulation seed", value = 1, min = 1, step = 1)
            ),
            tags$hr(),
            checkboxInput("showNBox_prop", "Show N at chosen Δ", value = TRUE),
            checkboxInput("showTable", "Show Δ table", value = FALSE),
            checkboxInput("showTable2", "Show p₁ table", value = FALSE),
            downloadButton("downloadData_plot1", "Download Δ table", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            downloadButton("downloadData_plot2", "Download p₁ table", class = "btn-sm btn-outline-primary", style = "width:220px;")
          )
        ),
        mainPanel(
          plotlyOutput("plot1", height = "420px"),
          uiOutput("n_box_prop"),
          tags$div(style = "height:18px;"),
          plotlyOutput("plot2", height = "420px"),
          DTOutput("dataTable"),
          DTOutput("dataTable2")
        )
      )
    ),
    
    
    #tab 3: continuous
    ################################################################
    tabPanel(
      title = "Continuous",
      sidebarLayout(
        sidebarPanel(
          numericInput("mu0", "Control mean (μ0):", value = 50),
          numericInput("mu1", "Expected experimental mean (μ1):", value = 50),
          numericInput("sd", "Common SD:", value = 10, min = 1, step = 1),
          numericInput("delta", "Non-inferiority margin (Δ):", value = 5, min = 0, step = 1),
          helpText("Note: Alpha, power and allocation ratio can be changed in the set up tab."),
          tags$div(
            checkboxInput("showExtras_mean", "More Options", value = FALSE),
            style = "margin-top:15px;"
          ),
          conditionalPanel(
            condition = "input.showExtras_mean == true",
            checkboxInput("showNBox_mean", "Show N at chosen Δ", value = TRUE),
            checkboxInput("showTable_mean", "Show Δ sensitivity table", value = FALSE),
            checkboxInput("showTable_mean2", "Show μ1 sensitivity table", value = FALSE),
            downloadButton("downloadData_mean1", "Download Δ sensitivity table", class = "btn-sm btn-outline-primary", style = "width:220px;"),
            downloadButton("downloadData_mean2", "Download μ₁ table", class = "btn-sm btn-outline-primary", style = "width:220px;")
          )
        ),
        mainPanel(
          plotlyOutput("plot_mean", height = "420px"),
          uiOutput("n_box_mean"),
          tags$div(style = "height:18px;"),
          plotlyOutput("plot_mean2", height = "420px"),
          DTOutput("dataTable_mean"),
          DTOutput("dataTable_mean2")
        )
      )
    )
  )
)
