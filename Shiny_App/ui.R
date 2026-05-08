ui <- fluidPage(
  theme = default_mode,

  # ── Head: fonts + custom CSS ─────────────────────────────────────────────────
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,600;1,9..40,300&family=DM+Mono:wght@400;500&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "custom.css")
  ),

  add_busy_spinner(spin = "fading-circle", color = "#18bdb9", position = "top-right"),

  # ── Header ───────────────────────────────────────────────────────────────────
  tags$div(
    class = "pgp-header",
    tags$img(src = "pg_power_logo.png", height = "68px"),
    tags$div(
      tags$h1("PG-Power", class = "pgp-title"),
      tags$p("Performance-goal trial planning — clear, fast, flexible.", class = "pgp-subtitle")
    )
  ),

  # ── Tabs ─────────────────────────────────────────────────────────────────────
  tabsetPanel(
    id = "main_tabs",

    # ── Tab 1: Set up ──────────────────────────────────────────────────────────
    tabPanel(
      title = "Set up",
      sidebarLayout(
        sidebarPanel(
          class = "pgp-sidebar",
          checkboxInput("dark_mode_on", "Dark mode", value = FALSE),
          tags$hr(class = "pgp-hr"),

          tags$p("Trial design settings", class = "sidebar-section-label"),

          selectInput(
            "prop_design", "Design",
            choices = c(
              "Two-arm NI (treatment vs control)"      = "two_arm",
              "Single-arm NI (treatment vs benchmark)" = "one_arm"
            ),
            selected = "two_arm"
          ),

          selectInput(
            "endpoint", "Endpoint",
            choices = c("Efficacy" = "efficacy", "Safety" = "safety"),
            selected = "efficacy"
          ),

          selectInput(
            "sig.level", "Significance level (one-sided α)",
            choices = c("0.025" = 0.025, "0.04" = 0.04, "0.05" = 0.05),
            selected = 0.025
          ),

          sliderInput(
            "power", "Power",
            min = 0.80, max = 0.95, step = 0.05, value = 0.90
          ),

          conditionalPanel(
            condition = "input.prop_design != 'one_arm'",
            selectInput(
              "r", "Allocation ratio (treatment : control)",
              choices = c("1:1" = 1, "2:1" = 2, "3:1" = 3, "4:1" = 4, "5:1" = 5),
              selected = 1
            )
          ),

          selectInput(
            "WindowMargin", "Sensitivity window for NI margin (±)",
            choices = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05),
            selected = 0.05
          )
        ),

        mainPanel(
          class = "pgp-main",

          tags$div(
            class = "info-card",
            tags$h5("Overview"),
            tags$p("PG-Power explores sample size requirements for non-inferiority /
                    performance-goal trials using sensitivity plots."),
            tags$p("This tab sets trial-level choices (one-sided α, power, allocation ratio)
                    that apply across the Proportions tab.")
          ),

          tags$div(
            class = "info-card",
            tags$h5("How to use"),
            tags$ol(
              tags$li("Choose α, power, and (for two-arm designs) allocation ratio on the left."),
              tags$li("Pick a sensitivity window (±) to control how wide the Δ curve is around your chosen margin."),
              tags$li("Go to the Proportions tab to enter outcome assumptions and view plots/tables."),
              tags$li(HTML("In Proportions → <em>More Options</em>, choose a CI method.
                     'Z (power formula)' uses an analytic approximation; all other methods use simulation."))
            )
          ),

          tags$div(
            class = "info-card",
            tags$h5("Methods"),
            tags$ul(
              tags$li(tags$b("Z (power formula): "),
                      "Standard z-approximation for a non-inferiority test on the risk-difference scale.
                       Fast; uses the Power slider directly."),
              tags$li(tags$b("CI methods (Wilson, Exact, AC, …): "),
                      "Estimates power by simulation. N is increased until simulated power reaches the target.")
            ),
            tags$hr(class = "pgp-hr"),
            tags$p("For CI-based methods the app constructs a conservative lower bound for
                    RD = p₁ − p₀ from two separate proportion CIs:"),
            tags$pre(class = "pgp-pre", "Lower(RD) = Lower(p₁) − Upper(p₀)\nDeclare NI if  Lower(RD) > −Δ"),
            tags$p("For single-arm designs:"),
            tags$pre(class = "pgp-pre", "Declare NI if  Lower(p) > p₀ − Δ")
          ),

          tags$div(
            class = "info-card",
            tags$h5("Equation reference"),
            tags$div(
              class = "eq-btn-row",
              actionButton("eq_ci_prop", "CI methods",           class = "btn-sm btn-outline-primary pgp-btn"),
              actionButton("eq_prop1",   "Δ plot",               class = "btn-sm btn-outline-primary pgp-btn"),
              actionButton("eq_prop2",   "p₁ plot",              class = "btn-sm btn-outline-primary pgp-btn"),
              actionButton("eq_compare", "CI method comparison", class = "btn-sm btn-outline-primary pgp-btn")
            )
          ),

          tags$div(
            class = "info-card",
            tags$h5("Interpretation"),
            tags$ul(
              tags$li("Δ plots show how tightening/loosening the NI margin changes required total N."),
              tags$li("p₁ plots show sensitivity to assumed device performance."),
              tags$li("The CI comparison table shows how method choice affects N — most important near p = 0 or 1."),
              tags$li("The dashed orange line marks your currently chosen parameter value on each plot.")
            )
          ),

          tags$div(
            class = "info-card pgp-footer-card",
            tags$p(
              HTML("Developed by <strong>Filip Kłosowski</strong> and
                    <strong>Áine Glynn</strong> · University of Galway")
            ),
            tags$a(
              href = "https://github.com/FilipMKgit/Margin-Jinn",
              target = "_blank",
              "View on GitHub ↗"
            )
          )
        )
      )
    ),

    # ── Tab 2: Proportions ─────────────────────────────────────────────────────
    tabPanel(
      title = "Proportions",
      sidebarLayout(
        sidebarPanel(
          class = "pgp-sidebar",

          sliderInput("p0.expected",
            "Control / benchmark event rate (p₀):",
            min = 0.00, max = 1.00, step = 0.01, value = 0.90),

          sliderInput("p1.expected",
            "Expected device / experimental event rate (p₁):",
            min = 0.00, max = 1.00, step = 0.01, value = 0.80),

          sliderInput("p1.tolerable",
            "Non-inferiority margin (Δ):",
            min = 0.00, max = 0.20, step = 0.01, value = 0.05),

          tags$p("α, power, and allocation ratio are set in the Set up tab.",
                 class = "pgp-help-text"),

          tags$div(
            style = "margin-top:18px;",
            checkboxInput("showExtras_prop", "More Options", value = FALSE)
          ),

          conditionalPanel(
            condition = "input.showExtras_prop == true",
            tags$hr(class = "pgp-hr"),

            selectInput(
              "ci_method_prop", "Method (drives the sample size)",
              choices = c(
                "Z (power formula)"         = "z_power",
                "Wilson Score Interval"     = "wilson",
                "Clopper-Pearson ('exact')" = "exact",
                "Agresti-Coull"             = "ac",
                "Asymptotic (Wald)"         = "asymptotic",
                "prop.test"                 = "prop.test",
                "Bayes"                     = "bayes",
                "Logit"                     = "logit",
                "Cloglog"                   = "cloglog",
                "Probit"                    = "probit"
              ),
              selected = "wilson"
            ),

            conditionalPanel(
              condition = "input.ci_method_prop != 'z_power'",
              selectInput(
                "sim_quality", "Simulation quality",
                choices = c(
                  "Fast (400 sims)"       = 400,
                  "Normal (1 000 sims)"   = 1000,
                  "Accurate (3 000 sims)" = 3000
                ),
                selected = 1000
              ),
              numericInput("sim_seed", "Simulation seed", value = 1, min = 1, step = 1)
            ),

            tags$hr(class = "pgp-hr"),

            checkboxInput("showNBox_prop", "Show N at chosen Δ",            value = TRUE),
            checkboxInput("showCompare",   "Show CI method comparison table", value = FALSE),
            checkboxInput("showTable",     "Show Δ sensitivity table",        value = FALSE),
            checkboxInput("showTable2",    "Show p₁ sensitivity table",       value = FALSE),

            tags$div(
              class = "dl-btn-col",
              downloadButton("downloadData_plot1", "↓ Download Δ table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadData_plot2", "↓ Download p₁ table",
                             class = "btn-sm btn-outline-primary pgp-btn")
            )
          )
        ),

        mainPanel(
          class = "pgp-main",
          plotlyOutput("plot1", height = "420px"),
          uiOutput("n_box_prop"),
          tags$div(style = "height:22px;"),
          plotlyOutput("plot2", height = "420px"),
          uiOutput("compare_section"),
          DTOutput("dataTable"),
          DTOutput("dataTable2")
        )
      )
    )
  )
)
