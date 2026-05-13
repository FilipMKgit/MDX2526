# -- Accordion builder (defined here so ui.R can call it at parse time) --------
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

ui <- fluidPage(
  theme = default_mode,
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,600;1,9..40,300&family=DM+Mono:wght@400;500&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "custom.css"),
    
    tags$style(HTML("
      .pgp-accordion { margin-bottom: 10px; border-radius: 8px; overflow: visible;
                        border: 1px solid #e2e8f0; position: relative; }
      .pgp-accordion-header {
        display: flex; align-items: center; justify-content: space-between;
        padding: 11px 16px; cursor: pointer; user-select: none;
        background: #f8fafc; font-weight: 600; font-size: 13.5px;
        color: #1a2e35; letter-spacing: 0.01em;
        border-bottom: 1px solid transparent; transition: background 0.15s;
        border-radius: 8px 8px 0 0;
      }
      .pgp-accordion-header:hover { background: #eef3f8; }
      .pgp-accordion-header.open  { background: #eef3f8; border-bottom-color: #e2e8f0; }
      .pgp-accordion-chevron { font-size: 11px; transition: transform 0.2s; color: #64748b; }
      .pgp-accordion-header.open .pgp-accordion-chevron { transform: rotate(180deg); }
      .pgp-accordion-body { padding: 14px 16px; background: #fff;
                             display: none; font-size: 13px; color: #374151;
                             overflow: visible; border-radius: 0 0 8px 8px;
                             position: relative; z-index: 10; }
      .pgp-accordion-body.open { display: block; }

      .report-panel { max-width: 700px; margin: 0 auto; padding: 24px 16px; }
      .report-cols  { display: flex; gap: 20px; align-items: flex-start; }
      .report-col-left  { flex: 1 1 50%; }
      .report-col-right { flex: 1 1 50%; }
      .report-group { background: #f8fafc; border: 1px solid #e2e8f0;
                      border-radius: 10px; padding: 20px 22px; margin-bottom: 18px; }
      .report-group h5 { font-size: 13px; font-weight: 700; text-transform: uppercase;
                          letter-spacing: 0.07em; color: #64748b; margin: 0 0 14px; }
      .report-group .form-group { margin-bottom: 10px; }
      .report-dl-btn { margin-top: 8px; }
      .report-note { font-size: 11.5px; color: #94a3b8; margin-top: 6px; }
      .report-contents { list-style: none; padding: 0; margin: 0; }
      .report-contents li { display: flex; align-items: baseline; gap: 8px;
                             padding: 5px 0; font-size: 12.5px; color: #374151;
                             border-bottom: 1px solid #f1f5f9; }
      .report-contents li:last-child { border-bottom: none; }
      .rc-tick  { color: #18bdb9; font-weight: 800; font-size: 14px; flex-shrink: 0; }
      .rc-cross { color: #cbd5e1; font-weight: 800; font-size: 14px; flex-shrink: 0; }
      .rc-soon  { color: #94a3b8; font-size: 11px; font-style: italic; }

      .ov-card { padding: 14px 16px; font-size: 13px; line-height: 1.65; color: #374151; }
      .ov-card ol, .ov-card ul { padding-left: 18px; margin: 8px 0 0; }
      .ov-card li { margin-bottom: 5px; }
      .ov-card pre { background: #f1f5f9; border-radius: 6px; padding: 10px 12px;
                      font-size: 12px; color: #1a2e35; margin-top: 8px; }
      .ov-card a { color: #18bdb9; }

      .pgp-header-text { display: flex; align-items: baseline; gap: 14px; flex-wrap: wrap; }
      .pgp-subtitle { font-style: italic; }
      .main-left  { padding-right: 10px; }
      .main-right { padding-left:  10px; }

      .var-chip {
        display: inline-block; padding: 3px 9px; font-size: 11px; font-weight: 500;
        background: #eef3f8; border: 1px solid #c8d8e8; border-radius: 20px;
        color: #2E74B5; cursor: pointer; font-family: 'DM Mono', monospace;
        transition: background 0.15s;
      }
      .var-chip:hover { background: #dbeafe; border-color: #93c5fd; }

      .interp-textarea {
        font-size: 12.5px; line-height: 1.65; resize: vertical;
        border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px 12px;
        color: #374151; background: #fafcff;
      }
      .interp-textarea:focus { border-color: #18bdb9; outline: none;
                                box-shadow: 0 0 0 2px rgba(24,189,185,0.15); }

      .report-export-row {
        display: flex; gap: 20px; align-items: flex-start;
        background: #f8fafc; border: 1px solid #e2e8f0;
        border-radius: 10px; padding: 18px 20px; margin-bottom: 18px;
      }
      .report-export-left  { flex: 1 1 60%; }
      .report-export-right { flex: 1 1 40%; }
      .report-export-right p { font-size:11px; font-weight:700;
                                text-transform:uppercase; letter-spacing:0.06em;
                                color:#64748b; margin:0 0 8px; }
    "))
  ),
  
  add_busy_spinner(spin = "fading-circle", color = "#18bdb9", position = "top-right"),
  
  tags$div(
    class = "pgp-header",
    tags$img(src = "pg_power_logo.png", height = "68px"),
    tags$div(
      class = "pgp-header-text",
      tags$h1("PG-Power", class = "pgp-title", style = "margin:0;"),
      tags$p(tags$em("design with confidence"), class = "pgp-subtitle",
             style = "margin:0; font-size:15px;")
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    # --------------------------------------------------------------------------
    # Tab 1 - Overview
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Overview",
      
      tags$div(
        style = "max-width: 720px; margin: 28px auto; padding: 0 16px;",
        
        # Basics
        acc_panel(
          id      = "acc_basics",
          heading = "Basics",
          open    = TRUE,
          tags$div(
            class = "ov-card",
            tags$p("PG-Power is a sample size calculator for non-inferiority (NI) studies
                    involving binary proportions. It supports both single-arm (device vs
                    performance goal) and two-arm (treatment vs control) designs, with
                    analytic and simulation-based CI methods."),
            tags$ol(
              tags$li("Go to the ", tags$b("Calculator"), " tab to compute required sample sizes."),
              tags$li("Use the ", tags$b("Interim Analysis"), " tab to monitor an ongoing study."),
              tags$li("Export a formatted summary from the ", tags$b("Generate Report"), " tab.")
            )
          )
        ),
        
        # Calculator Info
        acc_panel(
          id      = "acc_calc_info",
          heading = "Calculator",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(tags$b("Trial Design Settings")),
            tags$ul(
              tags$li(tags$b("Design:"), " Single-arm (device vs benchmark) or two-arm (treatment vs control)."),
              tags$li(tags$b("Endpoint:"), " Efficacy (higher is better) or Safety (lower is better). Switching presets p\u2080, p\u2081, and \u0394."),
              tags$li(tags$b("\u03b1:"), " One-sided significance level. Typically 0.025 for medical device studies."),
              tags$li(tags$b("Power:"), " Probability of correctly demonstrating NI."),
              tags$li(tags$b("Allocation ratio:"), " Two-arm only. Ratio of treatment to control patients."),
              tags$li(tags$b("CI method:"), " Drives the simulation-based search. Z (power formula) uses an analytic closed form.")
            ),
            tags$p(tags$b("Proportions")),
            tags$ul(
              tags$li(tags$b("p\u2080:"), " Control or benchmark event rate."),
              tags$li(tags$b("p\u2081:"), " Expected device or experimental event rate."),
              tags$li(tags$b("\u0394:"), " Non-inferiority margin. The maximum tolerable shortfall."),
              tags$li(tags$b("Sensitivity window:"), " Range (\u00b1) swept around \u0394 in the sensitivity plot.")
            ),
            tags$p(tags$b("Other Settings")),
            tags$ul(
              tags$li("Toggle the n result box, vertical plot marker, and sensitivity tables."),
              tags$li("Download sensitivity data as CSV."),
              tags$li("Use ", tags$b("Defaults"), " to reset all inputs.")
            )
          )
        ),
        
        # Confidence Intervals
        acc_panel(
          id      = "acc_ci",
          heading = "Confidence Intervals",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("CI methods drive the simulation-based sample size search. Key options:"),
            tags$ul(
              tags$li(tags$b("Wilson:"), " Good coverage near p = 0 or 1. Recommended default."),
              tags$li(tags$b("Exact (Clopper\u2013Pearson):"), " Inverts the binomial test. Conservative \u2014 gives larger n."),
              tags$li(tags$b("Agresti\u2013Coull:"), " Adds a small correction. Close to Wilson."),
              tags$li(tags$b("Asymptotic (Wald):"), " Normal approximation. Avoid at small n or extreme p."),
              tags$li(tags$b("prop.test:"), " Score/chi-squared; generally better than Wald."),
              tags$li(tags$b("Logit / Probit / Cloglog:"), " Model-based CI on transformed scale."),
              tags$li(tags$b("Bayes:"), " Bayesian credible interval \u2014 not a frequentist CI.")
            ),
            tags$hr(class = "pgp-hr"),
            tags$p(tags$b("Decision rules used in simulation:")),
            tags$p("Two-arm:"),
            tags$pre("Lower(RD) = Lower(p\u2081) \u2212 Upper(p\u2080)\nDeclare NI if Lower(RD) > \u2212\u0394"),
            tags$p("Single-arm:"),
            tags$pre("Declare NI if Lower(p) > p\u2080 \u2212 \u0394"),
            tags$div(
              style = "margin-top:14px; background:#eef9f9; border:1px solid #18bdb9;
                       border-radius:8px; padding:11px 14px; display:flex;
                       align-items:center; gap:10px;",
              tags$span("\U0001F4D6", style = "font-size:18px; flex-shrink:0;"),
              tags$div(
                tags$p(style = "margin:0 0 2px; font-size:12px; font-weight:700;
                                 color:#0f7f7c; letter-spacing:0.02em;",
                       "Want to learn more about choosing a CI method?"),
                tags$a(
                  href   = "https://filipmkgit.github.io/Small-Proportions-and-Confidence-Intervals-Analysis/small_proportions_ci.html",
                  target = "_blank",
                  style  = "font-size:12px; color:#18bdb9; font-weight:600;",
                  "Read: Small Proportions and Confidence Intervals \u2197"
                )
              )
            )
          )
        ),
        
        # Generate Report Info
        acc_panel(
          id      = "acc_report_info",
          heading = "Generate Report",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("The Generate Report tab builds a formatted summary from your current Calculator inputs."),
            tags$ul(
              tags$li(tags$b("Format:"), " Word (.docx) or PDF (requires the ", tags$code("pagedown"), " package)."),
              tags$li(tags$b("Title & Header:"), " Set a custom title; optionally include date, CI method, and author name."),
              tags$li(tags$b("Interpretation:"), " Edit a free-text paragraph and insert live variable values using tag buttons (e.g. {n}, {power_pct}). Choose from built-in templates or start blank."),
              tags$li(tags$b("Include in Report:"), " Toggle individual sections \u2014 results table, interpretation, CI comparison, definitions, calculation code, sensitivity plots, sensitivity tables, and interim analysis summary."),
              tags$li(tags$b("Report contents:"), " Live checklist at the top of the tab updates as you tick options.")
            )
          )
        ),
        
        # Interim Analysis Info
        acc_panel(
          id      = "acc_interim_info",
          heading = "Interim Analysis",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("The Interim Analysis tab lets you monitor a study in progress against its pre-specified NI boundary."),
            tags$ul(
              tags$li(tags$b("Inputs:"), " Enter the number of patients enrolled so far and the events observed. For two-arm designs, enter events for both arms."),
              tags$li(tags$b("Boundary:"), " Pulled automatically from the Calculator tab \u2014 set p\u2080, p\u2081, and \u0394 there first."),
              tags$li(tags$b("Status box:"), " Shows whether NI is currently demonstrated based on the observed CI bound vs the NI boundary."),
              tags$li(tags$b("Position plot:"), " Visualises the observed estimate and CI relative to the NI boundary."),
              tags$li(tags$b("Calculation table:"), " Step-by-step breakdown of every number in the status box."),
              tags$li(tags$b("CI method comparison:"), " For single-arm designs, shows the minimum (efficacy) or maximum (safety) event count needed under each CI method at the current n.")
            ),
            tags$p(style = "font-size:12px; color:#94a3b8; margin-top:10px;",
                   "Note: The interim tool is descriptive, not a formal interim analysis with alpha-spending.
                    It does not adjust for multiplicity. Consult a statistician before making stopping decisions.")
          )
        ),
        
        # ISO / FDA & Performance Goals
        acc_panel(
          id      = "acc_iso_fda",
          heading = "ISO / FDA & Performance Goals",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(tags$b("What is a Performance Goal (PG)?")),
            tags$p("A performance goal is a pre-specified, objective benchmark derived from
                    historical data, literature, or prior device performance. It represents
                    the minimum acceptable event rate (efficacy) or maximum acceptable
                    complication rate (safety) that a new device must meet or exceed."),
            tags$p(tags$b("Regulatory context")),
            tags$ul(
              tags$li(tags$b("FDA (US):"), " The FDA guidance on non-inferiority trials
                      (2016) and the Bayesian guidance (2010) describe performance goal
                      studies as appropriate when a concurrent control is not feasible
                      (e.g. rare conditions, ethical constraints, or well-established
                      benchmarks). The PG must be justified with a literature review
                      and ideally drawn from a meta-analysis of historical control data."),
              tags$li(tags$b("ISO 14155:2020:"), " Governs clinical investigation of medical
                      devices for human subjects. Requires a pre-specified primary
                      endpoint, sample size justification, and a defined success criterion
                      (the performance goal). Non-inferiority margin (Δ) must be
                      clinically justified."),
              tags$li(tags$b("ISO 5840 / ISO 11135 / device-specific standards:"),
                      " Many device families have published OPC (Objective Performance
                      Criteria) values in their specific ISO standards or FDA guidance
                      documents. These should be the first source for p₀.")
            ),
            tags$p(tags$b("One-sided vs two-sided testing")),
            tags$p("Performance goal studies typically use a ", tags$b("one-sided test"),
                   " at α = 0.025 (equivalent to a 95% confidence interval lower bound)
                   or α = 0.05 (90% CI lower bound). The FDA and ISO guidance both
                   accept one-sided 0.025 as the standard for pivotal device studies."),
            tags$p(tags$b("Choosing the NI margin (Δ)")),
            tags$ul(
              tags$li("The margin must be clinically meaningful: small enough that a
                       device just meeting it is still acceptable to patients."),
              tags$li("Common practice: Δ = 0 (pure superiority vs PG), or Δ set
                       at the lower bound of the historical 95% CI for the reference rate."),
              tags$li("For safety endpoints (e.g. major adverse events), Δ is the
                       maximum additional event rate considered clinically acceptable."),
              tags$li("Document the rationale for Δ explicitly in the clinical
                       investigation plan (CIP) or IDE submission.")
            ),
            tags$p(tags$b("Success criterion")),
            tags$p("The study is declared successful if the lower bound of the CI for
                    the device rate exceeds p₀ − Δ. PG-Power computes the
                    minimum event count (n-successes) corresponding to this boundary
                    for the chosen CI method and α."),
            tags$div(
              style = "margin-top:14px; background:#fff8ee; border:1px solid #e8c96a;
                       border-radius:8px; padding:11px 14px;",
              tags$p(style = "margin:0; font-size:12px; color:#7a5c00;",
                     tags$b("Important: "),
                     "PG-Power is a planning and monitoring tool. The performance goal,
                      NI margin, CI method, and α must all be pre-specified in the
                      study protocol before data collection begins. Post-hoc changes
                      require regulatory justification.")
            )
          )
        ),
        
        # Credits
        acc_panel(
          id      = "acc_credits",
          heading = "Credits",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(HTML("Developed by <strong>\u00c1ine Glynn</strong> and
                          <strong>Filip K\u0142osowski</strong> \u00b7 University of Galway.")),
            tags$p(tags$a(
              href   = "https://github.com/FilipMKgit/Margin-Jinn",
              target = "_blank",
              "View source on GitHub \u2197"
            )),
            tags$p("Built with R, Shiny, bslib, plotly, binom, and officer."),
            tags$p(style = "color:#94a3b8; font-size:11.5px; margin-top:8px;",
                   "Claude (Anthropic) assisted with parts of the code development.")
          )
        ),
        
        # -- Bottom action buttons --------------------------------------------
        tags$div(
          style = "display:flex; justify-content:flex-end; gap:8px;
                   margin-top:18px; padding-top:14px; border-top:1px solid #f1f5f9;",
          tags$button(
            class   = "btn btn-sm btn-outline-secondary",
            style   = "font-size:12px; padding:5px 16px; border-color:#e2e8f0;
                       color:#374151; display:flex; align-items:center; gap:6px;",
            onclick = "pgpResetAll();",
            tags$span("↺"),
            tags$span("Restore All Defaults")
          ),
          actionButton(
            "btn_reload_app",
            label    = tagList(tags$span("⏻"), tags$span("Reload App")),
            class    = "btn btn-sm btn-outline-secondary",
            style    = "font-size:12px; padding:5px 16px; border-color:#e2e8f0;
                        color:#374151; display:flex; align-items:center; gap:6px;"
          )
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # Tab 2 - Calculator
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Calculator",
      
      fluidRow(
        style = "margin: 18px 8px 0;",
        
        column(
          width = 4,
          class = "main-left",
          
          acc_panel(
            id      = "acc_design",
            heading = "Trial Design Settings",
            open    = TRUE,
            
            selectInput(
              "prop_design", "Design",
              choices = c(
                "Two-arm NI (treatment vs control)"      = "two_arm",
                "Single-arm NI (treatment vs benchmark)" = "one_arm"
              ),
              selected = "one_arm"
            ),
            
            selectInput(
              "endpoint", "Endpoint",
              choices = c("Efficacy" = "efficacy", "Safety" = "safety"),
              selected = "efficacy"
            ),
            
            selectInput(
              "sig.level", "Significance level (one-sided \u03b1)",
              choices = c("0.025" = 0.025, "0.04" = 0.04, "0.05" = 0.05),
              selected = 0.025
            ),
            
            sliderInput(
              "power", "Power",
              min = 0.80, max = 0.95, step = 0.05, value = 0.80
            ),
            
            conditionalPanel(
              condition = "input.prop_design != 'one_arm'",
              selectInput(
                "r", "Allocation ratio (treatment : control)",
                choices = c("1:1" = 1, "2:1" = 2, "3:1" = 3, "4:1" = 4, "5:1" = 5),
                selected = 1
              )
            ),
            
            tags$hr(class = "pgp-hr"),
            
            selectInput(
              "ci_method_prop", "CI method",
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
            
            checkboxInput("showCompare", "Show CI method comparison table", value = FALSE)
          ),
          
          acc_panel(
            id      = "acc_props",
            heading = "Proportions",
            open    = TRUE,
            
            sliderInput("p0.expected",
                        "Benchmark / performance goal (p\u2080):",
                        min = 0.00, max = 1.00, step = 0.01, value = 0.88),
            
            sliderInput("p1.expected",
                        "Expected device event rate (p\u2081):",
                        min = 0.00, max = 1.00, step = 0.01, value = 0.93),
            
            sliderInput("p1.tolerable",
                        "Non-inferiority margin (\u0394):",
                        min = 0.00, max = 0.20, step = 0.01, value = 0.05),
            
            selectInput(
              "WindowMargin", "Sensitivity window for NI margin (\u00b1)",
              choices = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05),
              selected = 0.05
            )
          ),
          
          acc_panel(
            id      = "acc_sim",
            heading = "Simulation Settings",
            open    = FALSE,
            
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
            )
          ),
          
          acc_panel(
            id      = "acc_other",
            heading = "Other Settings",
            open    = FALSE,
            
            checkboxInput("showNBox_prop",  "Show n at chosen \u0394",           value = TRUE),
            checkboxInput("showVline",      "Show vertical marker on plots",     value = FALSE),
            checkboxInput("showTable",      "Show \u0394 sensitivity table",     value = FALSE),
            checkboxInput("showTable2",     "Show p\u2081 sensitivity table",    value = FALSE),
            
            tags$div(
              class = "dl-btn-col",
              style = "margin-top: 10px;",
              downloadButton("downloadData_plot1", "\u2193 Download \u0394 table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadData_plot2", "\u2193 Download p\u2081 table",
                             class = "btn-sm btn-outline-primary pgp-btn")
            ),
            
            tags$div(
              style = "margin-top:14px; padding-top:12px; border-top:1px solid #f1f5f9;",
              tags$button(
                class   = "btn btn-sm btn-outline-secondary",
                style   = "font-size:12px; padding:4px 14px; border-color:#e2e8f0; color:#374151;",
                onclick = "pgpResetCalculator();",
                "\u21ba Defaults"
              )
            )
          )
        ),
        
        column(
          width = 8,
          class = "main-right pgp-main",
          
          plotlyOutput("plot1", height = "420px"),
          uiOutput("n_box_prop"),
          tags$div(style = "height:22px;"),
          plotlyOutput("plot2", height = "420px"),
          uiOutput("compare_section"),
          DTOutput("dataTable"),
          DTOutput("dataTable2")
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # Tab 3 - Interim Analysis
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Interim Analysis",
      
      fluidRow(
        style = "margin: 18px 8px 0;",
        
        column(
          width = 4,
          class = "main-left",
          
          acc_panel(
            id = "acc_interim_inputs", heading = "Interim Analysis", open = TRUE,
            
            uiOutput("interim_sidebar_label"),
            
            numericInput("interim_n", "Patients enrolled so far (n):",
                         value = 0, min = 0, step = 1),
            conditionalPanel(
              condition = "input.prop_design == 'two_arm'",
              tags$p("(n is per arm \u2014 equal allocation assumed)",
                     style = "font-size:11px; color:#94a3b8; margin:-6px 0 8px;")
            ),
            numericInput("interim_x", "Events observed (treatment arm, x\u2081):",
                         value = 0, min = 0, step = 1),
            
            conditionalPanel(
              condition = "input.prop_design == 'two_arm'",
              numericInput("interim_x_control",
                           "Events observed (control arm, x\u2080):",
                           value = 0, min = 0, step = 1)
            ),
            
            tags$hr(class = "pgp-hr"),
            tags$p("Values", class = "sidebar-section-label"),
            uiOutput("interim_pulled_vals")
          )
        ),
        
        column(
          width = 8,
          class = "main-right pgp-main",
          
          uiOutput("interim_orientation_text"),
          tags$div(style = "height:10px;"),
          uiOutput("interim_status_box"),
          tags$div(style = "height:14px;"),
          plotlyOutput("interim_position_plot", height = "260px"),
          tags$p(
            style = "font-size:11px; color:#718096; margin: 4px 0 18px; line-height:1.6;",
            HTML(paste0(
              "<span style='color:#e07b39; font-weight:600;'>--- NI boundary</span>",
              "&nbsp;&nbsp;|&nbsp;&nbsp;",
              "<span style='color:#718096;'>--- Reference (p\u2080 or zero diff.)</span>",
              "&nbsp;&nbsp;|&nbsp;&nbsp;",
              "<span style='color:#18bdb9; font-weight:600;'>\u25cf with bars</span>",
              " = observed estimate \u00b1 95% CI"
            ))
          ),
          uiOutput("interim_calc_table"),
          tags$div(style = "height:18px;"),
          uiOutput("interim_ci_threshold_table")
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # Tab 4 - Generate Report
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Generate Report",
      
      tags$div(
        class = "report-panel",
        
        tags$div(
          class = "report-export-row",
          
          tags$div(
            class = "report-export-left",
            tags$div(
              style = "display:flex; align-items:center; gap:14px; flex-wrap:wrap;",
              radioButtons(
                "report_format", label = NULL,
                choices  = c("Word (.docx)" = "docx", "PDF (.pdf)" = "pdf"),
                selected = "docx", inline = TRUE
              ),
              uiOutput("report_download_ui")
            ),
            tags$p(
              class = "report-note",
              style = "margin-top:6px;",
              "Report is built from your current Calculator tab inputs."
            )
          ),
          
          tags$div(
            class = "report-export-right",
            tags$p(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                            letter-spacing:0.06em; color:#64748b; margin:0 0 8px;",
                   "Report contents"),
            uiOutput("report_contents_ui")
          )
        ),
        
        # Title & Header
        acc_panel(
          id = "acc_rpt_header", heading = "Title & Header", open = FALSE,
          
          tags$div(
            style = "display:flex; align-items:center; gap:10px; margin-bottom:10px; flex-wrap:wrap;",
            tags$div(
              style = "flex:1 1 auto; min-width:180px;",
              tags$label(
                style = "font-size:11px; font-weight:700; text-transform:uppercase;
                          letter-spacing:0.06em; color:#64748b; display:block; margin-bottom:4px;",
                "Title template"
              ),
              tags$select(
                id    = "title_template_select",
                class = "form-control",
                style = "font-size:12px; height:32px; padding:4px 8px; color:#374151;
                         border:1px solid #e2e8f0; border-radius:6px; background:#fafcff;",
                tags$option(value = "default",  "Default \u2014 PG-Power Sample Size Report"),
                tags$option(value = "study",    "Study protocol title"),
                tags$option(value = "clinical", "Clinical investigation title"),
                tags$option(value = "stats",    "Statistical analysis plan title"),
                tags$option(value = "blank",    "Blank \u2014 enter your own")
              )
            ),
            tags$div(
              style = "flex:0 0 auto; padding-top:20px;",
              tags$button(
                class   = "btn btn-sm btn-outline-secondary",
                style   = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
                           color:#374151; white-space:nowrap;",
                onclick = "pgpLoadTitleTemplate();",
                "\u21ba Load"
              )
            ),
            tags$div(
              style = "flex:0 0 auto; padding-top:20px;",
              tags$button(
                class   = "btn btn-sm btn-outline-secondary",
                style   = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
                           color:#374151; white-space:nowrap;",
                onclick = "pgpRestoreTitleDefault();",
                "\u21ba Defaults"
              )
            )
          ),
          
          textInput(
            "rpt_title", label = "Report title",
            value = "PG-Power \u2014 Sample Size Report",
            placeholder = "Report title..."
          ),
          
          tags$div(
            style = "display:flex; gap:16px; flex-wrap:wrap; margin-bottom:4px;",
            checkboxInput("rpt_include_date",   "Include generation date", value = TRUE),
            checkboxInput("rpt_include_method", "Include CI method",       value = TRUE),
            checkboxInput("rpt_include_author", "Include author name",     value = FALSE)
          ),
          
          conditionalPanel(
            condition = "input.rpt_include_author == true",
            textInput(
              "rpt_author_name", label = NULL,
              value = "", placeholder = "Author name..."
            )
          )
        ),
        
        # Interpretation
        acc_panel(
          id = "acc_rpt_interp", heading = "Interpretation", open = FALSE,
          
          tags$div(
            style = "margin-top:4px;",
            
            tags$div(
              style = "display:flex; align-items:center; gap:10px; margin-bottom:12px; flex-wrap:wrap;",
              tags$div(
                style = "flex:1 1 auto; min-width:180px;",
                tags$label(
                  style = "font-size:11px; font-weight:700; text-transform:uppercase;
                            letter-spacing:0.06em; color:#64748b; display:block; margin-bottom:4px;",
                  "Template"
                ),
                tags$select(
                  id    = "interp_template_select",
                  class = "form-control",
                  style = "font-size:12px; height:32px; padding:4px 8px; color:#374151;
                           border:1px solid #e2e8f0; border-radius:6px; background:#fafcff;",
                  tags$option(value = "default",    "Default \u2014 single-arm, device success rate"),
                  tags$option(value = "concise",    "Concise \u2014 brief statistical statement"),
                  tags$option(value = "two_arm",    "Two-arm \u2014 risk difference framing"),
                  tags$option(value = "regulatory", "Regulatory \u2014 formal ISO / FDA language"),
                  tags$option(value = "safety",     "Safety endpoint \u2014 complication rate"),
                  tags$option(value = "blank",      "Blank \u2014 start from scratch")
                )
              ),
              tags$div(
                style = "flex:0 0 auto; padding-top:20px;",
                tags$button(
                  id    = "interp_load_template",
                  class = "btn btn-sm btn-outline-secondary",
                  style = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
                           color:#374151; white-space:nowrap;",
                  onclick = "pgpLoadTemplate();",
                  "\u21ba Load template"
                )
              ),
              tags$div(
                style = "flex:0 0 auto; padding-top:20px;",
                tags$button(
                  id    = "interp_restore_default",
                  class = "btn btn-sm btn-outline-secondary",
                  style = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
                           color:#374151; white-space:nowrap;",
                  onclick = "pgpRestoreDefault();",
                  "\u21ba Defaults"
                )
              )
            ),
            
            tags$p(
              style = "font-size:11.5px; color:#64748b; margin-bottom:6px;",
              "Insert live values into your text using these tags:"
            ),
            tags$div(
              style = "display:flex; flex-wrap:wrap; gap:6px; margin-bottom:10px;",
              lapply(
                list(
                  list(tag = "{n}",           label = "n"),
                  list(tag = "{n_dropout}",   label = "n (dropout)"),
                  list(tag = "{n_successes}", label = "n-successes"),
                  list(tag = "{power_pct}",   label = "Power %"),
                  list(tag = "{p0_pct}",      label = "p\u2080 %"),
                  list(tag = "{p1_pct}",      label = "p\u2081 %"),
                  list(tag = "{alpha}",       label = "\u03b1"),
                  list(tag = "{delta}",       label = "\u0394"),
                  list(tag = "{ci_method}",   label = "CI method")
                ),
                function(v) {
                  ins <- v$tag
                  js  <- paste0(
                    "var ta=document.getElementById(\"rpt_interp_text\");",
                    "var s=ta.selectionStart,e=ta.selectionEnd;",
                    "var ins=\"", ins, "\";",
                    "ta.value=ta.value.substring(0,s)+ins+ta.value.substring(e);",
                    "ta.selectionStart=ta.selectionEnd=s+ins.length;",
                    "ta.focus();",
                    "Shiny.setInputValue(\"rpt_interp_text\",ta.value,{priority:\"event\"});"
                  )
                  tags$button(v$label, class = "var-chip", onclick = js)
                }
              )
            ),
            
            tags$textarea(
              id          = "rpt_interp_text",
              class       = "form-control interp-textarea",
              rows        = "6",
              placeholder = "Interpretation text...",
              paste0(
                "A total of {n} evaluable patients are required to demonstrate, with ",
                "{power_pct}% power, that the device success rate exceeds the performance ",
                "goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. ",
                "Allowing for 10% dropout, the study should enrol {n_dropout} patients. ",
                "The study will be deemed successful if at least {n_successes} out of {n} ",
                "evaluable patients are free from a major adverse event at 12 months."
              )
            )
          )
        ),
        
        # Include in Report
        acc_panel(
          id = "acc_rpt_include", heading = "Include in Report", open = FALSE,
          
          # ── General ─────────────────────────────────────────────────────
          tags$p("General",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:0 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_results",     "Results table",         value = TRUE),
            checkboxInput("rpt_interp_inc",  "Interpretation",        value = TRUE),
            checkboxInput("rpt_definitions", "Definitions",           value = TRUE),
            checkboxInput("rpt_calc_code",   "Calculation code",      value = TRUE),
            checkboxInput("rpt_ci_compare",  "CI comparison table",   value = FALSE)
          ),
          
          # ── Sensitivity ──────────────────────────────────────────────────
          tags$p("Sensitivity",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_plot_delta",  "Δ plot",            value = FALSE),
            checkboxInput("rpt_plot_p1",     "p₁ plot",           value = FALSE),
            checkboxInput("rpt_table_delta", "Δ table",           value = FALSE),
            checkboxInput("rpt_table_p1",    "p₁ table",          value = FALSE)
          ),
          
          # ── Interim analysis ─────────────────────────────────────────────
          tags$p("Interim Analysis",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_interim_summ",  "Data summary",        value = FALSE),
            checkboxInput("rpt_interim_interp","Interpretation",      value = FALSE),
            checkboxInput("rpt_interim_ci",    "CI comparison",       value = FALSE),
            checkboxInput("rpt_interim_plot",  "Position plot",       value = FALSE)
          ),
          
          tags$div(
            style = "margin-top:10px; padding-top:8px; border-top:1px solid #f1f5f9;
                     display:flex; gap:6px; flex-wrap:wrap;",
            tags$button(
              class   = "btn btn-sm btn-outline-secondary",
              style   = "font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
              onclick = "pgpRestoreIncludes();",
              "\u21ba Defaults"
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-secondary",
              style   = "font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
              onclick = "pgpTickAllIncludes();",
              "\u2713 Tick all"
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-secondary",
              style   = "font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
              onclick = "pgpUntickAllIncludes();",
              "\u2715 Untick all"
            )
          )
        )
      )
    )
  ),
  
  # -- JS -----------------------------------------------------------------------
  tags$script(HTML('

    window.titleTemplates = {
      "default":  "PG-Power \u2014 Sample Size Report",
      "study":    "Sample Size Calculation \u2014 Study Protocol",
      "clinical": "Clinical Investigation: Sample Size Justification",
      "stats":    "Statistical Analysis Plan \u2014 Sample Size Section",
      "blank":    ""
    };

    window.pgpSetTitle = function(txt) {
      var el = document.getElementById("rpt_title");
      if (!el) return;
      el.value = txt;
      if (window.Shiny) Shiny.setInputValue("rpt_title", txt, {priority: "event"});
      el.dispatchEvent(new Event("input", {bubbles: true}));
    };

    window.pgpLoadTitleTemplate = function() {
      var sel = document.getElementById("title_template_select");
      var key = sel ? sel.value : "default";
      var txt = window.titleTemplates[key];
      if (txt === undefined) txt = window.titleTemplates["default"];
      window.pgpSetTitle(txt);
    };

    window.pgpRestoreTitleDefault = function() {
      window.pgpSetTitle(window.titleTemplates["default"]);
      var sel = document.getElementById("title_template_select");
      if (sel) sel.value = "default";
    };

    var pgpIncludeIds = [
        "rpt_results",
        "rpt_interp_inc",
        "rpt_ci_compare",
        "rpt_definitions",
        "rpt_calc_code",
        "rpt_plot_delta",
        "rpt_plot_p1",
        "rpt_table_delta",
        "rpt_table_p1",
        "rpt_interim_summ",
        "rpt_interim_interp",
        "rpt_interim_ci",
        "rpt_interim_plot"
      ];

    window.pgpTickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = true;
        if (window.Shiny) Shiny.setInputValue(id, true, {priority: "event"});
      });
    };

    window.pgpUntickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = false;
        if (window.Shiny) Shiny.setInputValue(id, false, {priority: "event"});
      });
    };

    window.pgpRestoreIncludes = function() {
      var defaults = {
        "rpt_results":     true,
        "rpt_interp_inc":  true,
        "rpt_ci_compare":  false,
        "rpt_definitions": true,
        "rpt_calc_code":   true,
        "rpt_plot_delta":   false,
        "rpt_plot_p1":      false,
        "rpt_table_delta":  false,
        "rpt_table_p1":     false,
        "rpt_interim_summ": false,
        "rpt_interim_interp": false,
        "rpt_interim_ci":   false,
        "rpt_interim_plot": false
      };
      Object.keys(defaults).forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = defaults[id];
        if (window.Shiny) Shiny.setInputValue(id, defaults[id], {priority: "event"});
      });
    };

    window.interpTemplates = {
      "blank":      "",
      "default":    "A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power, that the device success rate exceeds the performance goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. Allowing for 10% dropout, the study should enrol {n_dropout} patients. The study will be deemed successful if at least {n_successes} out of {n} evaluable patients are free from a major adverse event at 12 months.",
      "concise":    "A sample size of {n} patients provides {power_pct}% power (one-sided \u03b1 = {alpha}) to demonstrate non-inferiority of the device against the performance goal of {p0_pct}%, with an NI margin of \u0394 = {delta}, assuming a true device success rate of {p1_pct}%.",
      "two_arm":    "A total of {n} patients are required to demonstrate non-inferiority of the treatment versus the control, with {power_pct}% power and a one-sided significance level of {alpha}. The assumed event rates are {p1_pct}% (treatment) and {p0_pct}% (control), with a non-inferiority margin of {delta} on the risk difference scale. Allowing for 10% dropout, {n_dropout} patients should be enrolled.",
      "regulatory": "The study is designed as a single-arm, non-inferiority study comparing the device success rate to an objective performance criterion (OPC) of {p0_pct}%, consistent with published literature and historical data. A minimum of {n} evaluable subjects is required to demonstrate, with {power_pct}% power at a one-sided significance level of {alpha}, that the lower bound of the {ci_method} confidence interval for the device success rate exceeds the performance goal less the non-inferiority margin ({delta}). Accounting for a 10% dropout rate, the study will enrol {n_dropout} subjects. The primary endpoint will be met if at least {n_successes} of {n} evaluable subjects achieve procedural success.",
      "safety":     "A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power (one-sided \u03b1 = {alpha}), that the device complication rate is non-inferior to the performance goal of {p0_pct}%, assuming a true complication rate of {p1_pct}% and an acceptable margin of {delta}. With an anticipated dropout rate of 10%, {n_dropout} patients will be enrolled. The safety endpoint will be satisfied if no more than the pre-specified number of adverse events are observed among the {n} evaluable patients."
    };

    window.pgpSetInterp = function(txt) {
      var ta = document.getElementById("rpt_interp_text");
      if (!ta) return;
      ta.value = txt;
      if (window.Shiny) Shiny.setInputValue("rpt_interp_text", txt, {priority: "event"});
    };

    window.pgpLoadTemplate = function() {
      var sel = document.getElementById("interp_template_select");
      var key = sel ? sel.value : "default";
      var txt = (window.interpTemplates[key] !== undefined)
                  ? window.interpTemplates[key]
                  : window.interpTemplates["default"];
      window.pgpSetInterp(txt);
    };

    window.pgpRestoreDefault = function() {
      window.pgpSetInterp(window.interpTemplates["default"]);
      var sel = document.getElementById("interp_template_select");
      if (sel) sel.value = "default";
    };

    // Full app reset: calculator + report settings
    window.pgpResetAll = function() {
      // Calculator
      window.pgpResetCalculator();

      // Report: title
      window.pgpRestoreTitleDefault();

      // Report: interpretation
      window.pgpRestoreDefault();

      // Report: include checkboxes
      window.pgpRestoreIncludes();

      // Report: header checkboxes
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue("rpt_include_date",   true,  {priority: "event"});
      S.setInputValue("rpt_include_method", true,  {priority: "event"});
      S.setInputValue("rpt_include_author", false, {priority: "event"});
      S.setInputValue("report_format",      "docx",{priority: "event"});

      // Interim analysis inputs
      S.setInputValue("interim_n",         0, {priority: "event"});
      S.setInputValue("interim_x",         0, {priority: "event"});
      S.setInputValue("interim_x_control", 0, {priority: "event"});
    };

    window.pgpResetCalculator = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue("prop_design",     "one_arm",  {priority: "event"});
      S.setInputValue("endpoint",        "efficacy", {priority: "event"});
      S.setInputValue("sig.level",       "0.025",    {priority: "event"});
      S.setInputValue("power",           0.80,       {priority: "event"});
      S.setInputValue("r",               "1",        {priority: "event"});
      S.setInputValue("ci_method_prop",  "wilson",   {priority: "event"});
      S.setInputValue("showCompare",     false,      {priority: "event"});
      S.setInputValue("p0.expected",     0.88,  {priority: "event"});
      S.setInputValue("p1.expected",     0.93,  {priority: "event"});
      S.setInputValue("p1.tolerable",    0.05,  {priority: "event"});
      S.setInputValue("WindowMargin",    "0.05",{priority: "event"});
      S.setInputValue("sim_quality",     "1000",{priority: "event"});
      S.setInputValue("sim_seed",        1,     {priority: "event"});
      S.setInputValue("showNBox_prop",   true,  {priority: "event"});
      S.setInputValue("showVline",       false, {priority: "event"});
      S.setInputValue("showTable",       false, {priority: "event"});
      S.setInputValue("showTable2",      false, {priority: "event"});
    };

    $(document).on("click", ".pgp-accordion-header", function() {
      var $hdr  = $(this);
      var $body = $hdr.next(".pgp-accordion-body");
      $hdr.toggleClass("open");
      $body.toggleClass("open");
    });

    $(document).ready(function() {
      var ta = document.getElementById("rpt_interp_text");
      if (ta) {
        Shiny.setInputValue("rpt_interp_text", ta.value);
        ta.addEventListener("input", function() {
          Shiny.setInputValue("rpt_interp_text", ta.value, {priority: "event"});
        });
      }
    });
  '))
)