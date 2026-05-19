# =============================================================================
# ui.R  —  PG-Power
# Defines the app layout: header, tabs, accordions, inputs, and JavaScript.
# acc_panel() is defined in global.R (loaded before ui.R at startup).

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
      .rc-tick  { color: #5b35d5; font-weight: 800; font-size: 14px; flex-shrink: 0; }
      .rc-cross { color: #cbd5e1; font-weight: 800; font-size: 14px; flex-shrink: 0; }

      .ov-card { padding: 14px 16px; font-size: 13px; line-height: 1.65; color: #374151; }
      .ov-card ol, .ov-card ul { padding-left: 18px; margin: 8px 0 0; }
      .ov-card li { margin-bottom: 5px; }
      .ov-card pre { background: #f1f5f9; border-radius: 6px; padding: 10px 12px;
                      font-size: 12px; color: #1a2e35; margin-top: 8px; }
      .ov-card a { color: #5b35d5; }

      .pgp-header-text { display: flex; align-items: baseline; gap: 14px; flex-wrap: wrap; }
      .pgp-subtitle { font-style: italic; margin: 0 !important; line-height: 1; }
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
      .interp-textarea:focus { border-color: #5b35d5; outline: none;
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

      /* ── n-box expand/collapse ─────────────────────────────────────────── */
      .n-box-expanded {
        margin-top: 8px; padding: 10px 14px;
        background: #f8fafc; border: 1px solid #e2e8f0;
        border-radius: 8px; font-size: 12px;
      }
      .n-box-expanded .nb-row {
        display: flex; gap: 10px; padding: 4px 0;
        border-bottom: 1px solid #f1f5f9; line-height: 1.5;
      }
      .n-box-expanded .nb-row:last-child { border-bottom: none; }
      .n-box-expanded .nb-label {
        flex: 0 0 210px; font-weight: 600; color: #64748b; font-size: 11.5px;
      }
      .n-box-expanded .nb-val {
        flex: 1 1 auto; color: #1a2e35; font-family: 'DM Mono', monospace;
        font-size: 11.5px;
      }
      .n-box-toggle {
        background: none; border: 1px solid #e2e8f0; border-radius: 5px;
        font-size: 11px; color: #64748b; padding: 2px 9px; cursor: pointer;
        transition: border-color 0.15s, color 0.15s;
      }
      .n-box-toggle:hover { border-color: #5b35d5; color: #5b35d5; }

      /* ── Alpha slider tick labels ──────────────────────────────────────── */
      .alpha-slider-wrap { position: relative; }
      .alpha-tick-labels {
        display: flex; justify-content: space-between;
        font-size: 10px; color: #94a3b8;
        margin: -8px 7px 4px; line-height: 1.3;
      }
      .alpha-common-badge {
        display: inline-block; padding: 0px 5px; font-size: 9px; font-weight: 700;
        border-radius: 8px; letter-spacing: 0.03em;
        background: #5b35d522; color: #5b35d5; border: 1px solid #5b35d555;
        margin-left: 2px; vertical-align: middle;
      }

      /* -- Plot colour swatches ---------------------------------------------- */
      .pgp-colour-bar {
        display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
      }
      .pgp-colour-bar span {
        font-size: 12px; color: #64748b; margin-right: 2px;
      }
      .pgp-swatch {
        width: 22px; height: 22px; border-radius: 50%;
        border: 2px solid transparent; cursor: pointer;
        transition: transform 0.15s, border-color 0.15s;
        outline: none;
      }
      .pgp-swatch:hover   { transform: scale(1.18); }
      .pgp-swatch.active  { border-color: #1a2e35; transform: scale(1.18); }
    "))
  ),
  
  add_busy_spinner(spin = "fading-circle", color = "#5b35d5", position = "top-right"),
  
  tags$div(
    class = "pgp-header",
    tags$img(src = "pg_power_logo_1.png", height = "68px"),
    tags$p(tags$em("design with confidence"), class = "pgp-subtitle")
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
        
        acc_panel(
          id      = "acc_basics",
          heading = "Basics",
          open    = FALSE,
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
        
        acc_panel(
          id      = "acc_calc_info",
          heading = "Calculator",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(tags$b("Trial Design Settings")),
            tags$ul(
              tags$li(tags$b("Design:"), " Single-arm NI (device vs a performance goal) or Two-arm NI (treatment vs concurrent control)."),
              tags$li(tags$b("Endpoint direction:"), " Whether a higher or lower observed rate is the favourable outcome. Changing this updates the H\u2081 hypothesis and presets p\u2080, p\u2081, and \u0394."),
              tags$li(tags$b("\u03b1 (significance level):"), " One-sided \u03b1 from a dropdown. 0.025 is standard for pivotal medical device studies."),
              tags$li(tags$b("Power:"), " Probability of correctly demonstrating non-inferiority. 0.80 and 0.90 are most common."),
              tags$li(tags$b("Allocation ratio:"), " Two-arm only. Ratio of treatment to control patients (e.g. 2:1)."),
              tags$li(tags$b("CI method:"), " Method used in the simulation-based sample size search. Z (power formula) gives an instant analytic result; all others run a binary-search simulation.")
            ),
            tags$p(tags$b("Proportions")),
            tags$ul(
              tags$li(tags$b("p\u2080:"), " Benchmark or control event rate (the performance goal)."),
              tags$li(tags$b("p\u2081:"), " Expected device or experimental event rate. Must be on the favourable side of p\u2080 \u2212 \u0394."),
              tags$li(tags$b("\u0394:"), " Non-inferiority margin. The maximum clinically acceptable shortfall from the benchmark."),
              tags$li(tags$b("Sensitivity window:"), " Half-width of the \u0394 range swept in the sensitivity plot (\u00b10.005 to \u00b10.150).")
            ),
            tags$p(tags$b("n Result Box")),
            tags$p("Below the \u0394 plot, an expandable box shows the required n at the chosen \u0394, together with the hypotheses, CI equivalent, min successes needed to reject H\u2080, and the dropout-adjusted enrolment target."),
            tags$p(tags$b("Other Settings")),
            tags$ul(
              tags$li("Toggle the n result box and selected-value crosshair on plots."),
              tags$li("Show or hide the \u0394 and p\u2081 sensitivity tables below the plots."),
              tags$li("Adjust the ", tags$b("dropout rate"), " (1\u201320%) to set the enrolment inflation factor."),
              tags$li("Download sensitivity tables as CSV or save either plot as a PNG."),
              tags$li(tags$b("Defaults"), " resets all calculator inputs to their starting values.")
            )
          )
        ),
        
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
              style = "margin-top:14px; background:#f0eeff; border:1px solid #5b35d5;
                       border-radius:8px; padding:11px 14px; display:flex;
                       align-items:center; gap:10px;",
              
              tags$div(
                tags$p(style = "margin:0 0 2px; font-size:12px; font-weight:700;
                                 color:#3d21b7; letter-spacing:0.02em;",
                       "Want to learn more about choosing a CI method?"),
                tags$a(
                  href   = "https://filipmkgit.github.io/Small-Proportions-and-Confidence-Intervals-Analysis/small_proportions_ci.html",
                  target = "_blank",
                  style  = "font-size:12px; color:#5b35d5; font-weight:600;",
                  "Read: Small Proportions and Confidence Intervals \u2197"
                )
              )
            )
          )
        ),
        
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
              tags$li(tags$b("CI method comparison:"), " For single-arm designs, shows how many events are needed (or allowed) to demonstrate NI under each CI method at the current n, and whether the observed x passes.")
            ),
            tags$p(
              tags$b("Other Settings"), " includes toggles for the calculation table, CI comparison table,
              and calculation code. The position plot and both data tables can be downloaded directly
              from Other Settings."
            ),
            tags$p(style = "font-size:12px; color:#94a3b8; margin-top:10px;",
                   "Note: The interim tool is descriptive, not a formal interim analysis with alpha-spending.
                    It does not adjust for multiplicity or provide stopping boundaries.
                    Consult a statistician before making any stopping decision.")
          )
        ),
        
        acc_panel(
          id      = "acc_report_info",
          heading = "Generate Report",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("Builds a formatted PDF or Word report from the current Calculator inputs."),
            tags$p(tags$b("Format & export")),
            tags$ul(
              tags$li("Choose ", tags$b("PDF (.pdf)"), " or ", tags$b("Word (.docx)"), " and click the download button."),
              tags$li("PDF export requires the ", tags$code("pagedown"), " R package and Google Chrome.")
            ),
            tags$p(tags$b("Title & Header")),
            tags$p("Set a custom report title from a template or free text. Optionally include the generation date, CI method, and author name."),
            tags$p(tags$b("Interpretation")),
            tags$ul(
              tags$li("Edit a free-text paragraph using the built-in templates as a starting point."),
              tags$li("Click tag buttons (e.g. ", tags$code("{n}"), ", ", tags$code("{power_pct}"), ", ", tags$code("{dropout_pct}"), ") to insert live values from the current inputs."),
              tags$li("A collapsible ", tags$b("Current calculator values"), " panel shows all key inputs and outputs without switching tabs.")
            ),
            tags$p(tags$b("Include in Report")),
            tags$p("Toggle any combination of sections:"),
            tags$ul(
              tags$li(tags$b("General:"), " Results table, full n summary, interpretation, CI comparison, definitions, calculation code."),
              tags$li(tags$b("Sensitivity:"), " \u0394 and p\u2081 plots and/or tables."),
              tags$li(tags$b("Interim Analysis:"), " Data summary, interpretation, CI comparison, position plot.")
            ),
            tags$p("The live ", tags$b("Report contents"), " checklist in the top-right of the tab reflects your current selections.")
          )
        ),
        
        acc_panel(
          id      = "acc_iso_fda",
          heading = "ISO / FDA & Performance Goals",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(tags$b("What is a Performance Goal (PG)?")),
            tags$p("A performance goal is a pre-specified, objective benchmark derived from
                    historical data, literature, or prior device performance. It represents
                    the minimum or maximum acceptable event rate that a device must
                    achieve, depending on whether a higher or lower rate is the desired outcome."),
            tags$p(tags$b("Regulatory context")),
            tags$ul(
              tags$li(tags$b("FDA (US):"), " The FDA guidance on non-inferiority trials
                      (2016) and the Bayesian guidance (2010) describe performance goal
                      studies as appropriate when a concurrent control is not feasible."),
              tags$li(tags$b("ISO 14155:2020:"), " Governs clinical investigation of medical
                      devices for human subjects. Requires a pre-specified primary
                      endpoint, sample size justification, and a defined success criterion."),
              tags$li(tags$b("ISO 5840 / ISO 11135 / device-specific standards:"),
                      " Many device families have published OPC values in their specific
                      ISO standards or FDA guidance documents.")
            ),
            tags$p(tags$b("One-sided vs two-sided testing")),
            tags$p("Performance goal studies typically use a ", tags$b("one-sided test"),
                   " at \u03b1 = 0.025 (equivalent to a 95% CI lower bound)
                   or \u03b1 = 0.05 (90% CI lower bound)."),
            tags$p(tags$b("Choosing the NI margin (\u0394)")),
            tags$ul(
              tags$li("The margin must be clinically meaningful."),
              tags$li("Common practice: \u0394 = 0 (pure superiority vs PG), or \u0394 set
                       at the lower bound of the historical 95% CI for the reference rate."),
              tags$li("Document the rationale explicitly in the CIP or IDE submission.")
            ),
            tags$div(
              style = "margin-top:14px; background:#fff8ee; border:1px solid #e8c96a;
                       border-radius:8px; padding:11px 14px;",
              tags$p(style = "margin:0; font-size:12px; color:#7a5c00;",
                     tags$b("Important: "),
                     "PG-Power is a planning and monitoring tool. The performance goal,
                      NI margin, CI method, and \u03b1 must all be pre-specified in the
                      study protocol before data collection begins.")
            )
          )
        ),
        
        acc_panel(
          id      = "acc_credits",
          heading = "Credits",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p(
              style = "font-size:12px; color:#374151; line-height:1.8;",
              HTML("Developed by <strong>\u00c1ine Glynn</strong> and
                    <strong>Filip K\u0142osowski</strong>, University of Galway.")
            ),
            tags$p(
              style = "font-size:12px; color:#374151; line-height:1.8;",
              "Developed as part of a Master\u2019s thesis in Health Data Science.",
              " Source code on ",
              tags$a(href = "https://github.com/FilipMKgit/MDX2526",
                     target = "_blank", style = "color:#5b35d5;", "GitHub"), "."
            ),
            tags$p(
              style = "font-size:12px; color:#374151; line-height:1.8;",
              "Logo by ", tags$b("Daniel Breheny"), " (University of Galway)."
            ),
            tags$hr(style = "border-color:#f1f5f9; margin:8px 0;"),
            tags$p(
              style = "font-size:11.5px; color:#64748b; line-height:1.8;",
              tags$b("R packages: "),
              tags$code("shiny"), ", ", tags$code("bslib"), ", ",
              tags$code("ggplot2"), ", ", tags$code("plotly"), ", ",
              tags$code("DT"), ", ", tags$code("binom"), ", ",
              tags$code("officer"), ", ", tags$code("base64enc"), ", ",
              tags$code("shinybusy")
            ),
            tags$p(
              style = "color:#94a3b8; font-size:11px; margin-top:6px;",
              "Claude (Anthropic) assisted with parts of the code development."
            )
          )
        ),
        
        # -- Disclaimer -------------------------------------------------------
        tags$div(
          style = "margin-top:18px; padding:12px 16px;
                   background:#fff8ee; border:1px solid #e8c96a; border-radius:8px;",
          tags$p(
            style = "margin:0; font-size:11.5px; color:#7a5c00; line-height:1.65;",
            tags$b("Disclaimer: "),
            "PG-Power is an exploratory tool intended to assist with study planning
             and is provided for educational and research purposes only. It does not
             constitute regulatory, statistical, or clinical advice. All sample size
             calculations, NI margins, and assumptions must be reviewed and validated
             by a qualified statistician before use in any formal study protocol,
             regulatory submission, or clinical investigation plan. The authors accept
             no responsibility for decisions made on the basis of outputs from this tool."
          )
        ),
        
        # -- Bottom action bar: colour picker + hints + defaults + reload ------
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;
                   gap:8px; flex-wrap:wrap;
                   margin-top:18px; padding-top:14px; border-top:1px solid #f1f5f9;",
          
          # Left: plot colour swatches
          tags$div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span("Plot colour:", style = "font-size:12px; color:#64748b;"),
            tags$button(
              class = "pgp-swatch active", id = "swatch_purple",
              style = "background:#5b35d5;", title = "Purple",
              onclick = "pgpSetPlotColour('#5b35d5', this);"
            ),
            tags$button(
              class = "pgp-swatch", id = "swatch_teal",
              style = "background:#18bdb9;", title = "Teal",
              onclick = "pgpSetPlotColour('#18bdb9', this);"
            ),
            tags$button(
              class = "pgp-swatch", id = "swatch_red",
              style = "background:#c0392b;", title = "Red",
              onclick = "pgpSetPlotColour('#c0392b', this);"
            ),
            tags$button(
              class = "pgp-swatch", id = "swatch_black",
              style = "background:#1a2e35;", title = "Black",
              onclick = "pgpSetPlotColour('#1a2e35', this);"
            )
          ),
          
          # Right: hints + defaults + reload
          tags$div(
            style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
            tags$div(
              id      = "hints_toggle_btn",
              style   = "display:flex; align-items:center; gap:6px; padding:5px 14px;
                         border:1px solid #5b35d5; border-radius:6px;
                         background:#f0eeff; font-size:12px; color:#374151; cursor:pointer;",
              onclick = "pgpToggleHints(this);",
              tags$span(style = "font-size:13px; font-weight:700; color:#5b35d5;", "Hints"),
              tags$span("Hide hints", id = "hints_toggle_label")
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-secondary",
              style   = "font-size:12px; padding:5px 16px; border-color:#e2e8f0;
                         color:#374151; display:flex; align-items:center; gap:6px;",
              onclick = "pgpResetAll();",
              tags$span("\u21ba"),
              tags$span("Restore All Defaults")
            ),
            actionButton(
              "btn_reload_app",
              label = tagList(tags$span("\u23fb"), tags$span("Reload App")),
              class = "btn btn-sm btn-outline-secondary",
              style = "font-size:12px; padding:5px 16px; border-color:#e2e8f0;
                       color:#374151; display:flex; align-items:center; gap:6px;"
            )
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
            
            # Design — simplified labels
            selectInput(
              "prop_design", "Design",
              choices  = c("Single-arm NI" = "one_arm", "Two-arm NI" = "two_arm"),
              selected = "one_arm"
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p(
                HTML("Single-arm NI: device vs a fixed performance goal (benchmark). &nbsp;
                      Two-arm NI: experimental treatment vs concurrent control."),
                style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;"
              )
            ),
            
            # Endpoint — shows H1 framing in hint
            selectInput(
              "endpoint", "Endpoint direction",
              choices  = c(
                "Higher rate is better (e.g. success, patency)" = "efficacy",
                "Lower rate is better (e.g. complications, MACE)" = "safety"
              ),
              selected = "efficacy"
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true && input.endpoint == 'efficacy'",
              tags$p(
                HTML("H\u2081: p > p\u2080 &minus; \u0394 &nbsp;&mdash;&nbsp;
                      device rate must exceed the NI boundary.<br>
                      Use when a <em>higher</em> observed rate means the device performed well."),
                style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.6;"
              )
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true && input.endpoint == 'safety'",
              tags$p(
                HTML("H\u2081: p < p\u2080 + \u0394 &nbsp;&mdash;&nbsp;
                      device rate must stay below the NI boundary.<br>
                      Use when a <em>lower</em> observed rate means the device performed well."),
                style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.6;"
              )
            ),
            
            # Alpha dropdown
            selectInput(
              "sig.level",
              HTML("Significance level (one-sided \u03b1)"),
              choices = c(
                "0.005  (stringent)"           = 0.005,
                "0.010"                        = 0.010,
                "0.025  (pivotal device)"      = 0.025,
                "0.040"                        = 0.040,
                "0.050  (exploratory)"         = 0.050,
                "0.100"                        = 0.100,
                "0.150  (permissive / pilot)"  = 0.150
              ),
              selected = 0.025
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              uiOutput("alpha_display")
            ),
            
            # Power slider with common-power highlights
            tags$div(
              style = "margin-bottom: 4px;",
              tags$label(
                style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:6px;",
                "Power"
              ),
              sliderInput(
                "power",
                label  = NULL,
                min    = 0.70, max = 0.99,
                step   = 0.01, value = 0.80,
                ticks  = FALSE
              ),
              conditionalPanel(
                condition = "input.show_calc_hints == true",
                uiOutput("power_display")
              )
            ),
            
            conditionalPanel(
              condition = "input.prop_design != 'one_arm'",
              selectInput(
                "r", "Allocation ratio (treatment : control)",
                choices  = c("1:1" = 1, "2:1" = 2, "3:1" = 3, "4:1" = 4, "5:1" = 5),
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
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p(
                "Z (power formula) gives an analytic result. All other methods run a simulation-based search. Wilson is the recommended default.",
                style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;"
              )
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
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p("The reference or control rate. For single-arm studies, this is the performance goal (OPC) from the literature.",
                     style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
            ),
            
            sliderInput("p1.expected",
                        "Expected device event rate (p\u2081):",
                        min = 0.00, max = 1.00, step = 0.01, value = 0.93),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p("The true rate you expect the device to achieve. Must exceed p\u2080 \u2212 \u0394 for the study to be powerable.",
                     style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
            ),
            
            sliderInput("p1.tolerable",
                        "Non-inferiority margin (\u0394):",
                        min = 0.00, max = 0.20, step = 0.01, value = 0.05),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p("Maximum acceptable shortfall below p\u2080. Set to 0 for a pure superiority test. Must be clinically justified.",
                     style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
            ),
            
            selectInput(
              "WindowMargin", "Sensitivity window for NI margin (\u00b1)",
              choices = c(
                "\u00b10.005 (very narrow)" = 0.005,
                "\u00b10.010"               = 0.010,
                "\u00b10.020"               = 0.020,
                "\u00b10.050 (default)"     = 0.050,
                "\u00b10.075"               = 0.075,
                "\u00b10.100 (wide)"        = 0.100,
                "\u00b10.150 (very wide)"   = 0.150
              ),
              selected = 0.050
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
            
            checkboxInput("show_calc_code", "Show calculation code",           value = FALSE),
            checkboxInput("showNBox_prop",  "Show n at chosen \u0394",           value = TRUE),
            checkboxInput("showVline",      "Show selected value crosshair",    value = FALSE),
            checkboxInput("showTable",      "Show \u0394 sensitivity table",    value = FALSE),
            checkboxInput("showTable2",     "Show p\u2081 sensitivity table",   value = FALSE),
            
            tags$hr(class = "pgp-hr"),
            
            # Dropout rate slider
            tags$label(
              style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px;",
              "Dropout rate for enrolment estimate (%)"
            ),
            sliderInput(
              "dropout_rate",
              label  = NULL,
              min    = 1, max = 20,
              step   = 1, value = 10,
              ticks  = FALSE
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p(
                "Used to inflate n to account for expected dropout. Enrolment target = n / (1 \u2212 dropout%).",
                style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;"
              )
            ),
            
            tags$div(
              class = "dl-btn-col",
              style = "margin-top: 10px;",
              downloadButton("downloadData_plot1", "\u2193 Download \u0394 table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadData_plot2", "\u2193 Download p\u2081 table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlot1",      "\u2193 Download \u0394 plot",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlot2",      "\u2193 Download p\u2081 plot",
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
          
          conditionalPanel(
            condition = "input.show_calc_hints == true",
            tags$p("How total sample size changes as the NI margin varies. A tighter margin (smaller \u0394) requires more patients.",
                   style = "font-size:11px; color:#94a3b8; margin:0 0 4px; line-height:1.5;")
          ),
          plotlyOutput("plot1", height = "420px"),
          uiOutput("n_box_prop"),
          tags$div(style = "height:22px;"),
          conditionalPanel(
            condition = "input.show_calc_hints == true",
            tags$p("How total sample size changes as the expected device rate (p\u2081) varies. A rate closer to p\u2080 requires more patients.",
                   style = "font-size:11px; color:#94a3b8; margin:0 0 4px; line-height:1.5;")
          ),
          plotlyOutput("plot2", height = "420px"),
          uiOutput("compare_section"),
          DTOutput("dataTable"),
          DTOutput("dataTable2"),
          conditionalPanel(
            condition = "input.show_calc_code == true",
            uiOutput("calc_code_ui")
          )
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
            
            conditionalPanel(
              condition = "input.show_interim_hints == true",
              uiOutput("interim_sidebar_label")
            ),
            
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
          ),
          
          acc_panel(
            id = "acc_interim_other", heading = "Other Settings", open = FALSE,
            
            checkboxInput("show_interim_calctbl", "Show calculation table",   value = TRUE),
            checkboxInput("show_interim_citbl",   "Show CI comparison table", value = TRUE),
            checkboxInput("show_interim_code",    "Show calculation code",    value = FALSE),
            
            tags$div(
              class = "dl-btn-col",
              style = "margin-top:10px;",
              downloadButton("download_interim_calc_csv",
                             "\u2193 Download calc table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("download_interim_ci_csv",
                             "\u2193 Download CI table",
                             class = "btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadInterimPlot",
                             "\u2193 Download position plot",
                             class = "btn-sm btn-outline-primary pgp-btn")
            ),
            
            tags$div(
              style = "margin-top:14px; padding-top:12px; border-top:1px solid #f1f5f9;",
              tags$button(
                class   = "btn btn-sm btn-outline-secondary",
                style   = "font-size:12px; padding:4px 14px; border-color:#e2e8f0; color:#374151;",
                onclick = "pgpResetInterimSettings();",
                "\u21ba Defaults"
              )
            )
          )
        ),
        
        column(
          width = 8,
          class = "main-right pgp-main",
          
          conditionalPanel(
            condition = "input.show_interim_hints == true",
            uiOutput("interim_orientation_text"),
            tags$div(style = "height:10px;")
          ),
          uiOutput("interim_status_box"),
          tags$div(style = "height:14px;"),
          plotlyOutput("interim_position_plot", height = "260px"),
          conditionalPanel(
            condition = "input.show_interim_hints == true",
            tags$p(
              style = "font-size:11px; color:#718096; margin: 4px 0 18px; line-height:1.6;",
              HTML(paste0(
                "<span style='color:#e07b39; font-weight:600;'>--- NI boundary</span>",
                "&nbsp;&nbsp;|&nbsp;&nbsp;",
                "<span style='color:#718096;'>--- Reference (p\u2080 or zero diff.)</span>",
                "&nbsp;&nbsp;|&nbsp;&nbsp;",
                "<span style='color:#5b35d5; font-weight:600;'>\u25cf with bars</span>",
                " = observed estimate \u00b1 95% CI"
              ))
            )
          ),
          conditionalPanel(
            condition = "input.show_interim_calctbl != false",
            uiOutput("interim_calc_table"),
            tags$div(style = "height:18px;")
          ),
          conditionalPanel(
            condition = "input.show_interim_citbl != false",
            uiOutput("interim_ci_threshold_table")
          ),
          conditionalPanel(
            condition = "input.show_interim_code == true",
            uiOutput("interim_code_ui")
          )
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
                choices  = c("PDF (.pdf)" = "pdf", "Word (.docx)" = "docx"),
                selected = "pdf", inline = TRUE
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
                "\u21ba Load template"
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
            value       = "PG-Power \u2014 Sample Size Report",
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
        
        # Include in Report
        acc_panel(
          id = "acc_rpt_include", heading = "Include in Report", open = FALSE,
          
          tags$p("General",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:0 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_results",      "Results table",         value = TRUE),
            checkboxInput("rpt_interp_inc",   "Interpretation",        value = TRUE),
            checkboxInput("rpt_definitions",  "Definitions",           value = TRUE),
            checkboxInput("rpt_calc_code",    "Calculation code",      value = TRUE),
            checkboxInput("rpt_ci_compare",   "CI comparison table",   value = FALSE),
            checkboxInput("rpt_n_box",        "Full n summary table",  value = FALSE)
          ),
          
          tags$p("Sensitivity",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_plot_delta",  "\u0394 plot",  value = FALSE),
            checkboxInput("rpt_plot_p1",     "p\u2081 plot", value = FALSE),
            checkboxInput("rpt_table_delta", "\u0394 table", value = FALSE),
            checkboxInput("rpt_table_p1",    "p\u2081 table",value = FALSE)
          ),
          
          tags$p("Interim Analysis",
                 style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
          tags$div(
            style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_interim_summ",   "Data summary",   value = FALSE),
            checkboxInput("rpt_interim_interp", "Interpretation", value = FALSE),
            checkboxInput("rpt_interim_ci",     "CI comparison",  value = FALSE),
            checkboxInput("rpt_interim_plot",   "Position plot",  value = FALSE)
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
        ),
        
        # Interpretation
        acc_panel(
          id = "acc_rpt_interp", heading = "Interpretation", open = FALSE,
          
          tags$div(
            style = "margin-top:4px;",
            
            # Collapsible current calculator values panel
            tags$div(
              style = "margin-bottom:14px;",
              tags$div(
                style = "display:flex; align-items:center; justify-content:space-between;
                         background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px;
                         padding:8px 14px; cursor:pointer;",
                onclick = "pgpToggleCalcSummary(this);",
                tags$span(
                  style = "font-size:12px; font-weight:600; color:#374151;",
                  "Current calculator values"
                ),
                tags$span(
                  id    = "calc_summary_chevron",
                  style = "font-size:11px; color:#64748b; transition:transform 0.2s;",
                  "\u25be"
                )
              ),
              tags$div(
                id    = "calc_summary_body",
                style = "display:none;",
                uiOutput("rpt_calc_summary_ui")
              )
            ),
            
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
                  id      = "interp_load_template",
                  class   = "btn btn-sm btn-outline-secondary",
                  style   = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
                             color:#374151; white-space:nowrap;",
                  onclick = "pgpLoadTemplate();",
                  "\u21ba Load template"
                )
              ),
              tags$div(
                style = "flex:0 0 auto; padding-top:20px;",
                tags$button(
                  id      = "interp_restore_default",
                  class   = "btn btn-sm btn-outline-secondary",
                  style   = "font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0;
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
              tagList(lapply(
                list(
                  list(tag = "{n}",           label = "n"),
                  list(tag = "{n_dropout}",   label = "n (dropout)"),
                  list(tag = "{n_successes}", label = "n-successes"),
                  list(tag = "{power_pct}",   label = "Power %"),
                  list(tag = "{p0_pct}",      label = "p\u2080 %"),
                  list(tag = "{p1_pct}",      label = "p\u2081 %"),
                  list(tag = "{alpha}",       label = "\u03b1"),
                  list(tag = "{delta}",       label = "\u0394"),
                  list(tag = "{ci_method}",   label = "CI method"),
                  list(tag = "{dropout_pct}", label = "Dropout %")
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
              ))
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
                "Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. ",
                "The study will be deemed successful if at least {n_successes} out of {n} ",
                "evaluable patients are free from a major adverse event at 12 months."
              )
            )
          )
        )
      )
    )
  ),
  
  # ── JavaScript ──────────────────────────────────────────────────────────────
  tags$script(HTML("$(document).ready(function() {

// -- Hints state (global, default ON) ------------------------------------
    window.pgpHintsOn = true;

    window.pgpToggleHints = function(btn) {
      window.pgpHintsOn = !window.pgpHintsOn;
      var lbl = document.getElementById('hints_toggle_label');
      if (lbl) lbl.textContent = window.pgpHintsOn ? 'Hide hints' : 'Show hints';
      btn.style.background   = window.pgpHintsOn ? '#f0eeff' : '';
      btn.style.borderColor  = window.pgpHintsOn ? '#5b35d5' : '#e2e8f0';
      if (window.Shiny) {
        Shiny.setInputValue('show_calc_hints',    window.pgpHintsOn, {priority: 'event'});
        Shiny.setInputValue('show_interim_hints', window.pgpHintsOn, {priority: 'event'});
      }
    };

    // -- n-box expand/collapse -----------------------------------------------
    window.pgpToggleNBox = function(btn) {
      var exp = document.getElementById('n_box_expanded');
      if (!exp) return;
      var isHidden = exp.style.display === 'none' || exp.style.display === '';
      exp.style.display = isHidden ? 'block' : 'none';
      btn.textContent   = isHidden ? 'collapse ▴' : 'expand ▾';
    };

    // -- Calculator summary toggle (in report interpretation panel) ----------
    window.pgpToggleCalcSummary = function(hdr) {
      var body = document.getElementById('calc_summary_body');
      var chev = document.getElementById('calc_summary_chevron');
      if (!body) return;
      var isHidden = body.style.display === 'none' || body.style.display === '';
      body.style.display = isHidden ? 'block' : 'none';
      if (chev) chev.style.transform = isHidden ? 'rotate(180deg)' : '';
    };

    // -- Plot colour picker -------------------------------------------------
    window.pgpSetPlotColour = function(hex, btn) {
      // Update active swatch
      document.querySelectorAll('.pgp-swatch').forEach(function(s) {
        s.classList.remove('active');
      });
      btn.classList.add('active');
      // Send to Shiny
      if (window.Shiny)
        Shiny.setInputValue('plot_colour', hex, {priority: 'event'});
    };

    // Initialise plot_colour on app load
    $(document).ready(function() {
      if (window.Shiny)
        Shiny.setInputValue('plot_colour', '#5b35d5', {priority: 'event'});
    });

    // -- Title templates -----------------------------------------------------
    window.titleTemplates = {
      'default':  'PG-Power — Sample Size Report',
      'study':    'Sample Size Calculation — Study Protocol',
      'clinical': 'Clinical Investigation: Sample Size Justification',
      'stats':    'Statistical Analysis Plan — Sample Size Section',
      'blank':    ''
    };

    window.pgpSetTitle = function(txt) {
      var el = document.getElementById('rpt_title');
      if (!el) return;
      el.value = txt;
      if (window.Shiny) Shiny.setInputValue('rpt_title', txt, {priority: 'event'});
      el.dispatchEvent(new Event('input', {bubbles: true}));
    };

    window.pgpLoadTitleTemplate = function() {
      var sel = document.getElementById('title_template_select');
      var key = sel ? sel.value : 'default';
      var txt = window.titleTemplates[key];
      if (txt === undefined) txt = window.titleTemplates['default'];
      window.pgpSetTitle(txt);
    };

    window.pgpRestoreTitleDefault = function() {
      window.pgpSetTitle(window.titleTemplates['default']);
      var sel = document.getElementById('title_template_select');
      if (sel) sel.value = 'default';
    };

    // -- Include checkboxes --------------------------------------------------
    var pgpIncludeIds = [
        'rpt_results','rpt_interp_inc','rpt_ci_compare','rpt_definitions',
        'rpt_calc_code','rpt_n_box','rpt_plot_delta','rpt_plot_p1',
        'rpt_table_delta','rpt_table_p1','rpt_interim_summ','rpt_interim_interp',
        'rpt_interim_ci','rpt_interim_plot'
      ];

    window.pgpTickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = true;
        if (window.Shiny) Shiny.setInputValue(id, true, {priority: 'event'});
      });
    };

    window.pgpUntickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = false;
        if (window.Shiny) Shiny.setInputValue(id, false, {priority: 'event'});
      });
    };

    window.pgpRestoreIncludes = function() {
      var defaults = {
        'rpt_results': true, 'rpt_interp_inc': true, 'rpt_ci_compare': false,
        'rpt_definitions': true, 'rpt_calc_code': true, 'rpt_n_box': false,
        'rpt_plot_delta': false, 'rpt_plot_p1': false,
        'rpt_table_delta': false, 'rpt_table_p1': false,
        'rpt_interim_summ': false, 'rpt_interim_interp': false,
        'rpt_interim_ci': false, 'rpt_interim_plot': false
      };
      Object.keys(defaults).forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = defaults[id];
        if (window.Shiny) Shiny.setInputValue(id, defaults[id], {priority: 'event'});
      });
    };

    // -- Interpretation templates --------------------------------------------
    window.interpTemplates = {
      'blank':      '',
      'default':    'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power, that the device success rate exceeds the performance goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. The study will be deemed successful if at least {n_successes} out of {n} evaluable patients are free from a major adverse event at 12 months.',
      'concise':    'A sample size of {n} patients provides {power_pct}% power (one-sided α = {alpha}) to demonstrate non-inferiority of the device against the performance goal of {p0_pct}%, with an NI margin of Δ = {delta}, assuming a true device success rate of {p1_pct}%.',
      'two_arm':    'A total of {n} patients are required to demonstrate non-inferiority of the treatment versus the control, with {power_pct}% power and a one-sided significance level of {alpha}. The assumed event rates are {p1_pct}% (treatment) and {p0_pct}% (control), with a non-inferiority margin of {delta} on the risk difference scale. Allowing for {dropout_pct}% dropout, {n_dropout} patients should be enrolled.',
      'regulatory': 'The study is designed as a single-arm, non-inferiority study comparing the device success rate to an objective performance criterion (OPC) of {p0_pct}%, consistent with published literature and historical data. A minimum of {n} evaluable subjects is required to demonstrate, with {power_pct}% power at a one-sided significance level of {alpha}, that the lower bound of the {ci_method} confidence interval for the device success rate exceeds the performance goal less the non-inferiority margin ({delta}). Accounting for a {dropout_pct}% dropout rate, the study will enrol {n_dropout} subjects. The primary endpoint will be met if at least {n_successes} of {n} evaluable subjects achieve procedural success.',
      'safety':     'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power (one-sided α = {alpha}), that the device complication rate is non-inferior to the performance goal of {p0_pct}%, assuming a true complication rate of {p1_pct}% and an acceptable margin of {delta}. With an anticipated dropout rate of {dropout_pct}%, {n_dropout} patients will be enrolled. The safety endpoint will be satisfied if no more than the pre-specified number of adverse events are observed among the {n} evaluable patients.'
    };

    window.pgpSetInterp = function(txt) {
      var ta = document.getElementById('rpt_interp_text');
      if (!ta) return;
      ta.value = txt;
      if (window.Shiny) Shiny.setInputValue('rpt_interp_text', txt, {priority: 'event'});
    };

    window.pgpLoadTemplate = function() {
      var sel = document.getElementById('interp_template_select');
      var key = sel ? sel.value : 'default';
      var txt = (window.interpTemplates[key] !== undefined)
                  ? window.interpTemplates[key]
                  : window.interpTemplates['default'];
      window.pgpSetInterp(txt);
    };

    window.pgpRestoreDefault = function() {
      window.pgpSetInterp(window.interpTemplates['default']);
      var sel = document.getElementById('interp_template_select');
      if (sel) sel.value = 'default';
    };

    // -- Full app reset ------------------------------------------------------
    window.pgpResetAll = function() {
      window.pgpResetCalculator();
      window.pgpRestoreTitleDefault();
      window.pgpRestoreDefault();
      window.pgpRestoreIncludes();

      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('rpt_include_date',   true,  {priority: 'event'});
      S.setInputValue('rpt_include_method', true,  {priority: 'event'});
      S.setInputValue('rpt_include_author', false, {priority: 'event'});
      S.setInputValue('report_format',      'pdf', {priority: 'event'});

      S.setInputValue('interim_n',         0, {priority: 'event'});
      S.setInputValue('interim_x',         0, {priority: 'event'});
      S.setInputValue('interim_x_control', 0, {priority: 'event'});

      window.pgpResetInterimSettings();

      window.pgpHintsOn = true;
      var lbl = document.getElementById('hints_toggle_label');
      if (lbl) lbl.textContent = 'Hide hints';
      var btn = document.getElementById('hints_toggle_btn');
      if (btn) { btn.style.background = '#f0eeff'; btn.style.borderColor = '#5b35d5'; }
      S.setInputValue('show_calc_hints',    true, {priority: 'event'});
      S.setInputValue('show_interim_hints', true, {priority: 'event'});
      // Reset plot colour to purple
      document.querySelectorAll('.pgp-swatch').forEach(function(s) { s.classList.remove('active'); });
      var ps = document.getElementById('swatch_purple');
      if (ps) ps.classList.add('active');
      S.setInputValue('plot_colour', '#5b35d5', {priority: 'event'});
    };

    window.pgpResetInterimSettings = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('show_interim_calctbl', true,  {priority: 'event'});
      S.setInputValue('show_interim_citbl',   true,  {priority: 'event'});
      S.setInputValue('show_interim_code',    false, {priority: 'event'});
    };

    window.pgpResetCalculator = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('prop_design',    'one_arm',  {priority: 'event'});
      S.setInputValue('endpoint',       'efficacy', {priority: 'event'});
      S.setInputValue('sig.level',      '0.025',    {priority: 'event'});
      S.setInputValue('power',          0.80,       {priority: 'event'});
      S.setInputValue('r',              '1',        {priority: 'event'});
      S.setInputValue('ci_method_prop', 'wilson',   {priority: 'event'});
      S.setInputValue('showCompare',    false,      {priority: 'event'});
      S.setInputValue('p0.expected',    0.88,       {priority: 'event'});
      S.setInputValue('p1.expected',    0.93,       {priority: 'event'});
      S.setInputValue('p1.tolerable',   0.05,       {priority: 'event'});
      S.setInputValue('WindowMargin',   '0.05',     {priority: 'event'});
      S.setInputValue('sim_quality',    '1000',     {priority: 'event'});
      S.setInputValue('sim_seed',       1,          {priority: 'event'});
      S.setInputValue('show_calc_code', false,      {priority: 'event'});
      S.setInputValue('showNBox_prop',  true,       {priority: 'event'});
      S.setInputValue('showVline',      false,      {priority: 'event'});
      S.setInputValue('showTable',      false,      {priority: 'event'});
      S.setInputValue('showTable2',     false,      {priority: 'event'});
      S.setInputValue('dropout_rate',   10,         {priority: 'event'});
    };

    // -- Accordion toggle ----------------------------------------------------
    $(document).on('click', '.pgp-accordion-header', function() {
      var $hdr  = $(this);
      var $body = $hdr.next('.pgp-accordion-body');
      $hdr.toggleClass('open');
      $body.toggleClass('open');
    });

    // -- Sync interp textarea to Shiny ---------------------------------------
    var ta = document.getElementById('rpt_interp_text');
    if (ta) {
      Shiny.setInputValue('rpt_interp_text', ta.value);
      ta.addEventListener('input', function() {
        Shiny.setInputValue('rpt_interp_text', ta.value, {priority: 'event'});
      });
    }

});"))
)