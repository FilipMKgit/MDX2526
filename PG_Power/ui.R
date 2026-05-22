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
      .pgp-accordion-body .selectize-dropdown { z-index: 9999 !important; position: absolute !important; }
      .pgp-accordion { overflow: visible !important; }
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
            tags$p("PG-Power calculates the sample size needed to demonstrate that a medical device meets a pre-specified performance goal (PG) for a binary endpoint, using binom CI simulation methods."),
            tags$ol(
              tags$li(tags$b("Set up:"), " Enter the performance goal, expected device rate, significance level, power, and CI method in the ", tags$b("Calculator"), " tab."),
              tags$li(tags$b("Explore:"), " The power vs n plot shows the sawtooth exact binomial power curve. The CI diagram shows the interval at the required n."),
              tags$li(tags$b("Export:"), " Download a PDF or Word report from the ", tags$b("Generate Report"), " tab with any combination of tables, plots, and interpretation.")
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
              tags$li(tags$b("Endpoint direction:"), " Whether a higher or lower rate is the favourable outcome. ", tags$em("Lower is better"), " (e.g. complication rate) mirrors proportions internally so the same calculation applies."),
              tags$li(tags$b("\u03b1 (significance level):"), " One-sided \u03b1. 0.025 is standard for pivotal medical device studies."),
              tags$li(tags$b("Power:"), " Probability of correctly demonstrating the device meets the performance goal. 0.90 is the default."),
              tags$li(tags$b("CI method:"), " Six methods available, all using binom CI simulation (binary search). Wald is the default; Clopper-Pearson is the most conservative and is preferred for regulatory submissions.")
            ),
            tags$p(tags$b("Proportions")),
            tags$ul(
              tags$li(tags$b("Performance goal (PG):"), " The pre-specified benchmark rate the device must meet or beat."),
              tags$li(tags$b("Expected device rate:"), " The true rate you expect the device to achieve. Must be more favourable than the PG.")
            ),
            tags$p(tags$b("Main plot: n vs achieved power")),
            tags$p("Shows the exact binomial power at every sample size in a range around the required n. The green dot marks required n; the dashed line marks the target power. The annotation shows the minimum events needed to pass and the actual achieved power at that n."),
            tags$p(tags$b("CI method comparison table")),
            tags$p("Always visible below the n result box. Shows the required n under each CI method. Green rows meet or beat the required n; bold text = currently selected method."),
            tags$p(tags$b("n Result Box")),
            tags$p("Shows required n, hypotheses, CI equivalent, actual achieved power, min/max events needed, and the dropout-adjusted enrolment target."),
            tags$p(tags$b("Other Settings")),
            tags$ul(
              tags$li(tags$b("Power plot range:"), " Sets how many n values either side of required n are shown in the power plot (default \u00b150)."),
              tags$li(tags$b("Show power vs n table:"), " A full table of the plot data with a pass/fail column and which CI method(s) land at each n."),
              tags$li(tags$b("Show device rate sensitivity plot and table:"), " How required n changes as the assumed device rate varies."),
              tags$li(tags$b("Dropout rate:"), " Inflates n to account for expected dropout."),
              tags$li(tags$b("Simulation quality / seed:"), " Number of simulations and random seed for the binary search."),
              tags$li(tags$b("Defaults"), " resets all Calculator inputs.")
            )
          )
        ),
        
        acc_panel(
          id      = "acc_ci",
          heading = "Confidence Intervals",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("All six CI methods use a binom simulation-based binary search to find the smallest n
                    where the CI bound clears the performance goal at the target power. Wald is the default."),
            tags$ul(
              tags$li(tags$b("Wald (Z-score):"), " Normal approximation. Fast; default. Less accurate at extreme proportions or small n."),
              tags$li(tags$b("Wilson Score:"), " Good coverage at all proportions including near 0 or 1. Recommended for most cases."),
              tags$li(tags$b("Agresti\u2013Coull:"), " Small correction to Wilson; nearly identical in practice."),
              tags$li(tags$b("Clopper\u2013Pearson (Exact):"), " Inverts the exact binomial test. Most conservative \u2014 gives the largest n. Preferred for regulatory submissions."),
              tags$li(tags$b("Prop.test:"), " Score / chi-squared interval; close to Wilson."),
              tags$li(tags$b("Jeffreys:"), " Equal-tailed Bayesian credible interval with a Jeffreys prior. Good frequentist coverage.")
            ),
            tags$hr(class = "pgp-hr"),
            tags$p(tags$b("Decision rule:")),
            tags$p(tags$em("Higher is better:"), " declare success if CI lower bound > PG"),
            tags$p(tags$em("Lower is better:"), "  declare success if CI upper bound < PG"),
            tags$p(tags$b("CI diagram")),
            tags$p("Enable ", tags$b("Show CI diagram"), " in Other Settings to visualise the confidence interval for the expected device rate at the required n, alongside the PG boundary. Green = passes, red = fails. Enable ",
                   tags$b("Show all CI methods"), " to plot intervals for every method at once (note: this requires a short simulation run for each method)."),
            tags$div(
              style = "margin-top:14px; background:#f0eeff; border:1px solid #5b35d5;
                       border-radius:8px; padding:11px 14px;",
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
              tags$li("Edit a free-text paragraph using built-in templates as a starting point."),
              tags$li("Click tag buttons (e.g. ", tags$code("{n}"), ", ", tags$code("{pg_pct}"), ", ", tags$code("{dropout_pct}"), ") to insert live values from the current inputs."),
              tags$li("A collapsible ", tags$b("Current calculator values"), " panel shows all key inputs and outputs without switching tabs.")
            ),
            tags$p(tags$b("Include in Report")),
            tags$p("Toggle any combination of sections:"),
            tags$ul(
              tags$li(tags$b("General:"), " Results table, full n summary, interpretation, CI method comparison, definitions, calculation code."),
              tags$li(tags$b("Plots:"), " Power vs n plot (PNG) and/or device rate sensitivity plot (PNG)."),
              tags$li(tags$b("Tables:"), " Device rate sensitivity table.")
            ),
            tags$p("The live ", tags$b("Report contents"), " checklist in the top-right reflects your current selections.")
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
              tags$li(
                tags$b("FDA (US):"), " The FDA guidance on non-inferiority trials (2016) and
                the Bayesian guidance (2010) describe performance goal studies as appropriate
                when a concurrent control is not feasible. ",
                tags$a(
                  href = "https://www.fda.gov/media/78504/download",
                  target = "_blank", style = "color:#5b35d5;",
                  "Non-Inferiority Clinical Trials to Establish Effectiveness (2016) ↗"
                ), " · ",
                tags$a(
                  href = "https://www.fda.gov/media/71512/download",
                  target = "_blank", style = "color:#5b35d5;",
                  "Guidance for the Use of Bayesian Statistics (2010) ↗"
                )
              ),
              tags$li(
                tags$b("ISO 14155:2020:"), " Governs clinical investigation of medical devices
                for human subjects. Requires a pre-specified primary endpoint, sample size
                justification, and a defined success criterion. ",
                tags$a(
                  href = "https://www.iso.org/standard/83968.html",
                  target = "_blank", style = "color:#5b35d5;",
                  "ISO 14155:2020/Amd 1:2024 ↗"
                )
              ),
              tags$li(
                tags$b("ISO 5840 / ISO 11135 / device-specific standards:"),
                " Many device families have published OPC values in their specific ISO
                standards or FDA guidance documents. Check the relevant device standard
                or the FDA’s device-specific guidance for published OPC values."
              )
            ),
            tags$p(tags$b("One-sided vs two-sided testing")),
            tags$p("Performance goal studies typically use a ", tags$b("one-sided test"),
                   " at \u03b1 = 0.025 (equivalent to a 95% CI lower bound)
                   or \u03b1 = 0.05 (90% CI lower bound)."),
            tags$div(
              style = "margin-top:14px; background:#fff8ee; border:1px solid #e8c96a;
                       border-radius:8px; padding:11px 14px;",
              tags$p(style = "margin:0; font-size:12px; color:#7a5c00;",
                     tags$b("Important: "),
                     "PG-Power is a planning tool. The performance goal, CI method, and
                      \u03b1 must all be pre-specified in the study protocol before data
                      collection begins.")
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
            
            tags$hr(style = "border-color:#f1f5f9; margin:8px 0;"),
            tags$p(
              style = "font-size:11.5px; color:#64748b; line-height:1.8;",
              tags$b("R packages: "),
              tags$code("shiny"), ", ", tags$code("bslib"), ", ",
              tags$code("ggplot2"), ", ", tags$code("plotly"), ", ",
              tags$code("DT"), ", ", tags$code("binom"), ", ",
              tags$code("TrialSize"), ", ",
              tags$code("officer"), ", ", tags$code("base64enc"), ", ",
              tags$code("shinybusy")
            ),
            tags$p(
              style = "font-size:12px; color:#374151; line-height:1.8;",
              "Special thanks to ", tags$b("Prof. John Newell"), ", University of Galway,
               for supervision and guidance throughout this project."
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
          
          tags$div(style = "position:relative; z-index:100;",
                   acc_panel(
                     id      = "acc_design",
                     heading = "Trial Design Settings",
                     open    = TRUE,
                     
                     
                     
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
                         HTML("H\u2081: p > p\u2080 &nbsp;&mdash;&nbsp; device rate must exceed the performance goal.<br>Use when a <em>higher</em> observed rate means the device performed well."),
                         style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.6;"
                       )
                     ),
                     conditionalPanel(
                       condition = "input.show_calc_hints == true && input.endpoint == 'safety'",
                       tags$p(
                         HTML("H\u2081: p < p\u2080 &nbsp;&mdash;&nbsp; device rate must stay below the performance goal.<br>Use when a <em>lower</em> observed rate means the device performed well."),
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
                         step   = 0.01, value = 0.90,
                         ticks  = FALSE
                       ),
                       conditionalPanel(
                         condition = "input.show_calc_hints == true",
                         uiOutput("power_display")
                       )
                     ),
                     
                     
                     
                     tags$hr(class = "pgp-hr"),
                     
                     selectInput(
                       "ci_method_prop", "CI method",
                       choices = c(
                         "Wald (Z-score)"    = "asymptotic",
                         "Wilson Score"      = "wilson",
                         "Agresti-Coull"     = "ac",
                         "Clopper-Pearson (Exact)" = "exact",
                         "Prop.test"         = "prop.test",
                         "Jeffreys"          = "bayes"
                       ),
                       selected = "asymptotic"
                     ),
                     conditionalPanel(
                       condition = "input.show_calc_hints == true",
                       tags$p(
                         "All methods run a binom CI simulation search. Wilson is the recommended default; Clopper-Pearson is the most conservative.",
                         style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;"
                       )
                     ),
                   ),  # end z-index wrapper
                   
                   acc_panel(
                     id      = "acc_props",
                     heading = "Proportions",
                     open    = TRUE,
                     
                     sliderInput("p0.expected",
                                 "Performance goal (PG):",
                                 min = 0.00, max = 1.00, step = 0.01, value = 0.88),
                     conditionalPanel(
                       condition = "input.show_calc_hints == true",
                       tags$p("The pre-specified benchmark rate the device must meet or exceed. Typically sourced from published literature, prior device data, or a regulatory guidance document.",
                              style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
                     ),
                     
                     sliderInput("p1.expected",
                                 "Expected device rate:",
                                 min = 0.00, max = 1.00, step = 0.01, value = 0.93),
                     conditionalPanel(
                       condition = "input.show_calc_hints == true",
                       tags$p("The true rate you expect the device to achieve. Must be more favourable than the performance goal for the study to be achievable.",
                              style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
                     )
                   ),
                   
                   acc_panel(
                     id      = "acc_sim",
                     heading = "Simulation Settings",
                     open    = FALSE,
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
                   
                   acc_panel(
                     id      = "acc_other",
                     heading = "Other Settings",
                     open    = FALSE,
                     
                     checkboxInput("show_calc_code",   "Show calculation code",                      value = FALSE),
                     checkboxInput("showNBox_prop",   "Show n result box",                          value = TRUE),
                     checkboxInput("showVline",       "Show crosshair at required n",               value = TRUE),
                     checkboxInput("showCIDiagram",   "Show CI diagram",                            value = FALSE),
                     checkboxInput("showAllCI",       "Show all CI methods in diagram",             value = FALSE),
                     checkboxInput("showPowerTable",  "Show power vs n table",                      value = FALSE),
                     checkboxInput("showTable2",      "Show device rate sensitivity plot and table", value = FALSE),
                     
                     tags$hr(class = "pgp-hr"),
                     
                     tags$label(
                       style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px;",
                       "Power plot range (± n around required n)"
                     ),
                     sliderInput(
                       "power_plot_range",
                       label  = NULL,
                       min    = 10, max    = 300,
                       step   = 10, value = 50,
                       ticks  = FALSE
                     ),
                     
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
                       downloadButton("downloadPowerTable", "↓ Download power table (.csv)",
                                      class = "btn-sm btn-outline-primary pgp-btn"),
                       downloadButton("downloadData_plot2", "↓ Download sensitivity table",
                                      class = "btn-sm btn-outline-primary pgp-btn"),
                       downloadButton("downloadPlot2",      "↓ Download sensitivity plot",
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
              tags$p("Power vs sample size for the current performance goal and device rate. The orange dot marks the required n and the dashed line is the target power.",
                     style = "font-size:11px; color:#94a3b8; margin:0 0 4px; line-height:1.5;")
            ),
            plotlyOutput("plot_power", height = "380px"),
            uiOutput("n_box_prop"),
            tags$div(style = "height:22px;"),
            conditionalPanel(
              condition = "input.showTable2 == true",
              plotlyOutput("plot2", height = "380px"),
              tags$div(style = "height:12px;")
            ),
            uiOutput("compare_section"),
            conditionalPanel(
              condition = "input.showCIDiagram == true",
              tags$div(style = "height:16px;"),
              uiOutput("ci_diagram_wrapper")
            ),
            conditionalPanel(
              condition = "input.showPowerTable == true",
              tags$div(style = "height:16px;"),
              uiOutput("power_table_ui")
            ),
            conditionalPanel(
              condition = "input.showTable2 == true",
              tags$div(style = "height:8px;"),
              DTOutput("dataTable2")
            ),
            conditionalPanel(
              condition = "input.show_calc_code == true",
              uiOutput("calc_code_ui")
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
            
            tags$p("Plots & tables",
                   style = "font-size:10px; font-weight:700; text-transform:uppercase;
                           letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
            tags$div(
              style = "display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
              checkboxInput("rpt_ci_diagram", "CI diagram",                   value = FALSE),
              checkboxInput("rpt_plot_power", "Power vs n plot",              value = FALSE),
              checkboxInput("rpt_plot_p1",    "Device rate sensitivity plot", value = FALSE),
              checkboxInput("rpt_table_p1",   "Device rate sensitivity table",value = FALSE)
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
                    list(tag = "{pg_pct}",      label = "PG %"),
                    list(tag = "{pd_pct}",      label = "device rate %"),
                    list(tag = "{alpha}",       label = "\u03b1"),
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
      }
    };

    // -- Code block toggle (overview) ----------------------------------------
    window.pgpToggleCode = function(id, hdr) {
      var body = document.getElementById(id);
      if (!body) return;
      var chev = hdr.querySelector('span:last-child');
      var isHidden = body.style.display === 'none' || body.style.display === '';
      body.style.display = isHidden ? 'block' : 'none';
      if (chev) chev.style.transform = isHidden ? 'rotate(180deg)' : '';
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
        'rpt_calc_code','rpt_n_box','rpt_ci_diagram','rpt_plot_power','rpt_plot_p1','rpt_table_p1'
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
        'rpt_ci_diagram': false, 'rpt_plot_power': false, 'rpt_plot_p1': false, 'rpt_table_p1': false
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
      'default':    'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power, that the device rate meets the performance goal of {pg_pct}%, assuming a true device rate of {pd_pct}%. Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. The study will be deemed successful if at least {n_successes} out of {n} evaluable patients achieve the primary endpoint.',
      'concise':    'A sample size of {n} patients provides {power_pct}% power (one-sided α = {alpha}) to demonstrate non-inferiority of the device against the performance goal of {p0_pct}%, assuming a true device success rate of {p1_pct}%.',
      
      'regulatory': 'The study is designed as a single-arm performance goal study comparing the device rate to an objective performance criterion (OPC) of {pg_pct}%, consistent with published literature and historical data. A minimum of {n} evaluable subjects is required to demonstrate, with {power_pct}% power at a one-sided significance level of {alpha}, that the {ci_method} confidence interval bound for the device rate meets the performance goal. Accounting for a {dropout_pct}% dropout rate, the study will enrol {n_dropout} subjects. The primary endpoint will be met if at least {n_successes} of {n} evaluable subjects achieve the primary endpoint.',
      'safety':     'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power (one-sided α = {alpha}), that the device complication rate is non-inferior to the performance goal of {p0_pct}%, assuming a true complication rate of {p1_pct}% . With an anticipated dropout rate of {dropout_pct}%, {n_dropout} patients will be enrolled. The safety endpoint will be satisfied if no more than the pre-specified number of adverse events are observed among the {n} evaluable patients.'
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



      window.pgpHintsOn = true;
      var lbl = document.getElementById('hints_toggle_label');
      if (lbl) lbl.textContent = 'Hide hints';
      var btn = document.getElementById('hints_toggle_btn');
      if (btn) { btn.style.background = '#f0eeff'; btn.style.borderColor = '#5b35d5'; }
      S.setInputValue('show_calc_hints',    true, {priority: 'event'});
      // Reset plot colour to purple
      document.querySelectorAll('.pgp-swatch').forEach(function(s) { s.classList.remove('active'); });
      var ps = document.getElementById('swatch_purple');
      if (ps) ps.classList.add('active');
      S.setInputValue('plot_colour', '#5b35d5', {priority: 'event'});
    };



    window.pgpResetCalculator = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('endpoint',       'efficacy', {priority: 'event'});
      S.setInputValue('sig.level',      '0.025',    {priority: 'event'});
      S.setInputValue('power',          0.90,       {priority: 'event'});
      S.setInputValue('ci_method_prop', 'asymptotic', {priority: 'event'});
      S.setInputValue('p0.expected',    0.88,       {priority: 'event'});
      S.setInputValue('p1.expected',    0.93,       {priority: 'event'});
      S.setInputValue('sim_quality',    '1000',     {priority: 'event'});
      S.setInputValue('sim_seed',       1,          {priority: 'event'});
      S.setInputValue('show_calc_code', false,      {priority: 'event'});
      S.setInputValue('showNBox_prop',  true,       {priority: 'event'});
      S.setInputValue('showVline',      false,      {priority: 'event'});
      S.setInputValue('showTable2',      false, {priority: 'event'});
      S.setInputValue('showCIDiagram',   false, {priority: 'event'});
      S.setInputValue('showAllCI',       false, {priority: 'event'});
      S.setInputValue('showPowerTable',   false, {priority: 'event'});
      S.setInputValue('dropout_rate',   10,         {priority: 'event'});
      S.setInputValue('power_plot_range', 50,         {priority: 'event'});
    };

    // -- Show code -> GitHub popup ------------------------------------------
    window.pgpClosePopup = function() {
      var p = document.getElementById('pgp-gh-popup');
      if (p) p.remove();
    };
    Shiny.addCustomMessageHandler('showGithubPopup', function(url) {
      window.pgpClosePopup();
      var d = document.createElement('div');
      d.id = 'pgp-gh-popup';
      d.style.cssText = 'position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);' +
        'background:#fff;border:1px solid #e2e8f0;border-radius:12px;' +
        'padding:24px 28px;z-index:9999;box-shadow:0 8px 32px rgba(0,0,0,0.18);' +
        'max-width:380px;width:90%;font-family:DM Sans,sans-serif;';
      d.innerHTML =
        '<div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:12px;">' +
        '  <span style="font-weight:700;font-size:14px;color:#1a2e35;">Source code</span>' +
                       '  <button onclick="window.pgpClosePopup();" ' +
                       '    style="background:none;border:none;cursor:pointer;font-size:18px;color:#94a3b8;">&#x2715;</button>' +
                       '</div>' +
                       '<p style="font-size:12px;color:#374151;margin:0 0 14px;line-height:1.6;">' +
                       'The full source code for PG-Power is available on GitHub.</p>' +
                       '<a href="' + url + '" target="_blank" style="display:inline-flex;align-items:center;' +
                       'gap:6px;background:#5b35d5;color:#fff;text-decoration:none;padding:8px 16px;' +
                       'border-radius:7px;font-size:12px;font-weight:600;">&#x2197; Open on GitHub</a>' +
                       '<button onclick="window.pgpClosePopup();" ' +
                       'style="display:inline-block;margin-left:10px;background:none;border:1px solid #e2e8f0;' +
                       'border-radius:7px;padding:8px 14px;font-size:12px;color:#374151;cursor:pointer;">Close</button>';
                     document.body.appendChild(d);
                     setTimeout(function() {
                       document.addEventListener('click', function pgpClose(e) {
                         if (!d.contains(e.target)) { d.remove(); document.removeEventListener('click', pgpClose); }
                       });
                     }, 100);
                     });
                
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