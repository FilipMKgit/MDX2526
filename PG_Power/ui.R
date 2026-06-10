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

      .pgp-accordion-body {
        padding: 0 16px; background: #fff;
        max-height: 0; overflow: hidden;
        font-size: 13px; color: #374151;
        border-radius: 0 0 8px 8px;
        position: relative; z-index: 10;
        transition: max-height 0.2s ease, padding 0.2s ease;
      }
      .pgp-accordion-body.open { max-height: 2000px; padding: 14px 16px; overflow: visible; }
      .pgp-accordion-body .selectize-dropdown { z-index: 9999 !important; position: fixed !important; }
      .pgp-accordion { overflow: visible !important; }

      .report-panel { max-width: 700px; margin: 0 auto; padding: 24px 16px; }
      .report-group { background: #f8fafc; border: 1px solid #e2e8f0;
                      border-radius: 10px; padding: 20px 22px; margin-bottom: 18px; }
      .report-group h5 { font-size: 13px; font-weight: 700; text-transform: uppercase;
                          letter-spacing: 0.07em; color: #64748b; margin: 0 0 14px; }
      .report-group .form-group { margin-bottom: 10px; }
      .report-dl-btn {
        margin-top: 6px;
        font-size: 13px !important;
        padding: 8px 20px !important;
        font-weight: 600 !important;
        border-radius: 7px !important;
        display: inline-flex !important;
        align-items: center;
        gap: 6px;
      }
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

      /* n-box expand/collapse */
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

      /* Calc values card */
      .pgp-calc-values {
        margin-top: 10px;
        border: 1px solid #e2e8f0; border-radius: 8px;
        overflow: hidden; background: #fafcff;
      }
      .pgp-cv-section {
        font-size: 10px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.07em; color: #94a3b8;
        padding: 6px 12px 4px; background: #f1f5f9;
        border-bottom: 1px solid #e2e8f0;
        border-top: 1px solid #e2e8f0;
      }
      .pgp-cv-section:first-child { border-top: none; }
      .pgp-cv-row {
        display: flex; align-items: center;
        padding: 5px 12px; gap: 10px;
        border-bottom: 1px solid #f1f5f9;
        min-height: 30px;
      }
      .pgp-cv-row:last-child { border-bottom: none; }
      .pgp-cv-label {
        flex: 0 0 160px; font-size: 11.5px; font-weight: 600;
        color: #64748b; white-space: nowrap;
      }
      .pgp-cv-value {
        flex: 1 1 auto; font-size: 11.5px; color: #1a2e35;
        font-family: 'DM Mono', monospace;
      }
      .pgp-ins-btn {
        flex: 0 0 auto;
        display: inline-flex; align-items: center; gap: 3px;
        background: none; border: 1px solid #e2e8f0;
        border-radius: 5px; padding: 2px 7px;
        cursor: pointer; transition: border-color 0.15s, background 0.15s;
        white-space: nowrap;
      }
      .pgp-ins-btn:hover { border-color: #5b35d5; background: #f0eeff; }
      .pgp-ins-tag {
        font-size: 10px; color: #5b35d5; font-family: 'DM Mono', monospace;
        font-weight: 600;
      }
      .pgp-ins-arrow { font-size: 10px; color: #94a3b8; }

      /* Precision toggle */
      .pgp-precision-toggle {
        display: inline-flex; border: 1px solid #e2e8f0; border-radius: 6px;
        overflow: hidden;
      }
      .pgp-prec-btn {
        background: #fff; border: none; padding: 3px 10px;
        font-size: 11px; font-weight: 600; color: #94a3b8;
        cursor: pointer; transition: background 0.15s, color 0.15s;
        font-family: 'DM Sans', sans-serif; letter-spacing: 0.02em;
      }
      .pgp-prec-btn:not(:last-child) { border-right: 1px solid #e2e8f0; }
      .pgp-prec-btn.active { background: #5b35d5; color: #fff; }
      .pgp-prec-btn:hover:not(.active) { background: #f0eeff; color: #5b35d5; }

      /* 3dp numeric inputs */
      .form-group input[type=number] {
        font-size:13px; padding:5px 8px; height:34px;
        border:1px solid #e2e8f0; border-radius:6px; color:#1a2e35;
        font-family:'DM Mono',monospace; width:100%;
      }
      .form-group input[type=number]:focus { border-color:#5b35d5; outline:none;
        box-shadow:0 0 0 2px rgba(91,53,213,0.15); }

      /* Alpha slider tick labels */
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

      /* Plot colour swatches */
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
              tags$li(tags$b("Set up:"), " Enter the performance goal, expected device proportion, significance level, power, and CI method in the ", tags$b("Calculator"), " tab."),
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
              tags$li(tags$b("Endpoint direction:"), " Whether a higher or lower proportion is the favourable outcome. ", tags$em("Lower is better"), " (e.g. complication proportion) mirrors proportions internally so the same calculation applies."),
              tags$li(tags$b("\u03b1 (significance level):"), " One-sided \u03b1. 0.025 is standard for pivotal medical device studies."),
              tags$li(tags$b("Power:"), " Probability of correctly demonstrating the device meets the performance goal. 0.90 is the default."),
              tags$li(tags$b("CI method:"), " Six methods available, all using binom CI simulation (binary search). Wald is the default; Clopper-Pearson is the most conservative and is preferred for regulatory submissions.")
            ),
            tags$p(tags$b("Proportions")),
            tags$ul(
              tags$li(tags$b("Performance goal (PG):"), " The pre-specified benchmark rate the device must meet or beat."),
              tags$li(tags$b("Expected performance:"), " The true proportion you expect the device to achieve. Must be more favourable than the PG.")
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
              tags$li(tags$b("Show sensitivity plot and table:"), " How required n changes as the assumed device proportion varies."),
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
            tags$p("Enable ", tags$b("Show CI diagram"), " in Other Settings to visualise the confidence interval for the expected proportion at the required n, alongside the PG boundary. Green = passes, red = fails. Enable ",
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
              tags$li(tags$b("Plots:"), " Power vs n plot (PNG) and/or sensitivity plot (PNG)."),
              tags$li(tags$b("Tables:"), " Sensitivity table.")
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
                    the minimum or maximum acceptable event proportion that a device must
                    achieve, depending on whether a higher or lower rate is the desired outcome."),
            tags$p(tags$b("Regulatory context")),
            tags$ul(
              tags$li(
                tags$b("FDA (US):"), " The FDA guidance on non-inferiority trials (2016) and
                the Bayesian guidance (2010) describe performance goal studies as appropriate
                when a concurrent control is not feasible. ",
                tags$a(href = "https://www.fda.gov/media/78504/download", target = "_blank",
                       style = "color:#5b35d5;", "Non-Inferiority Clinical Trials to Establish Effectiveness (2016) \u2197"),
                " \u00b7 ",
                tags$a(href = "https://www.fda.gov/media/71512/download", target = "_blank",
                       style = "color:#5b35d5;", "Guidance for the Use of Bayesian Statistics (2010) \u2197")
              ),
              tags$li(
                tags$b("ISO 14155:2020:"), " Governs clinical investigation of medical devices
                for human subjects. Requires a pre-specified primary endpoint, sample size
                justification, and a defined success criterion. ",
                tags$a(href = "https://www.iso.org/standard/83968.html", target = "_blank",
                       style = "color:#5b35d5;", "ISO 14155:2020/Amd 1:2024 \u2197")
              ),
              tags$li(
                tags$b("ISO 5840 / ISO 11135 / device-specific standards:"),
                " Many device families have published OPC values in their specific ISO
                standards or FDA guidance documents. Check the relevant device standard
                or the FDA's device-specific guidance for published OPC values."
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
        
        # -- Bottom action bar ------------------------------------------------
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;
                   gap:8px; flex-wrap:wrap;
                   margin-top:18px; padding-top:14px; border-top:1px solid #f1f5f9;",
          
          tags$div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span("Plot colour:", style = "font-size:12px; color:#64748b;"),
            tags$button(class="pgp-swatch active", id="swatch_purple",
                        style="background:#5b35d5;", title="Purple",
                        onclick="pgpSetPlotColour('#5b35d5', this);"),
            tags$button(class="pgp-swatch", id="swatch_teal",
                        style="background:#18bdb9;", title="Teal",
                        onclick="pgpSetPlotColour('#18bdb9', this);"),
            tags$button(class="pgp-swatch", id="swatch_red",
                        style="background:#c0392b;", title="Red",
                        onclick="pgpSetPlotColour('#c0392b', this);"),
            tags$button(class="pgp-swatch", id="swatch_black",
                        style="background:#1a2e35;", title="Black",
                        onclick="pgpSetPlotColour('#1a2e35', this);")
          ),
          
          tags$div(
            style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
            tags$div(
              id      = "hints_toggle_btn",
              style   = "display:flex; align-items:center; gap:6px; padding:5px 14px;
                         border:1px solid #5b35d5; border-radius:6px;
                         background:#f0eeff; font-size:12px; color:#374151; cursor:pointer;",
              onclick = "pgpToggleHints(this);",
              tags$span(style="font-size:13px; font-weight:700; color:#5b35d5;", "Hints"),
              tags$span("Hide hints", id="hints_toggle_label")
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-secondary",
              style   = "font-size:12px; padding:5px 16px; border-color:#e2e8f0;
                         color:#374151; display:flex; align-items:center; gap:6px;",
              onclick = "pgpResetAll();",
              tags$span("\u21ba"), tags$span("Restore All Defaults")
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
          
          tags$div(
            style = "position:relative; z-index:100;",
            acc_panel(
              id      = "acc_design",
              heading = "Trial Design Settings",
              open    = TRUE,
              
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
                  HTML("H\u2081: p > p\u2080 &nbsp;&mdash;&nbsp; device proportion must exceed the performance goal.<br>Use when a <em>higher</em> observed proportion means the device performed well."),
                  style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.6;"
                )
              ),
              conditionalPanel(
                condition = "input.show_calc_hints == true && input.endpoint == 'safety'",
                tags$p(
                  HTML("H\u2081: p < p\u2080 &nbsp;&mdash;&nbsp; device proportion must stay below the performance goal.<br>Use when a <em>lower</em> observed proportion means the device performed well."),
                  style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.6;"
                )
              ),
              
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
              
              tags$div(
                style = "margin-bottom: 4px;",
                tags$label(
                  style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:6px;",
                  "Power"
                ),
                sliderInput("power", label=NULL, min=0.70, max=0.99, step=0.01, value=0.90, ticks=FALSE),
                conditionalPanel(
                  condition = "input.show_calc_hints == true",
                  uiOutput("power_display")
                )
              ),
              
              tags$hr(class = "pgp-hr"),
              
              selectInput(
                "ci_method_prop", "CI method",
                choices = c(
                  "Wald (Z-score)"          = "asymptotic",
                  "Wilson Score"            = "wilson",
                  "Agresti-Coull"           = "ac",
                  "Clopper-Pearson (Exact)" = "exact",
                  "Prop.test"               = "prop.test",
                  "Jeffreys"                = "bayes"
                ),
                selected = "exact"
              ),
              conditionalPanel(
                condition = "input.show_calc_hints == true",
                tags$p(
                  "All methods run a binom CI simulation search. Wilson is the recommended default; Clopper-Pearson is the most conservative.",
                  style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;"
                )
              )
            )
          ),
          
          acc_panel(
            id      = "acc_props",
            heading = "Proportions",
            open    = TRUE,
            
            tags$div(
              style = "display:flex; justify-content:flex-end; margin-bottom:10px;",
              tags$div(
                class = "pgp-precision-toggle",
                tags$button(id="prec_2dp", class="pgp-prec-btn active",
                            onclick="pgpSetPrecision(2, this);", "2 d.p."),
                tags$button(id="prec_3dp", class="pgp-prec-btn",
                            onclick="pgpSetPrecision(3, this);", "3 d.p.")
              )
            ),
            
            tags$label(
              style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px;",
              "Performance goal (PG):"
            ),
            conditionalPanel(
              condition = "input.prop_precision != '3dp'",
              sliderInput("p0.expected", label=NULL, min=0.00, max=1.00, step=0.01, value=0.88, ticks=FALSE)
            ),
            conditionalPanel(
              condition = "input.prop_precision == '3dp'",
              numericInput("p0.manual", label=NULL, value=0.880, min=0.000, max=1.000, step=0.001)
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p("The pre-specified benchmark rate the device must meet or exceed.",
                     style = "font-size:11px; color:#94a3b8; margin:-4px 0 8px; line-height:1.5;")
            ),
            
            tags$label(
              style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px; margin-top:8px;",
              "Expected performance:"
            ),
            conditionalPanel(
              condition = "input.prop_precision != '3dp'",
              sliderInput("p1.expected", label=NULL, min=0.00, max=1.00, step=0.01, value=0.93, ticks=FALSE)
            ),
            conditionalPanel(
              condition = "input.prop_precision == '3dp'",
              numericInput("p1.manual", label=NULL, value=0.930, min=0.000, max=1.000, step=0.001)
            ),
            conditionalPanel(
              condition = "input.show_calc_hints == true",
              tags$p("The true proportion you expect the device to achieve. Must be more favourable than the performance goal.",
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
            numericInput("sim_seed", "Simulation seed", value=1, min=1, step=1)
          ),
          
          acc_panel(
            id      = "acc_other",
            heading = "Other Settings",
            open    = FALSE,
            
            checkboxInput("show_calc_code",  "Show calculation code",          value=FALSE),
            checkboxInput("showNBox_prop",   "Show n result box",              value=TRUE),
            checkboxInput("showVline",       "Show crosshair at required n",   value=TRUE),
            checkboxInput("showCIDiagram",   "Show CI diagram",                value=FALSE),
            checkboxInput("showAllCI",       "Show all CI methods in diagram", value=FALSE),
            checkboxInput("showPowerTable",  "Show power vs n table",          value=FALSE),
            checkboxInput("showTable2",      "Show sensitivity plot and table", value=FALSE),
            
            tags$hr(class = "pgp-hr"),
            
            tags$label(
              style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px;",
              "Power plot range (\u00b1 n around required n)"
            ),
            sliderInput("power_plot_range", label=NULL, min=10, max=300, step=10, value=50, ticks=FALSE),
            
            tags$hr(class = "pgp-hr"),
            
            tags$label(
              style = "font-size:13px; font-weight:400; color:#212529; display:block; margin-bottom:4px;",
              "Dropout rate for enrolment estimate (%)"
            ),
            sliderInput("dropout_rate", label=NULL, min=1, max=20, step=1, value=10, ticks=FALSE),
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
              downloadButton("downloadPowerTable", "\u2193 Download power table (.csv)",
                             class="btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadData_plot2", "\u2193 Download sensitivity table (.csv)",
                             class="btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlot2",      "\u2193 Download sensitivity plot (.png)",
                             class="btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlotPower",  "\u2193 Download power vs n plot (.png)",
                             class="btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlotCI",     "\u2193 Download CI diagram (.png)",
                             class="btn-sm btn-outline-primary pgp-btn"),
              downloadButton("downloadPlotCIAll",  "\u2193 Download CI diagram \u2014 all methods (.png)",
                             class="btn-sm btn-outline-primary pgp-btn")
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
            tags$p("Power vs sample size for the current performance goal and expected proportion. The orange dot marks the required n and the dashed line is the target power.",
                   style = "font-size:11px; color:#94a3b8; margin:0 0 4px; line-height:1.5;")
          ),
          plotlyOutput("plot_power", height="380px"),
          uiOutput("n_box_prop"),
          tags$div(style="height:22px;"),
          conditionalPanel(
            condition = "input.showTable2 == true",
            plotlyOutput("plot2", height="380px"),
            tags$div(style="height:12px;")
          ),
          uiOutput("compare_section"),
          conditionalPanel(
            condition = "input.showCIDiagram == true",
            tags$div(style="height:16px;"),
            uiOutput("ci_diagram_wrapper")
          ),
          conditionalPanel(
            condition = "input.showPowerTable == true",
            tags$div(style="height:16px;"),
            uiOutput("power_table_ui")
          ),
          conditionalPanel(
            condition = "input.showTable2 == true",
            tags$div(style="height:8px;"),
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
    # Tab 3 - Generate Report
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Generate Report",
      
      tags$div(
        class = "report-panel",
        
        tags$div(
          class = "report-export-row",
          
          tags$div(
            class = "report-export-left",
            radioButtons(
              "report_format", label=NULL,
              choices=c("PDF (.pdf)"="pdf", "Word (.docx)"="docx"),
              selected="pdf", inline=TRUE
            ),
            uiOutput("report_download_ui"),
            tags$p(class="report-note", style="margin-top:8px;",
                   "Report is built from your current Calculator tab inputs.")
          ),
          
          tags$div(
            class = "report-export-right",
            tags$p(style="font-size:11px; font-weight:700; text-transform:uppercase;
                          letter-spacing:0.06em; color:#64748b; margin:0 0 8px;",
                   "Report contents"),
            uiOutput("report_contents_ui")
          )
        ),
        
        acc_panel(
          id="acc_rpt_header", heading="Title & Header", open=FALSE,
          
          tags$div(
            style="display:flex; align-items:center; gap:10px; margin-bottom:10px; flex-wrap:wrap;",
            tags$div(
              style="flex:1 1 auto; min-width:180px;",
              tags$label(
                style="font-size:11px; font-weight:700; text-transform:uppercase;
                        letter-spacing:0.06em; color:#64748b; display:block; margin-bottom:4px;",
                "Title template"
              ),
              tags$select(
                id="title_template_select", class="form-control",
                style="font-size:12px; height:32px; padding:4px 8px; color:#374151;
                       border:1px solid #e2e8f0; border-radius:6px; background:#fafcff;",
                tags$option(value="default",  "Default \u2014 PG-Power Sample Size Report"),
                tags$option(value="study",    "Study protocol title"),
                tags$option(value="clinical", "Clinical investigation title"),
                tags$option(value="stats",    "Statistical analysis plan title"),
                tags$option(value="blank",    "Blank \u2014 enter your own")
              )
            ),
            tags$div(
              style="flex:0 0 auto; padding-top:20px;",
              tags$button(class="btn btn-sm btn-outline-secondary",
                          style="font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0; color:#374151; white-space:nowrap;",
                          onclick="pgpLoadTitleTemplate();", "\u21ba Load template")
            ),
            tags$div(
              style="flex:0 0 auto; padding-top:20px;",
              tags$button(class="btn btn-sm btn-outline-secondary",
                          style="font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0; color:#374151; white-space:nowrap;",
                          onclick="pgpRestoreTitleDefault();", "\u21ba Defaults")
            )
          ),
          
          textInput("rpt_title", label="Report title",
                    value="PG-Power \u2014 Sample Size Report",
                    placeholder="Report title..."),
          
          tags$div(
            style="display:flex; gap:16px; flex-wrap:wrap; margin-bottom:4px;",
            checkboxInput("rpt_include_date",   "Include generation date", value=TRUE),
            checkboxInput("rpt_include_method", "Include CI method",       value=TRUE),
            checkboxInput("rpt_include_author", "Include author name",     value=FALSE)
          ),
          
          conditionalPanel(
            condition="input.rpt_include_author == true",
            textInput("rpt_author_name", label=NULL, value="", placeholder="Author name...")
          )
        ),
        
        acc_panel(
          id="acc_rpt_include", heading="Include in Report", open=FALSE,
          
          tags$p("General", style="font-size:10px; font-weight:700; text-transform:uppercase;
                  letter-spacing:0.07em; color:#94a3b8; margin:0 0 4px;"),
          tags$div(
            style="display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_results",     "Results table",       value=TRUE),
            checkboxInput("rpt_interp_inc",  "Interpretation",      value=TRUE),
            checkboxInput("rpt_definitions", "Definitions",         value=TRUE),
            checkboxInput("rpt_calc_code",   "Calculation code",    value=TRUE),
            checkboxInput("rpt_ci_compare",  "CI comparison table", value=FALSE),
            checkboxInput("rpt_n_box",       "Full n summary table", value=FALSE)
          ),
          
          tags$p("Plots & tables", style="font-size:10px; font-weight:700; text-transform:uppercase;
                  letter-spacing:0.07em; color:#94a3b8; margin:8px 0 4px;"),
          tags$div(
            style="display:grid; grid-template-columns:1fr 1fr; gap:0 12px;",
            checkboxInput("rpt_ci_diagram", "CI diagram",        value=FALSE),
            checkboxInput("rpt_plot_power", "Power vs n plot",   value=FALSE),
            checkboxInput("rpt_plot_p1",    "Sensitivity plot",  value=FALSE),
            checkboxInput("rpt_table_p1",   "Sensitivity table", value=FALSE)
          ),
          
          tags$div(
            style="margin-top:10px; padding-top:8px; border-top:1px solid #f1f5f9; display:flex; gap:6px; flex-wrap:wrap;",
            tags$button(class="btn btn-sm btn-outline-secondary",
                        style="font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
                        onclick="pgpRestoreIncludes();", "\u21ba Defaults"),
            tags$button(class="btn btn-sm btn-outline-secondary",
                        style="font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
                        onclick="pgpTickAllIncludes();", "\u2713 Tick all"),
            tags$button(class="btn btn-sm btn-outline-secondary",
                        style="font-size:12px; padding:4px 12px; border-color:#e2e8f0; color:#374151;",
                        onclick="pgpUntickAllIncludes();", "\u2715 Untick all")
          )
        ),
        
        acc_panel(
          id="acc_rpt_interp", heading="Interpretation", open=FALSE,
          
          tags$div(
            style="margin-top:4px;",
            
            tags$div(
              style="display:flex; align-items:center; gap:10px; margin-bottom:12px; flex-wrap:wrap;",
              tags$div(
                style="flex:1 1 auto; min-width:180px;",
                tags$label(
                  style="font-size:11px; font-weight:700; text-transform:uppercase;
                          letter-spacing:0.06em; color:#64748b; display:block; margin-bottom:4px;",
                  "Template"
                ),
                tags$select(
                  id="interp_template_select", class="form-control",
                  style="font-size:12px; height:32px; padding:4px 8px; color:#374151;
                         border:1px solid #e2e8f0; border-radius:6px; background:#fafcff;",
                  tags$option(value="default",       "Default \u2014 success, full statement"),
                  tags$option(value="concise",       "Concise \u2014 brief statistical"),
                  tags$option(value="regulatory",    "Regulatory \u2014 ISO / FDA formal"),
                  tags$option(value="success_ci",    "Success \u2014 CI-focused"),
                  tags$option(value="success_power", "Success \u2014 power justification"),
                  tags$option(value="safety",        "Safety \u2014 complication rate"),
                  tags$option(value="safety_ci",     "Safety \u2014 CI upper bound"),
                  tags$option(value="safety_reg",    "Safety \u2014 regulatory formal"),
                  tags$option(value="blank",         "Blank \u2014 start from scratch")
                )
              ),
              tags$div(
                style="flex:0 0 auto; padding-top:20px;",
                tags$button(id="interp_load_template", class="btn btn-sm btn-outline-secondary",
                            style="font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0; color:#374151; white-space:nowrap;",
                            onclick="pgpLoadTemplate();", "\u21ba Load template")
              ),
              tags$div(
                style="flex:0 0 auto; padding-top:20px;",
                tags$button(id="interp_restore_default", class="btn btn-sm btn-outline-secondary",
                            style="font-size:12px; height:32px; padding:0 12px; border-color:#e2e8f0; color:#374151; white-space:nowrap;",
                            onclick="pgpRestoreDefault();", "\u21ba Defaults")
              )
            ),
            
            tags$textarea(
              id="rpt_interp_text", class="form-control interp-textarea",
              rows="6", placeholder="Interpretation text...",
              paste0(
                "A total of {n} evaluable patients are required to demonstrate, with ",
                "{power_pct}% power, that the device success proportion exceeds the performance ",
                "goal of {p0_pct}%, assuming a true success proportion of {p1_pct}%. ",
                "Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. ",
                "The study will be deemed successful if at least {n_successes} out of {n} ",
                "evaluable patients are free from a major adverse event at 12 months."
              )
            ),
            
            uiOutput("rpt_calc_summary_ui")
          )
        )
      )
    )
  ),
  
  tags$script(src = "app.js")
)