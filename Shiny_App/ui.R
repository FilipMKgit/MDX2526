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
  
  # -- Head: fonts + custom CSS -------------------------------------------------
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,600;1,9..40,300&family=DM+Mono:wght@400;500&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "custom.css"),
    
    tags$style(HTML("
      /* \u2500\u2500 Accordion \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
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

      /* \u2500\u2500 Report tab \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
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

      /* \u2500\u2500 Overview info cards \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
      .ov-card { padding: 14px 16px; font-size: 13px; line-height: 1.65; color: #374151; }
      .ov-card ol, .ov-card ul { padding-left: 18px; margin: 8px 0 0; }
      .ov-card li { margin-bottom: 5px; }
      .ov-card pre { background: #f1f5f9; border-radius: 6px; padding: 10px 12px;
                      font-size: 12px; color: #1a2e35; margin-top: 8px; }
      .ov-card a { color: #18bdb9; }

      /* \u2500\u2500 Header \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
      .pgp-header-text { display: flex; align-items: baseline; gap: 14px; flex-wrap: wrap; }
      .pgp-subtitle { font-style: italic; }

      /* \u2500\u2500 Main tab two-column layout \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
      .main-left  { padding-right: 10px; }
      .main-right { padding-left:  10px; }

      /* \u2500\u2500 Report variable chips \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
      .var-chip {
        display: inline-block; padding: 3px 9px; font-size: 11px; font-weight: 500;
        background: #eef3f8; border: 1px solid #c8d8e8; border-radius: 20px;
        color: #2E74B5; cursor: pointer; font-family: 'DM Mono', monospace;
        transition: background 0.15s;
      }
      .var-chip:hover { background: #dbeafe; border-color: #93c5fd; }

      /* \u2500\u2500 Interpretation textarea \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
      .interp-textarea {
        font-size: 12.5px; line-height: 1.65; resize: vertical;
        border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px 12px;
        color: #374151; background: #fafcff;
      }
      .interp-textarea:focus { border-color: #18bdb9; outline: none;
                                box-shadow: 0 0 0 2px rgba(24,189,185,0.15); }

      /* \u2500\u2500 Export row (NOW TOP of report tab) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
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
  
  # -- Header -------------------------------------------------------------------
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
  
  # -- Tabs ---------------------------------------------------------------------
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
          id      = "acc_howto",
          heading = "How to use",
          open    = TRUE,
          tags$div(
            class = "ov-card",
            tags$ol(
              tags$li("In the ", tags$b("Calculator"), " tab, open ",
                      tags$b("Trial Design Settings"), " to choose design type, \u03b1, power, allocation ratio, and CI method."),
              tags$li("Open ", tags$b("Proportions"), " to enter p\u2080, p\u2081, \u0394, and the sensitivity window."),
              tags$li("Tick ", tags$b("Show CI method comparison table"),
                      " (under Trial Design Settings) to compare sample sizes across methods."),
              tags$li("Use ", tags$b("Simulation Settings"), " to adjust simulation quality when using a CI-based method."),
              tags$li("Toggle tables, the n result box, and the vertical marker in ", tags$b("Other Settings"), "."),
              tags$li("Go to ", tags$b("Generate Report"), " to customise and export a Word or PDF summary.")
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
              tags$li(tags$b("Agresti\u2013Coull:"), " Adds a small 'add 2/2' correction. Close to Wilson."),
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
              style = "margin-top:14px; background:#eef9f9; border:1px solid #18bdb9; border-radius:8px; padding:11px 14px; display:flex; align-items:center; gap:10px;",
              tags$span("\U0001F4D6", style = "font-size:18px; flex-shrink:0;"),
              tags$div(
                tags$p(style = "margin:0 0 2px; font-size:12px; font-weight:700; color:#0f7f7c; letter-spacing:0.02em;",
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
        
        acc_panel(
          id      = "acc_report_info",
          heading = "Generate Report Info",
          open    = FALSE,
          tags$div(
            class = "ov-card",
            tags$p("The Generate Report tab builds a structured summary from your current Calculator inputs."),
            tags$ul(
              tags$li(tags$b("Report Type:"), " Choose Word (.docx) or PDF. PDF requires the ",
                      tags$code("pagedown"), " package."),
              tags$li(tags$b("Title & Header:"), " Set a custom report title and toggle the date and CI method in the header."),
              tags$li(tags$b("Interpretation:"), " Edit the interpretation paragraph and insert live variable values using the tag buttons (e.g. {n}, {power_pct})."),
              tags$li(tags$b("Include in Report:"), " Toggle individual sections \u2014 results table, interpretation, CI method comparison, definitions, calculation code, sensitivity plots, and sensitivity tables."),
              tags$li(tags$b("Report Contents:"), " Live checklist at the top updates as you tick and untick options.")
            )
          )
        ),
        
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
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # Tab 2 - Main Calculator
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Calculator",
      
      fluidRow(
        style = "margin: 18px 8px 0;",
        
        # -- Left column: settings accordions ----------------------------------
        column(
          width = 4,
          class = "main-left",
          
          # 1. Trial Design Settings
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
              selected = 0.025   # <-- default changed to 0.025
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
          
          # 2. Proportions
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
          
          # 3. Simulation Settings
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
          
          # 4. Other Settings
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
            )
          )
        ),
        
        # -- Right column: plots + outputs ------------------------------------
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
    # Tab 3 - Generate Report
    # --------------------------------------------------------------------------
    tabPanel(
      title = "Generate Report",
      
      tags$div(
        class = "report-panel",
        
        # ── TOP: Export row + live contents list ────────────────────────────
        tags$div(
          class = "report-export-row",
          
          # Download controls (left)
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
          
          # Live contents (right)
          tags$div(
            class = "report-export-right",
            tags$p(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                            letter-spacing:0.06em; color:#64748b; margin:0 0 8px;",
                   "Report contents"),
            uiOutput("report_contents_ui")
          )
        ),
        
        # ── BELOW: Configuration accordions ─────────────────────────────────
        
        # Title & Header accordion
        acc_panel(
          id = "acc_rpt_header", heading = "Title & Header", open = FALSE,
          
          textInput(
            "rpt_title", label = "Report title",
            value = "PG-Power \u2014 Sample Size Report",
            placeholder = "Report title..."
          ),
          tags$div(
            style = "display:flex; gap:24px;",
            checkboxInput("rpt_include_date",   "Include generation date", value = TRUE),
            checkboxInput("rpt_include_method", "Include CI method",       value = TRUE)
          )
        ),
        
        # Interpretation accordion
        acc_panel(
          id = "acc_rpt_interp", heading = "Interpretation", open = FALSE,
          
          tags$div(
            style = "margin-top:4px;",
            
            # -- Template picker row -----------------------------------------
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
                  tags$option(value = "safety",     "Safety endpoint \u2014 complication rate")
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
                  "\u21ba Restore default"
                )
              )
            ),
            
            # -- Variable tag chips ------------------------------------------
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
            
            # -- Editable textarea -------------------------------------------
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
        
        # Include in Report accordion
        acc_panel(
          id = "acc_rpt_include", heading = "Include in Report", open = FALSE,
          
          checkboxInput("rpt_results",      "Results table",                value = TRUE),
          checkboxInput("rpt_interp_inc",   "Interpretation paragraph",     value = TRUE),
          checkboxInput("rpt_ci_compare",   "CI method comparison table",   value = FALSE),
          checkboxInput("rpt_definitions",  "Definitions glossary",         value = TRUE),
          checkboxInput("rpt_calc_code",    "Calculation code",             value = TRUE),
          checkboxInput("rpt_plots",        "Sensitivity plots",            value = FALSE),
          checkboxInput("rpt_tables",       "Sensitivity tables",           value = FALSE),
          tags$div(
            style = "opacity:0.55; pointer-events:none;",
            checkboxInput(
              "rpt_ci_bands",
              tags$span("CI bands on plots", tags$span(" \u2014 coming soon", class = "rc-soon")),
              value = FALSE
            )
          )
        )
      )
    )
  ),
  
  # -- Accordion + textarea JS --------------------------------------------------
  tags$script(HTML('

    // Interpretation templates - stored on window so inline onclick can reach them
    window.interpTemplates = {
      "default":    "A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power, that the device success rate exceeds the performance goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. Allowing for 10% dropout, the study should enrol {n_dropout} patients. The study will be deemed successful if at least {n_successes} out of {n} evaluable patients are free from a major adverse event at 12 months.",
      "concise":    "A sample size of {n} patients provides {power_pct}% power (one-sided \u03b1 = {alpha}) to demonstrate non-inferiority of the device against the performance goal of {p0_pct}%, with an NI margin of \u0394 = {delta}, assuming a true device success rate of {p1_pct}%.",
      "two_arm":    "A total of {n} patients are required to demonstrate non-inferiority of the treatment versus the control, with {power_pct}% power and a one-sided significance level of {alpha}. The assumed event rates are {p1_pct}% (treatment) and {p0_pct}% (control), with a non-inferiority margin of {delta} on the risk difference scale. Allowing for 10% dropout, {n_dropout} patients should be enrolled.",
      "regulatory": "The study is designed as a single-arm, non-inferiority study comparing the device success rate to an objective performance criterion (OPC) of {p0_pct}%, consistent with published literature and historical data. A minimum of {n} evaluable subjects is required to demonstrate, with {power_pct}% power at a one-sided significance level of {alpha}, that the lower bound of the {ci_method} confidence interval for the device success rate exceeds the performance goal less the non-inferiority margin ({delta}). Accounting for a 10% dropout rate, the study will enrol {n_dropout} subjects. The primary endpoint will be met if at least {n_successes} of {n} evaluable subjects achieve procedural success.",
      "safety":     "A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power (one-sided \u03b1 = {alpha}), that the device complication rate is non-inferior to the performance goal of {p0_pct}%, assuming a true complication rate of {p1_pct}% and an acceptable margin of {delta}. With an anticipated dropout rate of 10%, {n_dropout} patients will be enrolled. The safety endpoint will be satisfied if no more than the pre-specified number of adverse events are observed among the {n} evaluable patients."
    };

    // Helper: write text into textarea and notify Shiny
    window.pgpSetInterp = function(txt) {
      var ta = document.getElementById("rpt_interp_text");
      if (!ta) return;
      ta.value = txt;
      if (window.Shiny) Shiny.setInputValue("rpt_interp_text", txt, {priority: "event"});
    };

    // Load whichever template is selected in the dropdown
    window.pgpLoadTemplate = function() {
      var sel = document.getElementById("interp_template_select");
      var key = sel ? sel.value : "default";
      var txt = window.interpTemplates[key] || window.interpTemplates["default"];
      window.pgpSetInterp(txt);
    };

    // Always go back to the default template
    window.pgpRestoreDefault = function() {
      window.pgpSetInterp(window.interpTemplates["default"]);
      var sel = document.getElementById("interp_template_select");
      if (sel) sel.value = "default";
    };

    // Accordion toggle
    $(document).on("click", ".pgp-accordion-header", function() {
      var $hdr  = $(this);
      var $body = $hdr.next(".pgp-accordion-body");
      $hdr.toggleClass("open");
      $body.toggleClass("open");
    });

    // Sync textarea to Shiny on page load and on every keystroke
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