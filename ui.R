#ui
ui <- fluidPage(
  theme = default_mode,
  
  titlePanel("NI-Sizer"),
  
  tabsetPanel(
    
    
###SETUP TAB 1
    
    tabPanel(
      title = "Set Up",
      
      sidebarLayout(
        sidebarPanel(
          
          #dark mode toggle
          checkboxInput("dark_mode_on", "Dark mode", value = FALSE),
          
          tags$hr(),
          
          h4("Trial design settings"),
          
          selectInput(
            "sig.level",
            "Significance level (one-sided α)",
            choices = c("0.025" = 0.025, "0.05" = 0.05),
            selected = 0.025
          ),
          
          sliderInput(
            "power",
            "Power",
            min = 0.80,
            max = 0.95,
            step = 0.05,
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
          h3("What NI-Sizer does"),
          p("NI-Sizer is an interactive Shiny app that helps explore minimum sample size requirements for non-inferiority trials."),
          p("It supports binary outcomes (risk difference scale) and continuous outcomes (mean difference scale), and visualises how sensitive the required sample size is to key assumptions."),
          
          h4("Workflow"),
          tags$ol(
            tags$li("Set trial-level assumptions here (α, power, allocation ratio)."),
            tags$li("Use the Proportions tab or Continuous tab to enter outcome-specific assumptions."),
            tags$li("Use the plots to understand how design choices change the required sample size.")
          ),
          
          h4("Note"),
          tags$ul(
            tags$li("Plots are for planning/sensitivity analysis, not a full SAP."),
            tags$li("Always validate final design choices with a statistician.")
          )
        )
      )
    ),
    
###PROPORTIONS TAB 2
    # Plot 1
    
    tabPanel(
      title = "Proportions",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("p0.expected", "ExpectedControl:", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
          sliderInput("p1.expected", "ExpectedExperimental:", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
          sliderInput("p1.tolerable", "NonInferiorityMargin:", min = 0.01, max = 0.20, step = 0.01, value = 0.05),
          
          helpText("Note: alpha, power and allocation ratio can be set in the Set Up tab."),
          
  #TABLE SHOW/HIDE CHECKBOX
          checkboxInput("showTable", "Show Data Table (NonInferiority Margin)", value = FALSE),
          checkboxInput("showTable2", "Show Data Table (Expected Performance)", value = FALSE),
          
  #DOWNLOAD BUTTONS
          downloadButton("downloadData", "Download Data")
        ),
        
        mainPanel(
          plotOutput("plot1"),
          plotOutput("plot2"),
          DTOutput("dataTable"),
          DTOutput("dataTable2")
        )
      )
    ),
    
###CONTINOUS TAB 3
  # Plot 1

    tabPanel(
      title = "Continuous",
      
      sidebarLayout(
        sidebarPanel(
          numericInput("mu0", "Control mean (μ0):", value = 50),
          numericInput("mu1", "Expected experimental mean (μ1):", value = 50),
          
          numericInput("sd", "Common SD:", value = 10, min = 0.0001),
          numericInput("delta", "Non-inferiority margin (Δ):", value = 5, min = 0.0001),
          
          helpText("Note: alpha, power and allocation ratio can be set in the Set Up tab."),
          
          #TABLE SHOW/HIDE CHECKBOX
          checkboxInput("showTable_mean",  "Show Data Table (Δ sensitivity)", value = FALSE),
          checkboxInput("showTable_mean2", "Show Data Table (μ1 sensitivity)", value = FALSE),
          
          #DOWNLOAD BUTTONS
          downloadButton("downloadData_mean", "Download Δ table"),
          downloadButton("downloadData_mean2", "Download μ1 table")
        ),
        
        mainPanel(
          plotOutput("plot_mean"),
          plotOutput("plot_mean2"),
          DTOutput("dataTable_mean"),
          DTOutput("dataTable_mean2")
        )
      )
    )

  )
)


