#ui
ui <- fluidPage(
  
  titlePanel("NI-Sizer"),
  
  tabsetPanel(
    
###PROPORTIONS TAB 1
    # Plot 1
    
    tabPanel(
      title = "Proportions",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("p0.expected", "ExpectedControl:", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
          sliderInput("p1.expected", "ExpectedExperimental:", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
          sliderInput("p1.tolerable", "NonInferiorityMargin:", min = 0.01, max = 0.20, step = 0.01, value = 0.05),
          
          sliderInput("power", "Power:", min = 0.80, max = 0.95, step = 0.05, value = 0.90),
          
          selectInput("WindowMargin", "WindowMargin:",
                      choices = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05),
                      selected = 0.05),
          
          selectInput("r", "ratio",
                      choices = c("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4, "Choice 5" = 5),
                      selected = 1),
          
          selectInput("sig.level", "alpha",
                      choices = c("0.025" = 0.025, "0.05" = 0.05),
                      selected = 0.025),
          
          
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
    
###CONTINOUS TAB 2
  # Plot 1

    tabPanel(
      title = "Continuous",
      
      sidebarLayout(
        sidebarPanel(
          numericInput("mu0", "Control mean (μ0):", value = 50),
          numericInput("mu1", "Expected experimental mean (μ1):", value = 50),
          
          numericInput("sd", "Common SD:", value = 10, min = 0.0001),
          numericInput("delta", "Non-inferiority margin (Δ):", value = 5, min = 0.0001),
          
          helpText("Note: alpha, power and allocation ratio are shared from the Proportions tab."),
          
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
    ),
    
###INFO TAB 3

    tabPanel(
      title = "Info",
      
      #settings
      
      sidebarLayout(
        sidebarPanel(
          h4("Settings"),
          
          radioButtons(
            inputId = "app_theme",
            label = "Theme",
            choices = c("Default" = "default_mode", "Dark" = "dark_mode"),
            selected = "default_mode"
          ),
          
          #text
          
          tags$hr(),
          
          h4("About the app"),
          p("Interactive sample size exploration for non-inferiority trials.")
        ),
        
        mainPanel(
          h3("Purpose"),
          p("Explore how NI margins, expected performance, alpha, power, and allocation ratio affect required sample size."),
          
          h3("How to use"),
          tags$ul(
            tags$li("Proportions tab: binary outcomes (risk difference NI)."),
            tags$li("Continuous tab: mean difference NI."),
            tags$li("Use sensitivity plots to see how assumptions impact sample size.")
          ),
          
          h3("Notes"),
          tags$ul(
            tags$li("Plots are for planning/sensitivity analysis, not a full SAP."),
            tags$li("Always validate final design choices with a statistician/supervisor.")
          )
        )
      )
    )
    
  )
)   


