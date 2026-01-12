#ui
ui <- fluidPage(
  theme = default_mode,
  
  tags$div(
    style = "
    display: flex;
    align-items: center;
    gap: 14px;
    margin-bottom: 6px;
  ",
    
    tags$img(
      src = "nisizer_logo.png",
      height = "100px"
    ),
  
  tags$h1(
    "NI-Sizer",
    style = "
    color: #18bdb9;
    font-size: 38px;
    font-weight: 600;
    letter-spacing: 1px;
    margin-bottom: 10px;
  "
  ),
  tags$h4(
    "Non-inferiority sample size exploration tool",
    style = "
    color: #666666;
    font-weight: 300;
    margin-top: 0;
  "
  )),
  
  tabsetPanel(
    
    
###SETUP TAB 1
    
    tabPanel(
      title = "Set up",
      
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
            selected = 0.02
          )
        ),
        
        mainPanel(
          h3("Overview"),
          p("NI-Sizer explores sample size requirements for non-inferiority trials using sensitivity plots."),
          p("This tab sets trial-level choices (one-sided α, power, allocation ratio) that apply across both the Proportions and Continuous tabs."),
          
          h4("How to use"),
          tags$ol(
            tags$li("Choose α, power, and allocation ratio on the left."),
            tags$li("Pick a sensitivity window (±) to control how wide the Δ curve is around your chosen margin."),
            tags$li("Go to Proportions (binary) or Continuous (means) to enter outcome assumptions and view plots/tables.")
          ),
          
          h4("Interpretation"),
          tags$ul(
            tags$li("Δ plots show how tightening/loosening the NI margin changes the required total sample size."),
            tags$li("p₁ / μ₁ plots show how sensitive results are to assumed experimental performance.")
          ),
          
          h4("Equations"),
          
          actionButton("eq_prop1", "Proportions: Δ plot", class = "btn-sm btn-outline-primary", style="width:220px;"),
          actionButton("eq_prop2", "Proportions: p₁ plot", class = "btn-sm btn-outline-primary", style="width:220px;"),
          tags$br(), tags$br(),
          actionButton("eq_mean1", "Continuous: Δ plot", class = "btn-sm btn-outline-primary", style="width:220px;"),
          actionButton("eq_mean2", "Continuous: μ₁ plot", class = "btn-sm btn-outline-primary", style="width:220px;"),
          
          tags$hr(),
          
          h4("Note"),
          tags$ul(
            tags$li("Outputs are for planning and sensitivity exploration."),
            tags$li("Final design choices should be justified clinically and statistically.")
          ),
          
          tags$hr(),
          
          tags$p(
            style = "font-size: 13px; color: #777777; line-height: 1.4;",
            HTML(
              "Developed by <strong>Filip Kłosowski</strong> and <strong>Áine Glynn</strong><br/>
     University of Galway"
            )
          ),
          
          tags$p(
            style = "font-size: 13px;",
            tags$a(
              href = "https://github.com/FilipMKgit/NI-Sizer",
              target = "_blank",
              "View the NI-Sizer GitHub repository (open source)"
            )
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
          
          sliderInput("p0.expected", "Control event rate (p0):", min = 0.01, max = 0.99, step = 0.01, value = 0.10),
          sliderInput("p1.expected", "Expected experimental event rate (p1):", min = 0.01, max = 0.99, step = 0.01, value = 0.10),
          sliderInput("p1.tolerable", "Non-inferiority margin (Δ, risk difference):", min = 0.01, max = 0.20, step = 0.01, value = 0.05),
          
          helpText("Note: Alpha, power and allocation ratio can be changed in the set up tab."),
          
          #options tick
          tags$div(
          checkboxInput("showExtras_prop", "More Options", value = FALSE),
          style = "margin-top: 15px;"
          ),
          
          conditionalPanel(
            condition = "input.showExtras_prop == true",
            
            tags$hr(),
  #TABLE SHOW/HIDE CHECKBOX
          checkboxInput("showTable", "Show Δ table", value = FALSE),
          checkboxInput("showTable2", "Show p₁ table", value = FALSE),
          
  #DOWNLOAD BUTTONS
  downloadButton("downloadPlot_prop1", "Download Δ plot",  class = "btn-sm btn-outline-primary", style = "width: 220px;"),
  downloadButton("downloadPlot_prop2", "Download p₁ plot", class = "btn-sm btn-outline-primary", style = "width: 220px;"),
  downloadButton("downloadData_plot1", "Download Δ table", class = "btn-sm btn-outline-primary", style = "width: 220px;"),
  downloadButton("downloadData_plot2", "Download p₁ table", class = "btn-sm btn-outline-primary", style = "width: 220px;")
        )),
        
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
          
          numericInput("sd", "Common SD:", value = 10, min = 1, step = 1),
          numericInput("delta", "Non-inferiority margin (Δ):", value = 5, min = 0, step = 1),
          
          helpText("Note: Alpha, power and allocation ratio can be changed in the set up tab."),
          
          tags$br(),
          
          #options tick
          tags$div(
          checkboxInput("showExtras_mean", "More Options", value = FALSE),
          style = "margin-top: 15px;"),
          
          
          conditionalPanel(
            condition = "input.showExtras_mean == true",
            
            #TABLE SHOW/HIDE CHECKBOX
          checkboxInput("showTable_mean",  "Show Δ sensitivity table", value = FALSE),
          checkboxInput("showTable_mean2", "Show μ1 sensitivity table", value = FALSE),
          
          #DOWNLOAD BUTTONS
          downloadButton("downloadPlot_mean1", "Download Δ plot",  class = "btn-sm btn-outline-primary", style = "width: 220px;"),
          downloadButton("downloadPlot_mean2", "Download μ₁ plot", class = "btn-sm btn-outline-primary", style = "width: 220px;"),
          downloadButton("downloadData_mean1", "Download Δ sensitivity table", class = "btn-sm btn-outline-primary",  style = "width: 220px;"),
          downloadButton("downloadData_mean2", "Download μ₁ table", class = "btn-sm btn-outline-primary",  style = "width: 220px;")
        )),
        
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


