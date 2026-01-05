#ui
ui <- fluidPage(
  titlePanel("Minimum Sample Size Required"),
  tabsetPanel(
    
  #tab1
  ###PROPORTIONS


    
    
    tabPanel(
      title = "Proportions",
      
      sidebarLayout(
        sidebarPanel(
          
          sliderInput("p0.expected", "ExpectedControl:", min = .01, max = .99, step =0.01, value = .05),
          sliderInput("p1.expected", "ExpectedExperimental:", min = .01, max = .99, step =0.01, value = .05),
          sliderInput("p1.tolerable", "NonInferiorityMargin:", min = .01, max = 0.2, step =0.01, value = .05),
          sliderInput("power", "Power:", min = 0.8, max = 0.95, step =0.05, value = .05),
          selectInput("WindowMargin", "WindowMargin:", choices = list("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05), selected = .05),
          selectInput("r","ratio", choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4,"Choice 5" = 5), selected = 1),
          selectInput("sig.level","alpha",choices = list("0.025" = 0.025, "0.05" = 0.05), selected = 0.025), 
          checkboxInput("showTable", "Show Data Table (NonInferiority Margin)", value = FALSE),
          checkboxInput("showTable2", "Show Data Table (Expected Preformance)", value = FALSE),
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

  #tab2
  ###CONTINOUS
  tabPanel(
    title = "Continuous",
           sidebarLayout(
             sidebarPanel(
               numericInput("mu0", "Control mean (mu0):", value = 50),
               numericInput("mu1", "Expected experimental mean (mu1):", value = 50),
               
               numericInput("sd", "Common SD:", value = 10, min = 0.0001),
               
               numericInput("delta", "Non-inferiority margin (Δ):", value = 5, min = 0.0001),
               
               helpText("Note: alpha, power and allocation ratio are shared from the Proportions tab.")
             ),
             mainPanel(
               plotOutput("plot_mean")
             )
             )
           )
  )
)
