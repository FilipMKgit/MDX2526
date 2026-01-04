library(shiny)
library(dani)
library(ggplot2)
library(dplyr)
#Here I have done the calculation and set actual values however all WILL be sliders
ExpectedControl <-0.05 # Expected control event rate 
#Slider: 0.01 → 0.99, Step: 0.01
ExpectedExperimental <- 0.05# expected experimental performance 
#Slider: 0.01 → 0.99 Step: 0.01
NonInferiorityMargin <-0.1 # Maximum tolerable active event rate 
#Slider: 0.01 → 0.20, Step: 0.01 *Large margins produce artificially small sample sizes
ratio <-1 # Allocation ratio
#Dropdown: 1–5, Step: 1
power <-0.9 # Power 
#Slider: 0.7 → 0.95, Step: 0.05 *Possibly dropdown of common values
alpha <-0.025 # Significance level
#Dropdown: 0.025 (default), 0.05 (other)

sample.size.RD <- sample.size.NI(
  p0.expected = ExpectedControl,
  p1.expected = ExpectedExperimental,
  p1.tolerable = NonInferiorityMargin,
  sig.level = alpha,
  power = power,
  r = ratio,
  scale = "RD"
)



# Define UI
ui <- fluidPage(
  titlePanel("Minimum Sample Size Required"),
  sidebarLayout(
    sidebarPanel( 
      sliderInput("p0.expected", "ExpectedControl:", min = .01, max = .99, step =0.01, value = .05),
      sliderInput("p1.expected", "ExpectedExperimental:", min = .01, max = .99, step =0.01, value = .05),
      sliderInput("p1.tolerable", "NonInferiorityMargin:", min = .01, max = 0.2, step =0.01, value = .05),
      sliderInput("power", "Power:", min = 0.8, max = 0.95, step =0.05, value = .05),
      selectInput("r","ratio",choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3, "Choice 4" = 4,"Choice 5" = 5), selected = 1),
      selectInput("sig.level","alpha",choices = list("0.025" = 0.025, "0.05" = 0.05), selected = 0.025)
      ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  total_sample_size <- function(p0, p1, p1tol, sig.level, power, r) {
    n <- sample.size.NI(
      p0.expected  = p0,
      p1.expected  = p1,
      p1.tolerable = p1tol,
      sig.level    = sig.level,
      power        = power,
      r            = r,
      scale        = "RD"
    )
    sum(n)  
  }
    
  # Reactive plot output based on the slider value
  PlotReact <- reactive({
    x <-seq(from = input$p1.tolerable - 0.02, to = input$p1.tolerable, by = 0.005)
    y <- sapply(x, function(x) {
      total_sample_size(
        p0 = input$p0.expected,
        p1 = input$p1.expected,
        p1tol = x,
        sig.level = as.numeric(input$sig.level),
        power = input$power,
        r = as.numeric(input$r)
      )
    })
    data.frame(x = x, y = y)
  })

#How this works: When the formula is calculation it find the target, control, and total sample size. 
#The first calculation boils this down to just the total sample size (one value).
#The x axis is creating a sequence considering a sensitivity window of -.02NI (Going to also make this a slider so can choose value 0.01/0.02/0.05)
#Then using the value from every point in the sequence the corresponding sample size is computed
#(Will we do many graphs or just one and instead of NI use how well you think your product would do)?
#Next points-different tabs for safety/ effectivness? Have an opening into part (Like Clodaghs?). 
  output$plot1 <- renderPlot({
    ggplot(PlotReact(), aes(x, y)) +
      geom_line() + 
      geom_point() +
      labs(title = 'Non-Inferiority Margin vs. Minimum Total Sample Size', x = 'Non-Inferiority Margin', y = 'Minimum Total Sample Size') +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)