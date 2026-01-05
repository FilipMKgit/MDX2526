#server

server <- function(input, output, session) {
  
  ###PROPORTIONS
# Reactive plot output based on the slider value
PlotReact <- reactive({
  x <-seq(from = input$p1.tolerable - 0.02, to = input$p1.tolerable, by = 0.005)
  y <- sapply(x, function(x) {
    total_sample_size_prop(
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


  ###CONTINOUS
PlotReact_mean <- reactive({
  req(input$mu0, input$mu1, input$sd, input$delta, input$power, input$sig.level, input$r)
  delta_seq <- seq(from = input$delta - 0.20, to = input$delta, by = 0.05)
  delta_seq <- delta_seq[delta_seq > 0]
  
  y <- sapply(delta_seq, function(d) {
    total_sample_size_mean(
      mu0 = input$mu0, #control mean
      mu1 = input$mu1, #expected experimental mean
      sd = input$sd, #common sd
      delta = d, #
      sigma = as.numeric(input$sig.level), #one sided alpha 
      power = input$power,
      r = as.numeric(input$r)
    )
  })
  
  data.frame(delta = delta_seq, total_n = y)
})

output$plot_mean <- renderPlot({
  
  df <- PlotReact_mean()
  
  ggplot(df, aes(x = delta, y = total_n)) +
    geom_line() +
    geom_point() +
    labs(
      title = "NI margin (mean difference) vs. minimum total sample size",
      x = "Non-inferiority margin (Δ)",
      y = "Minimum total sample size"
    ) +
    theme_minimal()
})
}
