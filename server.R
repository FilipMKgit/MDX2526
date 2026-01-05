#server
server <- function(input, output, session) {
  

  ###PROPORTIONS TAB 1

# Plot 1
# Reactive plot output based on the slider value

  PlotReact <- reactive({
    x <-seq(from = input$p1.tolerable - as.numeric(input$WindowMargin), to = input$p1.tolerable +as.numeric(input$WindowMargin), by = 0.005)
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

#Plot 2
PlotReact2 <- reactive({
  x <-seq(from = input$p1.expected, to = input$p0.expected, by = 0.005)
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

output$plot2 <- renderPlot({
  ggplot(PlotReact2(), aes(x, y)) +
    geom_line() + 
    geom_point() +
    labs(title = 'Expected Product/ Competition Preformance vs. Minimum Total Sample Size', x = 'Expected Product/ Competition Preformance', y = 'Minimum Total Sample Size') +
    theme_minimal()
})

#TABLE SHOW/HIDE CHECKBOX
output$dataTable <- renderDT({
  if (input$showTable) {
    DT::datatable(
      PlotReact(),
      data.frame(
        NI_Margin = numeric(0),
        Total_Sample_Size = numeric(0)
      ),
      options = list(
        stripe = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = FALSE,
        colnames = TRUE #I can figure out how to name the columns
      )
    )
  } else {
    NULL
  }
})

# Show/hide data table based on checkbox
output$dataTable2 <- renderDT({
  if (input$showTable2) {
    DT::datatable(
      PlotReact2(),
      options = list(
        dom = "t", #Unsure what this does thought it could handle null values then but clearly not. 
        stripe = TRUE,
        hover = TRUE,
        bordered = TRUE,
        na = "NA",
        rownames = FALSE,
        colnames = TRUE #I cant figure out how to name the columns 
      )
    )
  } else {
    NULL
  }
})

# Download data table as CSV
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("Minimum_Sample_Size_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(PlotReact(), file, row.names = FALSE)
  }
)




  ###CONTINOUS TAB 2

# Plot 1
# Reactive plot Δ vs n

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
      power = input$power, #power wanted
      r = as.numeric(input$r) #allocation ratio (treat/control)
    )
  })
  
  data.frame(delta = delta_seq, total_n = y) #return dataframe where x = margin, y = sample size
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

# Plot 2
# assumed experimental mean (mu1) vs sample size (with Δ fixed)

PlotReact_mean2 <- reactive({
  req(input$mu0, input$mu1, input$sd, input$delta, input$power, input$sig.level, input$r)
  mu1_seq <- seq(from = input$mu1 - 5, to = input$mu1 + 5, by = 1)
  y <- sapply(mu1_seq, function(m1) {
    total_sample_size_mean(
      mu0 = input$mu0,
      mu1 = m1,
      sd = input$sd,
      delta = input$delta,
      sigma = as.numeric(input$sig.level),
      power = input$power,
      r = as.numeric(input$r)
    )
  })
  
  data.frame(mu1 = mu1_seq, total_n = y)
})

output$plot_mean2 <- renderPlot({
  
  df <- PlotReact_mean2()
  
  ggplot(df, aes(x = mu1, y = total_n)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Assumed experimental mean (mu1) vs minimum total sample size",
      x = "Assumed experimental mean (mu1)",
      y = "Minimum total sample size"
    ) +
    theme_minimal()
})

output$dataTable_mean <- renderDT({
  if (input$showTable_mean) {
    DT::datatable(
      PlotReact_mean(),
      options = list(
        stripe = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = FALSE
      )
    )
  } else {
    NULL
  }
})

output$dataTable_mean2 <- renderDT({
  if (input$showTable_mean2) {
    DT::datatable(
      PlotReact_mean2(),
      options = list(
        stripe = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = FALSE
      )
    )
  } else {
    NULL
  }
})
  
}

