#server
server <- function(input, output, session) {
  
  ###SET UP TAB 1
  #observe dark mode logic

  observeEvent(input$dark_mode_on, {
    if (isTRUE(input$dark_mode_on)) { #if user ticks dark mode
      session$setCurrentTheme(dark_mode) #switch theme to dark mode
    } else {
      session$setCurrentTheme(default_mode) #else switch to default
    }
  }, ignoreInit = TRUE)
  
  

  ###PROPORTIONS TAB 2

# Plot 1
# Reactive plot output based on the slider value

  PlotReact <- reactive({
    
    #window around margin
    window <- as.numeric(input$WindowMargin)
    
    #this clamps the x-range so it never becomes negative or above 1
    #this fixes the crash “p1.tolerable >= 0 is not TRUE”
    x_min <- max(0.001, input$p1.tolerable - window) #lower bound
    x_max <- min(0.99, input$p1.tolerable + window) #upper bound
    
    x <-seq(from = x_min, to = x_max, by = 0.005)
    
    y <- sapply(x, function(xi) { #xi is each candidate margin
      total_sample_size_prop(
        p0 = input$p0.expected, #control event rate
        p1 = input$p1.expected, #expected experimental event rate
        p1tol = xi, #vary NI margin over x
        sig.level = as.numeric(input$sig.level), #alpha from setup
        power = input$power, #power from set up
        r = as.numeric(input$r) #allocation ratio from setup
      )
    })
    data.frame(x = x, y = y) #data for ggplot
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
    labs(title = 'Δ vs total sample size', x = 'Non-inferiority margin (Δ)', y = 'Total sample size') +
    plot_theme_large
})

#Plot 2
PlotReact2 <- reactive({
  
  #i changed this so instead of sequencing from p1.expected to p0.expected (which could be empty)
  #you now build a symmetric window around p1.expected
  x_min <- max(0.001, input$p1.expected - 0.10)
  x_max <- min(0.999, input$p1.expected + 0.10)
  
  x <-seq(from = x_min, to = x_max, by = 0.005)
  
  y <- sapply(x, function(pli) { #pli = each candidate p1 value
    total_sample_size_prop(
      p0 = input$p0.expected, #control rate stays fixed
      p1 = pli, #vary expected experimental rate across x
      p1tol = input$p1.tolerable, #margin stays fixed
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
    labs(title = 'p₁ vs total sample size', x = 'Expected experimental event rate (p₁)', y = 'Total sample size') +
    plot_theme_large
})

#TABLE SHOW/HIDE CHECKBOX
#Table 1
output$dataTable <- renderDT({
  if (input$showTable) {
    
    #now we store plotreact() in a df so that wee can rename the columns
    df <- PlotReact() #grabs data used in plot1
    colnames(df) <- c("NI Margin", "Total Sample Size") #renames columns
    
    DT::datatable(df,
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

#Table 2
output$dataTable2 <- renderDT({
  if (input$showTable2) {
    
    df <- PlotReact2() #get data used in plot2
    colnames(df) <- c(
      "Expected Experimental Event Rate",
      "Total Sample Size (N)"
    )
    
    DT::datatable(
      df,
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
###DOWNLOAD PLOTS

#1
output$downloadPlot_prop1 <- downloadHandler(
  filename = function() paste0("prop_delta_plot_", Sys.Date(), ".png"),
  content  = function(file) {
    p <- ggplot(PlotReact(), aes(x, y)) +
      geom_line() + geom_point() +
      labs(title = "Δ vs total sample size",
           x = "Non-inferiority margin (Δ)",
           y = "Total sample size") +
      plot_theme_large
    
    ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
  }
)

#2
output$downloadPlot_prop2 <- downloadHandler(
  filename = function() paste0("prop_p1_plot_", Sys.Date(), ".png"),
  content  = function(file) {
    p <- ggplot(PlotReact2(), aes(x, y)) +
      geom_line() + geom_point() +
      labs(title = "p₁ vs total sample size",
           x = "Expected experimental event rate (p₁)",
           y = "Total sample size") +
      plot_theme_large
    
    ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
  }
)


###DOWNLOAD TABLES CSV
#Table 1
output$downloadData_plot1 <- downloadHandler(
  filename = function() {
    paste0("NI_margin_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    
    df <- PlotReact()
    colnames(df) <- c(
      "NI Margin",
      "Total Sample Size (N)"
    )
    
    write.csv(df, file, row.names = FALSE)
  }
)

#Table 2
output$downloadData_plot2 <- downloadHandler(
  filename = function() {
    paste0("expected_performance_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    
    df <- PlotReact2()
    colnames(df) <- c(
      "Expected Experimental Event Rate",
      "Total Sample Size (N)"
    )
    
    write.csv(df, file, row.names = FALSE)
  }
)

  ###CONTINOUS TAB 3

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
      title = "Δ vs total sample size",
      x = "Non-inferiority margin (Δ)",
      y = "Total sample size (N)"
    ) + plot_theme_large
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
      title = "μ₁ vs total sample size",
      x = "Assumed experimental mean (μ₁)",
      y = "Total sample size (N)"
    ) + plot_theme_large
})


#TABLES FOR CONTINOUS
#Table 1

output$dataTable_mean <- renderDT({
  if (input$showTable_mean) {
    
    df <- PlotReact_mean()
    colnames(df) <- c(
      "Non-Inferiority Margin (Δ)",
      "Total Sample Size (N)"
    )
    
    DT::datatable(
      df,
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

#Table 2
output$dataTable_mean2 <- renderDT({
  if (input$showTable_mean2) {
    
    df <- PlotReact_mean2()
    colnames(df) <- c(
      "Assumed Experimental Mean (μ1)",
      "Total Sample Size (N)"
    )
    
    DT::datatable(
      df,
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

###DOWNLOAD PLOTS

#1
output$downloadPlot_mean1 <- downloadHandler(
  filename = function() paste0("mean_delta_plot_", Sys.Date(), ".png"),
  content  = function(file) {
    df <- PlotReact_mean()
    
    p <- ggplot(df, aes(x = delta, y = total_n)) +
      geom_line() + geom_point() +
      labs(title = "Δ vs total sample size",
           x = "Non-inferiority margin (Δ)",
           y = "Total sample size (N)") +
      plot_theme_large
    
    ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
  }
)


#2
output$downloadPlot_mean2 <- downloadHandler(
  filename = function() paste0("mean_mu1_plot_", Sys.Date(), ".png"),
  content  = function(file) {
    df <- PlotReact_mean2()
    
    p <- ggplot(df, aes(x = mu1, y = total_n)) +
      geom_line() + geom_point() +
      labs(title = "μ₁ vs total sample size",
           x = "Assumed experimental mean (μ₁)",
           y = "Total sample size (N)") +
      plot_theme_large
    
    ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
  }
)



###DOWNLOAD TABLES CSV
#Table 1

output$downloadData_mean1 <- downloadHandler(
  filename = function() {
    paste0("mean_NI_margin_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    
    df <- PlotReact_mean()
    colnames(df) <- c(
      "Non-Inferiority Margin (Δ)",
      "Total Sample Size (N)"
    )
    
    write.csv(df, file, row.names = FALSE)
  }
)

#Table 2
output$downloadData_mean2 <- downloadHandler(
  filename = function() {
    paste0("assumed_mean_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    
    df <- PlotReact_mean2()
    colnames(df) <- c(
      "Assumed Experimental Mean (μ1)",
      "Total Sample Size (N)"
    )
    
    write.csv(df, file, row.names = FALSE)
  }
)
}




