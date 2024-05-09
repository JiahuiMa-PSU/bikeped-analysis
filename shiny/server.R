shinyServer(function(input, output) {
  
  output$statesPlot <- renderPlotly({
    
    # Reactive expression to filter dataset based on user input
    filtered_data <- reactive({
      monthly_mean_function %>%
        filter(year == input$year, functional_classification == input$funcClassInput)
    })
    
    # Create the Plotly object
    p2 <- plot_ly(data = filtered_data(), x = ~month, y = ~avg_daily_vol, color = ~state, type = 'scatter', mode = 'lines+markers',
                  line = list(width = 2), marker = list(size = 10)) %>%
      layout(title = paste("Monthly Change in Average Volume for", input$year, "&", input$funcClassInput),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Average Volume"))
    legend = list(title = list(text='State'))
    
    p2 # Return the plot
  })
})
