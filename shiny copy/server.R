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
  
  output$summaryPlot <- renderPlotly({
    ggplot(functional_classification_summary, aes(state, num_segment_area_id, fill = functional_classification)) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() + # Horizontal bar chart
      labs(title = "Number of Segment Areas by Functional Classification and State",
           x = "State",
           y = "Number of Segment Area IDs",
           fill = "Functional Classification") 
  })
  
  output$summaryTable <- renderTable({
    summary_df
  })
  
  popupContent <- paste("Segment area ID: ", madt$segment_area_id, "<br/>",
                        "Segment name: ", madt$segment_name, "<br/>",
                        "City: ", madt$city, ", ", madt$state, "<br/>",
                        "Functional classification: ", madt$functional_classification, "<br/>",
                        "Facility type: ", madt$facility_type, "<br/>",
                        "Description:", madt$descriptions)
                        
     
  output$mymap <- renderLeaflet({
    leaflet(data = madt) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)%>%
      addCircleMarkers(lng = ~long, 
                       lat = ~lat, 
                       radius = 5, 
                       color = "state",
                       popup = popupContent)
      
  })
  
  
   
})
