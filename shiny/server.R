library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(lubridate)

server <- function(input, output, session) {
  # Define a color palette for states
  state_colors <- colorFactor(palette = "Set1", domain = segment_area$state)
  
  # Reactive value to store the clicked segment area ID
  selected_segment <- reactiveVal()
  
  segment_area$popupContent <- paste0(
    "<strong>Segment area ID: </strong>", segment_area$segment_area_id, "<br>",
    "<strong>Segment name: </strong>", segment_area$segment_name, "<br>",
    "<strong>City: </strong>", segment_area$city, "<br>",
    "<strong>Functional classification: </strong>", segment_area$functional_classification, "<br>",
    "<strong>Facility type: </strong>", segment_area$facility_types, "<br>",
    "<strong>Description: </strong>", segment_area$descriptions, "<br>",
    "<strong>Latitude: </strong>", segment_area$lat, "<br>",
    "<strong>Longitude: </strong>", segment_area$long
  )
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data = segment_area) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat,
        layerId = ~segment_area_id,
        radius = 5,
        color = ~state_colors(state),
        popup = ~popupContent
      )
  })
  
  # Update the selected segment when a map marker is clicked
  observeEvent(input$map_marker_click, {
    selected_segment(input$map_marker_click$id)
  })
  
  
  # Render the plot with the monthly volume
  output$volumePlot <- renderPlot({
    req(selected_segment())
    
    segment_data <- madt %>%
      filter(segment_area_id == selected_segment())
    
    ggplot(segment_data, aes(x = start_time, y = volume, color = flow_type)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      labs(
        title = "Monthly Volume",
        x = "Month",
        y = "Volume"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Render the value boxes
  output$valueBox1 <- renderValueBox({
    req(selected_segment())
    
    segment_data <- madt %>%
      filter(segment_area_id == selected_segment())
    
    valueBox(
      value = nrow(segment_data),
      subtitle = "Number of Records",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$valueBox2 <- renderValueBox({
    req(selected_segment())
    
    segment_data <- madt %>%
      filter(segment_area_id == selected_segment())
    
    avg_volume <- mean(segment_data$volume)
    
    valueBox(
      value = round(avg_volume, 0),
      subtitle = "Average Monthly Volume",
      icon = icon("bar-chart"),
      color = "green"
    )
  })
  
  output$valueBox3 <- renderValueBox({
    req(selected_segment())
    
    segment_data <- seasonal_ratios %>%
      filter(segment_area_id == selected_segment()) %>% 
      filter(year == 2023)
    
    valueBox(
      
      value = segment_data$high_vol_month,
      subtitle = "2023 Highest Bike Volume Month",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  
  
  top_vol <- reactive({
    req(input$state, input$month, input$year, input$flow_type)
    
    data_filtered <- madt %>%
      select(segment_area_id, segment_name, volume, month, year, flow_type, state)%>%
      filter(state == input$state, month == input$month, year == input$year)
    
    if (input$flow_type == "Bike only") {
      data_filtered <- data_filtered %>% filter(flow_type == "bike")
    } else if (input$flow_type == "Pedestrian only") {
      data_filtered <- data_filtered %>% filter(flow_type == "ped")
    } else if (input$flow_type == "Combined") {
      data_filtered <- data_filtered %>%
        group_by(segment_area_id, segment_name, month, year) %>%
        summarise(volume = sum(volume, na.rm = TRUE), .groups = 'drop')
                  
    }
    
    data_filtered %>%
      arrange(desc(volume)) %>%
      slice_head(n = 5) %>%
      mutate(
        "Year" = as.integer(year),
        "Month" = month,
        "Segment Area ID" = as.integer(segment_area_id),
        "Segment Name" = segment_name,
        "Volume" = as.integer(volume)
      ) %>% 
      select(`Year`,`Month`, `Segment Area ID`,`Segment Name` ,`Volume`)
    
      
  })
  
  output$dynamicHeader <- renderUI({
    h3(paste("Top five locations in", input$state, "in", input$month, input$year, "based on", input$flow_type), "flows")
  })
  
  output$segmentTable <- renderTable({
    top_vol()
  })
  

  
 
   output$summaryTable <- renderTable({
    num_distinct_segment_state
  })
   
   output$seasonalRatiosBoxPlot <- renderPlotly({
     fig <- plot_ly(
       data = seasonal_ratios,
       x = ~functional_classification,
       y = ~seasonal_ratio,
       type = 'box',
       boxpoints = FALSE, # Equivalent to outlier.shape = NA
       color = ~functional_classification
     ) %>%
       layout(
         
         xaxis = list(title = 'Functional Classification'),
         yaxis = list(title = 'Seasonal Ratio', range = c(0, 10))
       )
     fig
     # ggplot(seasonal_ratios, aes(x = functional_classification, y = seasonal_ratio, fill = functional_classification)) +
     #   geom_boxplot(outlier.shape = NA) +
     #   scale_y_continuous(limits = c(0, 10)) +
     #   labs(title = "Seasonal Ratios by Functional Classification",
     #        x = "Functional Classification",
     #        y = "Seasonal Ratio") +
     #   theme_minimal() +
     #   theme(
     #     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase size of x-axis text
     #     axis.text.y = element_text(size = 14),  # Increase size of y-axis text
     #     axis.title.x = element_text(size = 16),  # Increase size of x-axis title
     #     axis.title.y = element_text(size = 16),  # Increase size of y-axis title
     #     plot.title = element_text(size = 20, face = "bold"),  # Increase size and make plot title bold
     #     legend.text = element_text(size = 14),  # Increase size of legend text
     #     legend.title = element_text(size = 16)  # Increase size of legend title
     #   )
       
   })
   
   
   seasonal_ratios<- seasonal_ratios %>% 
     ungroup()
   highest_volume_months <- seasonal_ratios %>%
     select(functional_classification, high_vol_month) %>% 
     count(functional_classification, high_vol_month) %>%
     group_by(functional_classification) %>%
     mutate(percentage = n / sum(n) * 100) %>%
     ungroup() %>%
     mutate(high_vol_month = factor(high_vol_month, levels = month.abb))
   
   
   
   output$seasonalRatiosBarPlot <- renderPlotly({
     fig <- plot_ly(
       highest_volume_months,
       x = ~percentage,
       y = ~functional_classification,
       color = ~high_vol_month,
       type = 'bar',
       orientation = 'h'
     ) %>%
       layout(
         title = 'Percentage of Highest Volume Months by Functional Classification',
         xaxis = list(title = 'Percentage', tickfont = list(size = 12), hoverformat = '.2f'),
         yaxis = list(title = 'Functional Classification', tickfont = list(size = 12)),
         legend = list(title = list(text = 'Month', font = list(size = 14)), font = list(size = 12)),
         margin = list(l = 100, r = 20, t = 50, b = 50),
         barmode = 'stack'
       )
     fig
     
    
   })
}

