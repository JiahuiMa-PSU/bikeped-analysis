shinyUI(navbarPage(
  title = "Bicycle volume dashboard",
  
  tabPanel(
    title = "Home",
    img(
      src = "rainbike.jpg",
      width = "75%"
    ),
    h1("About This App"),
    p("Placeholder: 
Use the upper plot to see the monthly volume change through the years for your chosen state and functional classification.

Use the lower plot to compare the three states for your chosen year and functional classification.")
  ),
  
  tabPanel(
    title = "Dashboard",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "year",
          label = "Select a year:",
          choices = unique(monthly_mean_function$year)
        ),
        
        selectInput(
          inputId = "funcClassInput",
          label = "Select a Functional Classification: ",
          choices = unique(monthly_mean_function$functional_classification)
        ),
        
        selectInput(
          inputId = "state",
          label = "Select a state: ",
          choices = unique(monthly_mean_function$state)
        )
      ),
      
      mainPanel(
        plotlyOutput("statesPlot")
      )
    )
  )
  
  
))
