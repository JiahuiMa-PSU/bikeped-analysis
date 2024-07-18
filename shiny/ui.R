library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Bike Volume Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Maintenance tool", tabName = "maintenance", icon = icon("toolbox")),
      menuItem("Volume by Locations", tabName = "map", icon = icon("map")),
      menuItem("Explore Seasonality", tabName = "season", icon = icon("cloud")),
      menuItem("FAQs", tabName = "FAQs", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              img(
                src = "rainbike.jpg",
                width = "75%"
              ),
              
              h2(
                "About:"
              ),
              p(
                "This is a showcase using data from ", 
                a("BikePed Portal", href = "https://bikeped.trec.pdx.edu/", target = "_blank"), 
                " at",
                a("TREC, Portland State University", href = "https://trec.pdx.edu/", target = "_blank"), 
                "."
              ),
              p(
                "Data included in this dashboard are the autonomous counters in MD, VA, and DC."
              ),
              h3("What can you do with this app: "
              ),
              p(  
              HTML("<strong>Maintenance tool </strong> allows user to choose a state and a month, 
                   and shows most used segment area.", "<br>",
                  "<strong>Volume by locations </strong> shows all counter locations in the map 
                    and information for selected location", "<br>",
              HTML("<strong>Explore seasonality </strong> compare seasonal ratios among locations and functional classifications.
                   ")
                )
          
              )
      ),
     
       tabItem(tabName= "maintenance",
               h3(
                 "Use this page to explore the most used locations"
               ), 
               br(),
         fluidRow(
           column(width = 4,
                  selectInput("state", "Choose a State", choices = unique(segment_area$state))
           ),
           column(width = 4,
                  selectInput("flow_type", "Choose a Flow Type", choices = c("Bike only", "Pedestrian only", "Combined"))
           )
         ),
         fluidRow(
               
                column(width = 4,
                     selectInput("month", "Choose a Month", choices = unique(madt$month))
                 ),
                column(width = 4,
                       selectInput("year", "Choose a Year", choices = unique(madt$year))
                )
               ),
         br(),
         uiOutput("dynamicHeader"),
              fluidRow(
                column(width = 12,
                      tableOutput("segmentTable")
                      )
              )
      ),
      
      tabItem(tabName = "map",
              h3(
                "Click on a location"
              ), 
              br(),
              fluidRow(
                column(width = 6,
                       leafletOutput("map", height = "400px")
                ),
                column(width = 6,
                       fluidRow(
                         valueBoxOutput("valueBox1")
                       ),
                       fluidRow(
                         valueBoxOutput("valueBox2")
                       ),
                       fluidRow(
                         valueBoxOutput("valueBox3")
                       )
                       
                )
              ),
              
              fluidRow(
                column(width = 12,
                       plotOutput("volumePlot", height = "300px")
                )
              )
      ),
      
      tabItem(tabName = "season",
              h3(
                "Number of counters:"
              ), 
              br(),
              fluidRow(
                column(width = 12,
                       tableOutput("summaryTable")
                       )
              ),
              br(),
              h3(
                "Seasonal ratios by functional classification*"
              ), 
              fluidRow(
                column(8,
                       plotlyOutput("seasonalRatiosBoxPlot")
                )
              ),
              p(
                HTML("*Seasonal ratio (SR) = highest monthly volume in a year/lowest monthly volume.<br/>SR is calculated only for counters with full 12 monthly data within a year.")
              ),
              br(),
              h3(
                "Higest volume months for different functional classification"
              ), 
              fluidRow(
                
                column(8,
                       plotlyOutput("seasonalRatiosBarPlot")
                )
              )
              
      ),
      tabItem(tabName = "FAQs"
        
      )
    )
  )
)

