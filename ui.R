library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("WeatheR"),
  
  sidebarPanel(
    
    selectInput("searchTerm", "Choose a search term:", 
                choices = c("leaves","hot chocolate","cold","chilly","snuggie","Snuggie","wind","windy","rain","rainy day","scarf","scarves","earmuffs","pumpkin","sweater","sweater weather","Uggs", "uggs","snow","snowman","snowflake","autumn","winter","gloves","storm","warm", "warm weather","sunny","cozy","electric blanket","Thanksgiving","holiday","icy","blizzard")),
    
    br(),
    selectInput("city", "Choose a city:", 
                choices = c("NYC", "LA", "Seattle","Chicago","Minneapolis","Denver","San Antonio","New Orleans","Atlanta")),
    
    
    br(),
    # conditionalPanel(
    #   condition="input.tabs1=='Map'",
    sliderInput("day", "Date Range:",min=1,max=31,value=1,animate=animationOptions(interval=1500, loop=T))
    #  )
  ),
  
  mainPanel(tabsetPanel(
    #tabsetPanel(id ="tabs1",
    
    tabPanel("Map",plotOutput("mapMotion") ), 
    tabPanel("Time Series-Frequency",plotOutput("freqPlot")), 
    tabPanel("Time Series-Frequency vs. Temperature",plotOutput("freqPlotTemp") ),
    tabPanel("Random Tweets",htmlOutput("gvis"))
    # )
  )
  )
))
