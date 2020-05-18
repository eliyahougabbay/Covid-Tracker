## COVID-19 Shiny application to improve my R skills
# This work is base on 
# Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk)
# last updated April 2020 :   https://github.com/eparker12/nCoV_tracker.git
# It's also based on Shiny tutorial : https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/


## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example



# load required packages
library(shiny)
library(leaflet)
library(geojsonio)

############## Read CSV ###############""
df = read.csv("COVID_data_2020-05-17.csv")
worldcountry = geojson_read("countries.geojson", what = "sp")



###   MAPPING   ###




u.i <- fluidPage(titlePanel("Covid Mapping"),
                 
                 sidebarLayout(position = "left",
                   
                   radioButtons(inputId = "state", label =  "Choose a highlighted Map",
                                choices = c("Cases", 
                                            "New cases",
                                            "Deaths", 
                                            "New Deaths",
                                            "Recoverd",
                                            "New Recovered")),

                 mainPanel(
                   h3("Leaflet plot is coming soon.."))
                 )
          )


ser.ver <- function(input, output){}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = u.i, server = ser.ver)
