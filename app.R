## COVID-19 Shiny application to improve my R skills
# This work is base on 
# Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk)
# last updated April 2020 :   https://github.com/eparker12/nCoV_tracker.git
# It's also based on Shiny tutorial : https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/


## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example



# Load Required Packages
library(shiny)
library(leaflet)
library(geojsonio)
library(RColorBrewer)




############## Read files ###############""
cv_cases = read.csv("input_data/coronavirus.csv")
df = read.csv("input_data/COVID_data_2020-05-17.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")


### Data ###

# for the date given
date = "2020-05-17"
# df for a specific date
spec_date_df = df[df$date == date,]
# order country by df order
bigOne <- merge(worldcountry, 
                merge(spec_date_df, countries, by = "country"), 
                by.x = "ADM0_A3", by.y = "alpha3")

worldcountryData <- worldcountry[match(bigOne$ADM0_A3, worldcountry$ADM0_A3),] %>%
  cbind(cases = bigOne$cases,
        newcases = bigOne$new_cases, 
        deaths = bigOne$deaths,
        newdeaths = bigOne$new_cases,
        recovered = bigOne$recovered,
        newrecovered = bigOne$new_recovered)
colnames(worldcountryData@data)[c(72, 73, 74, 75, 76, 77)] <- c("cases",
                                                                "new_cases",
                                                                "deaths",
                                                                "new_deaths",
                                                                "recovered", 
                                                                "new_recovered")




###   MAPPING   ###
palettes <- c("cases" = "Blues", "deaths" = "OrRd", "recovered" = "YlGn")

basemap <- leaflet(worldcountryData) %>%
                addPolygons(
                  fillColor = ~ palDeaths(deaths),
                  fillOpacity = 1,
                  color = "black",
                  weight = 1
                ) %>%
                setView(lng = 0,
                        lat = 30,
                        zoom = 1.4) %>%
                addLegend(
                  position = "bottomright",
                  pal = palDeaths,
                  values = ~ deaths,
                  opacity = 1
                )




#### Shiny Application ###
u.i <- fluidPage(
  
  titlePanel("Covid Mapping"),
  
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  
                  radioButtons(
                    inputId = "category",
                    label =  "Category",
                    choices = list(
                      "Cases" = 1,
                      "New cases" = 2,
                      "Deaths" = 3,
                      "New Deaths" = 4,
                      "Recoverd" = 5,
                      "New Recovered" = 6
                    )
                  ),
                  
                  sliderInput("date",
                              label = "Date",
                              min = as.Date("2020-01-22","%Y-%m-%d"),
                              max = as.Date("2020-05-17", "%Y-%m-%d"),
                              value = as.Date("2020-05-17", "%Y-%m-%d"),
                              timeFormat = "%F")
                ),
                
                mainPanel(h3("Leaflet plot maybe one day ?"),
                          basemap
                          )
                )
  )


ser.ver <- function(input, output){
  
  reactive_db_date = reactive({
    spec_date_df = df[df$date == input$date,]
  })
  
  reactive_db_category = reactive({
    if (input$category == "Cases" | input$category == "New Cases"){
      pal = colorBin(palettes = "Blues", domain = worldcountryData$cases)
    }
    if (input$category == "New Cases"){
      pal = colorBin("Blues", domain = worldcountryData$new_Cases)
    }
    if (input$category == "Deaths" | input$category == "New Cases"){
      pal = colorBin(palettes = "OrRd", domain = worldcountryData$deaths)
    }
    if (input$category == "New Deaths"){
      pal = colorBin("OrRd", domain = worldcountryData$new_deaths)
    }
    if (input$category == "Recovered" | input$category == "New Cases"){
      pal = colorBin(palettes = "YlGn", domain = worldcountryData$recovered)
    }
    if (input$category == "New Recovered"){
      pal = colorBin("YlGn", domain = worldcountryData$new_recovered)
    }
  })
  
  
}




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







