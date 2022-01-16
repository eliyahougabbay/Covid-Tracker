## COVID-19 Shiny application to improve my R skills
# This work is based on Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk)
# Last update April 2020 : https://github.com/eparker12/nCoV_tracker.git
# Also based on Shiny tutorial : https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/


## Includes code adapted from the following sources:
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
basemap <- leaflet(worldcountryData) %>%
               addTiles() %>%
               setView(lng = 0,
                        lat = 30,
                        zoom = 1.4)



#### Shiny Application ###
u.i <- fluidPage(
  
  titlePanel("Covid Tracker"),
  
  sidebarLayout(position = "left", 
                
                sidebarPanel(
                  
                  h3("Scale"),
                  checkboxInput(inputId = "quantile",
                                label = "Quantile scale",
                                value = FALSE),
                  
                  radioButtons(
                    inputId = "category",
                    label = h3("Category"),
                    choices = list(
                      "Cases" = "cases",
                      "New cases" = "new_cases",
                      "Deaths" = "deaths",
                      "New Deaths" = "new_deaths",
                      "Recoverd" = "recovered",
                      "New Recovered" = "new_recovered"
                    )
                  ),
                  
                  sliderInput("date",
                              label = h3("Date"),
                              min = as.Date("2020-01-22","%Y-%m-%d"),
                              max = as.Date("2020-05-17", "%Y-%m-%d"),
                              value = as.Date("2020-05-17", "%Y-%m-%d"),
                              timeFormat = "%F")
                ),
                
                mainPanel(leafletOutput("map")
                          )
                )
  )


ser.ver <- function(input, output){
  
  dataInput <- reactive({
    spec_date_df <- df[as.Date(df$date) == input$date,]
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
    worldcountryData
  })
  
  colorPal <- reactive({
    if(input$quantile){
      colorQuantile
    } else {
      colorBin
    }
  })
  
  
  output$map <- renderLeaflet({
    if (input$category == "cases"){
      plt = colorPal()(palette = "Blues", domain = unique(dataInput()$cases), n = 6) 
      domn = dataInput()$cases
      legend_title = "Cases"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$cases, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    if (input$category == "new_cases"){
      plt = colorPal()(palette = "Blues", domain = unique(dataInput()$new_cases), n = 6)
      domn = dataInput()$new_cases
      legend_title = "New Cases"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$new_cases, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    if (input$category == "deaths"){
      plt = colorPal()(palette = "OrRd", domain = dataInput()$deaths, n = 6)
      domn = dataInput()$deaths
      legend_title = "Deaths"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$deaths, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    if (input$category == "new_deaths"){
      plt = colorPal()(palette = "OrRd", domain = unique(dataInput()$new_deaths), n = 6) 
      domn = dataInput()$new_deaths
      legend_title = "New Deaths"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$new_deaths, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    if (input$category == "recovered"){
      plt = colorPal()(palette = "YlGn", domain = unique(dataInput()$recovered), n = 6)
      domn = dataInput()$recovered
      legend_title = "Recovered"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$recovered, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    if (input$category == "new_recovered"){
      plt = colorPal()(palette = "YlGn", domain = unique(dataInput()$new_recovered), n = 6)
      domn = dataInput()$new_recovered
      legend_title = "New Recovered"
      label <- sprintf("<strong>%s</strong><br/>%s",
                       dataInput()$SOVEREIGNT,
                       format(dataInput()$new_recovered, big.mark = " ")) %>% lapply(htmltools::HTML)
    }
    
    basemap %>% addPolygons(fillColor = ~ plt(domn),
                            fillOpacity = 1,
                            color = "black",
                            weight = 1,
                            highlight = highlightOptions(weight = 3,
                                                         color = "black"),
                            label = label,
                            ) %>%
                            addLegend(title = legend_title,
                              position = "bottomright",
                              pal = plt,
                              values = ~ domn,
                              opacity = 1
                            )
  })
  
}



shinyApp(ui = u.i, server = ser.ver)




