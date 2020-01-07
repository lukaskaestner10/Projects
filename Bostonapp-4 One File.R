#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shinyWidgets')
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(png)
library(dplyr)
library(lubridate)
library(maps)
library(mapproj)
library(plotly)


boston.bb <- c( left = -71.11, bottom = 42.32, right = -70.98, top = 42.39)
map <- get_stamenmap(boston.bb, zoom = 14, maptype = "terrain", color = "color")
ggmap(map)

bostonCrime = read.csv('bostonCrime.csv', header = TRUE)

boston.bb <- c( left = -71.11, bottom = 42.32, right = -70.98, top = 42.39)
map <- get_stamenmap(boston.bb, zoom = 14, maptype = "terrain", color = "color")

Location <- c("Freedom Trail", "Faneuil Hall Marketplace", "Fenway Park",
              "Boston Commons", "Louisber Square", "Museum of Fine Arts",
              "Museum of Science", "Old North Church", "Boston Public Library",
              "Copley Square")

Longitude <- as.numeric(c("-71.06", "-71.05", "-71.10", "-71.06", "-71.07", "-71.09", "-71.07",
                          "-71.05", "-71.08", "-71.07"))

Latitude <- as.numeric(c("42.35", "42.36","42.35", "42.36", "42.36", "42.34", "42.37", "42.34",
                         "42.34", "42.35"))

tourist_spots <- data.frame(Location, Longitude, Latitude)
#Add new Date column to simplify things
bostonCrime = bostonCrime %>% 
  select(everything()) %>% 
  mutate(Date = make_datetime(Year, Month, Day, Hour)) %>% 
  mutate(timeOfDay =  
           ifelse(Hour %in% 0:5, "Overnight",
                  ifelse(Hour %in% 6:10, "Morning",
                         ifelse(Hour %in% 11:14, "Afternoon",
                                ifelse(Hour %in% 15:17, "Late Afternoon",
                                       ifelse(Hour %in% 18:20, "Dinner", "Late Night"))))))

#Filter on important factors only 
bostonCrime
bostonCrimeFilt = bostonCrime %>% 
  select(Date, Day.of.week, Fiscal.Week, Lat, Long, District, timeOfDay)






#bostonCrime = read.csv('bostonCrime.csv', header = TRUE)
#Add new Date column to simplify things
bostonCrimeFilt

time_of_day = c("Morning", "Afternoon", "Late Afternoon", "Dinner", "Late Night", "Overnight")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Safety First"),
  h4("Plan your trip with safety in mind - Know when to visit and where to go"),
  
  h3("Interactive Boston Map"),
  h6("Click on the map to see popular destinations and zoom in "),
  plotlyOutput("distPlot"),
  
  hr(),
  fluidRow(
    h3("Change the Map View"),
    column(3,
      dateInput("date", label = "Select a Date", format = "mm/dd/yyyy", value = today()),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))),
    column(4,offset = 1,
           sliderTextInput("timeofday",
              "Change Time of Day:",
              choices = time_of_day
              )),
           )
    
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    df = data.frame(date = c(input$date))
    df$fiscalWeek <- as.integer(strftime(df$date, format = '%V'))
    df$weekday <- weekdays(as.Date(df$date))
    
    bostonCrimeFiltDayAndWeek = bostonCrimeFilt %>% 
      group_by(Fiscal.Week,Day.of.week) %>% 
      filter(Fiscal.Week == df$fiscalWeek) %>% 
      filter(Day.of.week == df$weekday)
    
    map <- ggmap(map) + geom_point(data = bostonCrimeFiltDayAndWeek %>% 
                                     filter(timeOfDay == input$timeofday),
              mapping = aes(x = Long, y = Lat), color = 'red', size = 15, alpha = .2) +
              geom_point(tourist_spots, mapping = aes(x = Longitude, y = Latitude, label = Location),
                color = "black", shape = 4, size = 2) +
              theme_void() + 
              scale_colour_manual(name = 'Legend',guide = 'legend')
    
    ggplotly(map, tooltip = "label")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

