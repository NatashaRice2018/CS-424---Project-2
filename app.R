# Author: Guillermo Rojas Hernandez

# Shiny dashboard showing data for Project 2

# I followed this tutorial for information on how to make tabs:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/ui.R

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)

######### Beginning of Project 2
### April Flight Data
# Read in file
AprilData<- read.table(file = "April_flights.cleaned.csv", header = FALSE,sep = '\t') 


#########




ui <- dashboardPage(
  dashboardHeader(title = "CS 424: Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName="dashboard"),
      menuItem("About", tabName="about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(title = "April Flights Table", solidHeader = TRUE, status = "primary", width = 12,
                    dataTableOutput("aprilFlightsTable")
                )
              ),   
              fluidRow(
                box(title = "O'Hare Airport", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("leaf")
                )
              )
      ), #end of tab item
      tabItem("about",
              h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddarth Basu"),
              a("Future link to project website", href="https://google.com")
      )
    ) # end of TabItems 
  ) # end of DashboardBody
) # end of DashboardPage

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  
  #Create table output of April Data table
  output$aprilFlightsTable <- DT::renderDataTable(
    DT::datatable(
      {AprilData},
      options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(4, 'asc'))
      ) 
    )
  )
  
  
  
  # use DT to help out with the tables - https://datatables.net/reference/option/
  
  # add a leaflet map and put a marker on it at the location of the lab
  # while not overly useful this can ceratinly be expnded upon
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.9073, lat = 41.9742, zoom = 13)
    map <- addMarkers(map, lng = -87.9073 , lat = 41.9742, popup = "O'Hare")
    map
  })
  
  
}

shinyApp(ui = ui, server = server)
