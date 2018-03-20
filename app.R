#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(dplyr)
library(plotly)
#Libaries needed to read in data faster.
library(data.table)
library(fasttime)
library(ggridges)
#libaries for the map
library(ggrepel)

library(gridExtra)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize

ColNames<- c("FL_DATE","AIRLINE_ID","CARRIER","ORIGIN_AIRPORT_ID","ORIGIN","ORIGIN_CITY_NAME","ORIGIN_STATE_NM","DEST_AIRPORT_ID","DEST_CITY_NAME","DEST_STATE_NM","DEP_TIME","DEP_DELAY","DEP_DELAY_NEW","ARR_TIME","ARR_DELAY","ARR_DELAY_NEW","CANCELLED","CANCELLATION_CODE","DIVERTED","ACTUAL_ELAPSED_TIME","FLIGHTS","DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY","NULL")



airport = list.files(pattern = "*.cleaned.csv")
allData <- lapply(airport, read.table , header = FALSE,sep='\t')
allData2 <- do.call(rbind, allData)
names(allData2)<-ColNames

allData2$ARR_TIME <- sprintf("%04d", allData2$ARR_TIME)
allData2$DEP_TIME <- sprintf("%04d", allData2$DEP_TIME)

x <- allData2$ARR_TIME
mins  <-  substr(x, nchar(x)-1, nchar(x))
hour  <-  substr(x, 0, nchar(x)-2)
time  <-  paste0(hour, ':', mins)
allData2$ARR_TIME_new <- fastPOSIXct(paste(allData2$FL_DATE, time), "%Y-%m-%d %H%M GMT" )
#allData2$DEP_TIME_new <- as.POSIXct(paste(allData2$FL_DATE, allData2$DEP_TIME), format = "%Y-%m-%d %H%M" )
x <- allData2$DEP_TIME
mins  <-  substr(x, nchar(x)-1, nchar(x))
hour  <-  substr(x, 0, nchar(x)-2)
time  <-  paste0(hour, ':', mins)
allData2$DEP_TIME_new <- fastPOSIXct(paste(allData2$FL_DATE, time), "%Y-%m-%d %H%M GMT" )

allData2$hour <- lubridate::hour( allData2$ARR_TIME_new)
#allData2$hour <-paste(allData2$hour,":00",sep = ""
allData2$day <- lubridate::day( allData2$ARR_TIME_new)
allData2$weekday <- weekdays( allData2$ARR_TIME_new)

allData2$hour_dep <- hour(allData2$DEP_TIME_new)
#allData2$hour_dep <-paste(allData2$hour_dep,":00",sep = "")
allData2$weekday_dep <- weekdays(allData2$DEP_TIME_new)

# passenger dataset
passenger_raw <- read.csv("passenger.csv")
passenger <- passenger_raw
passenger <- passenger[passenger$ORIGIN %in% c("MDW","ORD") | passenger$DEST %in% c("MDW","ORD"),]
passenger <- passenger[passenger$PASSENGERS !=0,]


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}



to12hour <- function(v){ 
  if (is.POSIXct(v)==TRUE){
    v<- format(strptime(v,"%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d %I:%M %p")
  }
  v
}

to24hour<-function(x){
  if (is.POSIXct(x)==FALSE){
    x<-as.POSIXct(aa,format="%Y-%m-%d %I:%M %p")
  }
  x 
}



airport_code_name<- group_by(allData2,ORIGIN_AIRPORT_ID,ORIGIN) %>% summarise()
colnames(airport_code_name)<-c("DEST_AIRPORT_ID","DEST")
allData2 <- merge(allData2,airport_code_name)

t<-c("24 hour","12 hour am/pm")
months <- c(1:12)
loc <- c('MDW','ORD')
airlines <- unique(allData2$CARRIER)
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
delays_type<- colnames(allData2)[23:27]
# Define UI for application that draws a histogram
ui <- 
  
  dashboardPage( 
    
    dashboardHeader(title = "Learning to Fly",
                    titleWidth=650
    ),
    
    dashboardSidebar(
      width= 650,
      sidebarMenu(
        menuItem("About", tabName="about"),
        menuItem("Arrivals & Departures", tabName="arrivals_departures"),
        menuItem("Arrivals & Departures: Airlines", tabName="arrivals_departures_airlines"),
        menuItem("Delays", tabName="delays"),
        menuItem("Top Airports", tabName="top_airports"),
        menuItem("Top Airlines", tabName="airline"),
        menuItem("Explore Date", tabName="date"),
        menuItem("Explore Weekday", tabName="day"),
        menuItem("Travel To & From Illinois", tabName="map"),
        menuItem("10 Interesting Travel Days", tabName="interesting_days"),
        menuItem("Flight Distances", tabName="flight_distances"),
        menuItem("Flight Facts", tabName="flight_facts"),
        menuItem("Unit",
                 box(
                   selectInput("Unit","Miles or Kilometers", choices=c("Miles","Kilometers"), selected = 'Miles'), width=650
                 )
        ),
        menuItem("Time",
                 box(
                   selectInput("Time", "12 hour am/pm time or 24 hour time ", choices=t, selected = '24 hour'), width=650
                 )
        )
      )
      
    ),
    
    dashboardBody(
      
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="custom.css")
      ),
      tabItems(
        tabItem("about",
                h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddharth Basu"),
                a("Link to project website", href="https://guillermokrh.github.io/CS-424---Project-2-Website/")
        ),
        
        tabItem("arrivals_departures",
                fluidRow(
                  tabBox(
                    tabPanel("Month Comparison",
                             fluidRow(
                               box(
                                 selectInput("arrivals_departures_month", "Select the month to visualize", choices=months, selected = 8), width=6
                               )
                               
                             ),
                             fluidRow(
                               column(width=2,
                                      box(title = "O'Hare Arrivals and Departures by Hour", solidHeader = TRUE, status = "primary", width = 12,
                                          dataTableOutput("tab2_ohare"), style="font-size:100%"
                                      )
                               ),
                               column(width=2,
                                      box(title = "Midway Arrivals and Departures by Hour", solidHeader = TRUE, status = "primary", width = 12,
                                          dataTableOutput("tab2_mdw")
                                      )
                               ),
                               column(width=8,
                                      box( title = "O'Hare Arrivals and Departures by Hour", solidHeader = TRUE, status = "primary", width = 12,
                                           plotOutput("BarByTimeOhare")
                                      ),
                                      box( title = "Midway Arrivals and Departures by Hour", solidHeader = TRUE, status = "primary", width = 12,
                                           plotOutput("BarByTimeMDW")
                                      )
                               )
                             ),
                             fluidRow(
                               box(title = "O'Hare Arrivals and Departures by Day of Week", solidHeader = TRUE, status = "primary", width = 2,
                                   dataTableOutput("tab3_ohare")
                               ),
                               box(title = "Midway Arrivals and Departures by Day of Week", solidHeader = TRUE, status = "primary", width = 2,
                                   dataTableOutput("tab3_mdw")
                               ),
                               box( title = "O'Hare Arrivals and Departures by Day of Week", solidHeader = TRUE, status = "primary", width = 4,
                                    plotOutput("BarByWeekdayOhare")
                               ),
                               box( title = "Midway Arrivals and Departures by Day of Week", solidHeader = TRUE, status = "primary", width = 4,
                                    plotOutput("BarByWeekdayMDW")
                               )
                             )
                    ),
                    tabPanel("Month-To-Month Comparison",
                             fluidRow(
                               
                               box( title = "Arrivals in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                    plotOutput("HeatArrHrORD", height=800)
                               ),
                               box( title = "Departures in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                    plotOutput("HeatDepHrORD", height=800)
                               )
                               
                             ),
                             fluidRow(
                               
                               box( title = "Arrivals in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                    plotOutput("HeatArrHrMdw", height=800)
                               ),
                               box( title = "Departures in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                    plotOutput("HeatDepHrMdw", height=800)
                               )
                               
                             )
                    ),
                    tabPanel("Additional Heatmaps",
                             fluidRow(
                               
                               box(title = "Heatmap week", solidHeader = TRUE, status = "primary",width = 12,
                                   
                                   plotOutput("HeatArrHourMon", height=800)),
                               box(title = "Heatmap hour", solidHeader = TRUE, status = "primary",width = 12,
                                   plotOutput("HeatArrHourWeek"))
                             )
                    ),
                    width = 12
                  )
                )
        ),
        
        tabItem("arrivals_departures_airlines",              
                fluidRow(
                  tabBox(
                    tabPanel("Month Comparison",
                             fluidRow(
                               box(
                                 selectInput("arrivals_departures_airlines_month", "Select the month to visualize", choices=months, selected = 8)
                               ),
                               
                               textOutput("arrivals_departures_airlines_result")
                               
                             ), 
                             fluidRow(
                               box(title = "O'Hare Departures and Arrivals By Airline", solidHeader = TRUE, status = "primary", width = 3,
                                   
                                   dataTableOutput("tab1_ord")
                                   
                               ),
                               box( title = "O'Hare Departure and Arrival Totals By Airline", solidHeader = TRUE, status = "primary", width = 9,
                                    plotOutput("TotalDepAriORD")
                               )
                             ),
                             fluidRow(
                               
                               box(title = "Midway Departures and Arrivals By Airline", solidHeader = TRUE, status = "primary", width = 3,
                                   dataTableOutput("tab1_mdw")
                               ),
                               box( title = "Midway Departure and Arrival Totals By Airline", solidHeader = TRUE, status = "primary", width = 9,
                                    plotOutput("TotalDepAriMDW")
                               )
                             )
                    ),
                    tabPanel( "Month to Month Comparison",
                              fluidRow(
                                
                                box( title = "Arrivals in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                     plotOutput("HeatArrMonORD")
                                ),
                                
                                box( title = "Departures in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                     plotOutput("HeatDepMonORD")
                                )
                                
                              ),
                              fluidRow(
                                
                                box( title = "Arrivals in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                     plotOutput("HeatArrMonMDW")
                                ),
                                
                                box( title = "Departures in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                                     plotOutput("HeatDepMonMDW")
                                )
                              )
                    ), # end tab panel
                    width=12) #end tab box
                ) # end fluid row
        ),
        tabItem("delays",                
                fluidRow(
                  tabBox(
                    tabPanel("Month Comparison",
                             fluidRow(
                               box(
                                 selectInput("delays_month", "Select the month to visualize", choices=months, selected = 4), width=4
                               )
                               #box(
                               #ORD/MDW choice
                               #selectInput("delays_airport", "Select the base airport to visualize", choices=loc, selected = 'MDW'), width=8
                               #)
                             ),
                             fluidRow (
                               box(title = "O'Hare Delays Per Hour", solidHeader = TRUE, status = "primary",width = 4,
                                   dataTableOutput("tab4_ord")),
                               
                               box( title = "O'Hare Delays Per Hour By Chosen Airport", solidHeader = TRUE, status = "primary", width = 8,
                                    plotOutput("BarDelaybyHourORD", height=800)
                               )
                             ),
                             fluidRow (
                               box(title = "Midway Delays Per Hour", solidHeader = TRUE, status = "primary",width = 4,
                                   dataTableOutput("tab4_mdw")),
                               
                               box( title = "Midway Delays Per Hour By Chosen Airport", solidHeader = TRUE, status = "primary", width = 8,
                                    plotOutput("BarDelaybyHourMDW", height=800)
                               )
                             )
                    ),
                    tabPanel("Month-To-Month Comparison",
                             fluidRow (
                               box(title = "Delays Change By month Midway", solidHeader = TRUE, status = "primary",width = 6,
                                   plotOutput("NumAndTypeOfDelayChangeMDW")),
                               box(title = "Delays Change By month O'Hare", solidHeader = TRUE, status = "primary",width = 6,
                                   plotOutput("NumAndTypeOfDelayChangeORD"))
                             )
                    ),
                    tabPanel("Delay Type Comparison",
                             fluidRow(
                               box(selectInput("delay_type", "Select delay type", choices=delays_type, selected = NULL), width=8),
                               box(title = "Delay Type Change By month", solidHeader = TRUE, status = "primary",width = 6,
                                   plotOutput("delay_type_month")),
                               box(title = "Delay Type Change By hour", solidHeader = TRUE, status = "primary",width = 10, plotOutput("delay_type_hour")
                               )
                               
                             )
                    ), 
                    width=12) #end tab box
                ) # end fluid row
        ), #end tab item
        tabItem("top_airports",                
                fluidRow(
                  tabBox(
                    tabPanel("Month Comparison",
                             fluidRow(
                               box(
                                 selectInput("top_airports_month", "Select the month to visualize", choices=months, selected = 4), width=4
                               )
                             ),
                             fluidRow( 
                               box(title = "O'Hare's 15 Most Common Airports ", solidHeader = TRUE, status = "primary",width = 4,
                                   dataTableOutput("tab5_ord")),
                               box( title = "O'Hare's 15 Most Common Airports", solidHeader = TRUE, status = "primary", width = 8,
                                    plotOutput("BarTop10ORD", height=750)
                               )
                             ),
                             fluidRow( 
                               box(title = "Midway's 15 Most Common Airports ", solidHeader = TRUE, status = "primary",width = 4,
                                   dataTableOutput("tab5_mdw")),
                               box( title = "Midway's 15 Most Common Airports", solidHeader = TRUE, status = "primary", width = 8,
                                    plotOutput("BarTop10MDW", height=750)
                               )
                             )
                    ),
                    tabPanel("Month-To-Month Comparison",
                             fluidRow(
                               box(title = "top 15 airports by month O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                                   plotOutput("Top15DestAirportsOrd")),
                               box(title = "top 15 airports by month Midway", solidHeader = TRUE, status = "primary", width = 6,
                                   plotOutput("Top15DestAirportsMdw"))
                             )
                    ),
                    tabPanel("Airport Comparison",
                             fluidRow(
                               #box(selectInput("top50_airport","Pick a airport",choices = top50, selected = '',width = 4)),
                               box(title = "Top 50 airports", solidHeader = TRUE, status = "primary", width = 12,
                                   uiOutput("top50Controls") 
                               )
                             ),
                             fluidRow(
                               box(title = "Arrivals & Departures By Hour for Selected Airport", solidHeader = TRUE, status = "primary", width = 12,
                                   plotOutput("Top50Airport_hour")
                               )
                             ),
                             fluidRow(
                               box(title = "Arrivals & Departures By Month for Selected Airport", solidHeader = TRUE, status = "primary", width = 12,
                                   plotOutput("Top50Airports_month"))
                             )
                    ), # end tab panel
                    width = 12) #end tab box
                ) # end fluid row
        ),
        tabItem("map",
                fluidRow(
                  box(title = "Percentage of Arrivals from Illinois", solidHeader = TRUE, status = "primary", width = 12,
                      plotlyOutput("MapDeparturePercent", height=900)
                  )
                ),
                fluidRow(
                  box(title = "Percentage of Departures from Illinois", solidHeader = TRUE, status = "primary", width = 12,
                      plotlyOutput("MapArrivalePercent", height=900)
                  )
                )
        ),
        
        tabItem("interesting_days",
                
                fluidRow(
                  box(
                    selectInput("day_airport", "Selected Airport:", choices=loc), width=6
                  )
                ),
                fluidRow(
                  box(title = "Flight/Delay", solidHeader = TRUE, status = "primary",width = 6,
                      dataTableOutput("interesting_day"))
                )
        ),
        
        tabItem("airline",
                fluidRow(
                  box(
                    selectInput("Airline", "Airline", choices=airlines), width=4
                  )
                ),
                fluidRow(
                  box(title = "Arrivals for selected Airport by month at O'Hare", solidHeader = TRUE, status = "primary", width = 5,
                      plotOutput("AirlineArrivalsMonthORD")
                  ),
                  box(title = "Arrivals for selected Airport by month at Midway", solidHeader = TRUE, status = "primary", width = 5,
                      plotOutput("AirlineArrivalsMonthMDW")
                  )
                ),
                fluidRow(
                  box(title = "Departures/Arrivals for selected Airport by hour at O'Hare", solidHeader = TRUE, status = "primary", width = 7,
                      plotOutput("AirlineDepartureHrORD")
                  )
                ),
                fluidRow(
                  box(title = "Departures/Arrivals for selected Airport by hour at Midway", solidHeader = TRUE, status = "primary", width = 7,
                      plotOutput("AirlineDepartureHRMDW")
                  )
                )
        ),
        tabItem("date",
                dateInput("date", label = h3("Date input"), value = "2017-01-01", min = "2017-01-01", max = "2017-12-31" ),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("value"))),
                fluidRow(
                  box(title = "Departures/Arrivals for selected Date by hour at Midway", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DateDepArrHrMDW", height=800)),
                  
                  box(title = "Departures/Arrivals for selected date by hour at O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DateDepArrHrORD", height=800)
                  )
                ),
                fluidRow(
                  box(title = "Delays for selected Date by hour at Midway", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DateDelayMDW", height=800)),
                  
                  box(title = "Delays for selected date by hour at O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DateDelayORD", height=800)
                  )
                )
        ),
        tabItem("day",
                fluidRow(
                  box(
                    selectInput("day", "Day of the Week ", choices=days), width=4
                  )),
                fluidRow(
                  box(title = "Departures/Arrivals for selected Date by hour at Midway", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DayDepArrHrMDW")),
                  
                  box(title = "Departures/Arrivals for selected date by hour at O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DayDepArrHrORD")
                  )
                ),
                fluidRow(
                  box(title = "Delays for selected Date by hour at Midway", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DayDelayMDW")),
                  
                  box(title = "Delays for selected date by hour at O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("DayDelayORD")
                  )
                )
        ),
        tabItem("flight_distances",
                fluidRow(
                  #ORD/MDW Choice
                  box(
                    selectInput("arrivals_departures_airport","Select airport:",choices=loc,selected='MDW'), width=6
                  ),
                  box(title = "Distance:", solidHeader = TRUE, status = "primary", width = 4,
                      uiOutput("distance") ),
                  box(title = "The number of Arrivals and Departures to and from different distances", solidHeader = TRUE, status = "primary", width = 8,plotOutput("numDistance")),
                  box(title = "The number of Flight over Distance Group",solidHeader = TRUE, status = "primary",width = 8,plotOutput("numDistance_bar"))
                ),
                fluidRow(
                  box(title = "Flight Time:", solidHeader = TRUE, status = "primary", width = 8,
                      plotOutput("numFlightTime") )
                )
        ),
        tabItem("flight_facts",
                fluidRow(
                  box(
                    selectInput("arrivals_departures_passenger_airport", "Select airport to visualize", choices=loc, selected = 'MDW')
                  )
                ),
                fluidRow(
                  box(title = "Departures/Arrivals for selected airport by Month", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("passenger_month"))
                ),
                fluidRow(
                  box(title = "Select Airline to visualize", solidHeader = TRUE, status = "primary", width = 6,
                      uiOutput("airlineControls") )
                ),
                fluidRow(
                  box(title = "Passengers for selected airline by Month", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("passenger_airline_month")) 
                ),
                fluidRow({
                  box(title = "Flights for selected airline by Month", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("flight_airline_month"))
                }
                )        
        )
      ) # end of TabItems 
      
    ), # end of DashboardBody
    skin = c("black")
  ) # end of DashboardPage



server <- function(input, output) {
  
  day_interesting <- reactive({
    switch(input$day_interesting,
           "Severe Storm" = "2017-06-14",
           "New Year's Day" = "2017-01-01",
           "Thanksgiving" = "2017-11-23",
           "Independence Day"="2017-07-04", 
           "Labor Day"="2017-09-04", 
           "Lollapalooza"="2017-08-03", 
           "Veteran's Day"="2017-11-11", 
           "Memorial Day"="2017-05-29", 
           "Columbus Day"="2017-10-09", 
           "Presidents' Day"="2017-02-20"
    )
  })
  
  # increase the default font size
  theme_set(theme_grey(base_size = 32)) 
  
  colorsAD <- c('#334464','#9DADBF')
  #Blue Gradient
  #colorsLH <- c("#99ccff", "#000066")
  #blue-green Gradiant
  #colorsLH <- c("#00C9FF", "#92FE9D")
  #blue-pink graident
  #colorsLH <- c("#00dbde", "#fc00ff")
  #Purple-Blue Graident
  colorsLH <- c("#0FEBDB", "#3A057A")
  
  dayOfWeek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  # justOneMonthReactive <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$Month)})
  arrivalsDeparturesAirlinesMonth <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$arrivals_departures_airlines_month)})
  arrivalsDeparturesMonth <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$arrivals_departures_month)})
  delaysMonth <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$delays_month)})
  topAirportsMonth <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$top_airports_month)})
  topInterestingDays <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$top_interesting_days)})
  
  passenger_arrival <- reactive({subset(passenger,passenger$DEST==input$arrivals_departures_passenger_airport)})
  passenger_depart <- reactive({subset(passenger,passenger$ORIGIN==input$arrivals_departures_passenger_airport)})
  
  
  output$result <- renderText({
    paste("You chose month:", input$Month, "and airport:", input$Airport)
  })
  
  switch_km<- function(x){
    tem <- x
    if(input$Unit =="Kilometers"){
      tem <- tem *1.61
    } 
    tem 
  }
  
  
  
  switch_hour<- function(x){
    c <- x
    #ifelse(input$Time=="24 hour", c<-paste(c$hour,":00",sep=""), ifelse(c<12, paste(c,":00 AM",sep=""),paste(cc-12,":00 PM",sep = "")))
    if (input$Time !="24 hour"){
      c <- ifelse(c<12, paste(c,":00 AM",sep=""),paste(c-12,":00 PM",sep = ""))
      #code currently has a 0:00am - we need to change that to 12 am.
      c[c == "0:00 AM"] <- "12:00 AM"
      c[c == "0:00 PM"] <- "12:00 PM"
    } else {
      c<-paste(c,":00",sep="")
    }
    c
  }
  
  switch_hour_chosen<- function(x, chosenTime){
    c <- x
    #ifelse(input$Time=="24 hour", c<-paste(c$hour,":00",sep=""), ifelse(c<12, paste(c,":00 AM",sep=""),paste(cc-12,":00 PM",sep = "")))
    if (chosenTime !="24 hour"){
      c <- ifelse(c<12, paste(c,":00 AM",sep=""),paste(c-12,":00 PM",sep = ""))
      #code currently has a 0:00am - we need to change that to 12 am.
      c[c == "0:00 AM"] <- "12:00 AM"
      c[c == "0:00 PM"] <- "12:00 PM"
    } else {
      c<-paste(c,":00",sep="")
    }
    c
  }
  
  set_time_factor<-function(x)
  {
    time <- x
    if (input$Time =="24 hour"){
      temp <- c("0:00","1:00", "2:00", "3:00", "4:00", "5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00","13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00");
    }
    else{
      temp <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM");
    }
    time <- factor(time, levels = temp)
    
    time
  }
  
  emptyarray <- function(type)
  {
    #empty array so we can plot without any data
    empty <- NULL
    if(type == "T")
    {
      if (input$Time =="24 hour"){
        empty$hour <- c("0:00","1:00", "2:00", "3:00", "4:00", "5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00","13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00");
      }
      else{
        empty$hour <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM");
      }
      empty$type <- c( rep("Arrival", times = 12), rep("Departure", times = 12))
      empty$hour <- set_time_factor(empty$hour)
      empty$count <- rep(0L, times = 24)
    }
    if(type == "M")
    {
      empty$month =  month.abb
      empty$month <- factor(empty$month, levels = month.abb)
      empty$type <- c( rep("Arrival", times = 6), rep("Departure", times = 6))
      empty$count <- rep(0L, times = 12)
    }
    
    empty <- as.data.frame(empty)
    
    empty
  }
  
  #########Passenger######################
  #selected Input for airport: arrivals_departures_passenger_airport
  
  output$passenger_month <- renderPlot({
    
    passenger_arrival <- passenger_arrival()
    passenger_depart <- passenger_depart()
    n_arrival <- aggregate(PASSENGERS ~ MONTH, data = passenger_arrival, sum)
    n_depart <- aggregate(PASSENGERS ~ MONTH, data = passenger_depart, sum)
    
    n_arrival$type = "Departure"
    n_depart$type ="Arrival"
    data1 <- merge(n_arrival,n_depart,all=TRUE)
    data1 <- data1[complete.cases(data1), ]
    data1 <- as.data.frame(data1)
    
    data1$MONTH <- month.abb[data1$MONTH]
    
    ggplot(data1, aes(x=MONTH,y = PASSENGERS/1000000 , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") + ggtitle("Number of Passengers Flying by Month of Year")+labs(x="Month", y = "Passengers (in millions)") +
      scale_fill_manual(values=colorsAD) + scale_x_discrete(limits = month.abb) +
      geom_text(aes(label=PASSENGERS), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
    
  })
  
  #################Interesting days##########################
  
  output$interesting_day <- DT::renderDataTable(
    DT::datatable({ 
      Occasion = c("New Year's Day","valentine's day","St Patrick's day","Memorial Day","Independence Day",   "Labor Day" , "Columbus Day", "Veteran's Day","Thanksgiving","Christmas")
      date = c("2017-01-01","2017-02-14","2017-03-17","2017-05-29","2017-07-04", "2017-09-04", "2017-10-09","2017-11-11","2017-11-23","2017-12-25")
      Data2 <- subset(allData2, (ORIGIN == input$day_airport | DEST == input$day_airport) )
      arr_data <- Data2[Data2$FL_DATE %in% date, ]
      dep_data <- Data2[Data2$FL_DATE %in% date, ]
      
      dep_hour <- group_by(dep_data,FL_DATE)  %>% select(ORIGIN) %>% filter(ORIGIN== input$day_airport ) %>% summarise(dep=n()) %>% arrange(FL_DATE)
      
      #dep_hour$type = "Departure"
      
      arr_hour <- group_by(arr_data,FL_DATE)  %>% select(DEST) %>% filter(DEST==input$day_airport) %>% summarise(arr=n())
      #arr_hour$type = "Arrival"
      delay_arr <- group_by(arr_data,FL_DATE) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST==input$day_airport ) %>% summarise(arrival_delays=n())
      delay_dep <- group_by(dep_data,FL_DATE)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(DEP_DELAY <0 & ORIGIN==input$day_airport) %>% summarise(departure_delays=n())
      
      
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data1 <- merge(delay_arr,delay_dep,all=TRUE)
      c <- merge(data2,data1,all=TRUE)
      
      c$event <- Occasion
      
      
      #set a factor for time baised on what clock we are in
      #data2$hour <- set_time_factor(data2$hour)
      c <- as.data.frame(c)
      c
      
      
      
    }))
  
  #####Passenger and Flight##########
  
  output$airlineControls <- renderUI({
    passenger_arrival <- passenger_arrival()
    n_arrival <- aggregate(PASSENGERS ~ UNIQUE_CARRIER_NAME + MONTH, data = passenger_arrival, sum)
    arrival <- group_by(allData2,CARRIER,MONTH=month(DEP_TIME_new))  %>% select(DEST,CARRIER ) %>% filter(DEST==input$arrivals_departures_passenger_airport) %>% summarise(Count=n())
    colnames(n_arrival)[1]="CARRIER"
    c <- merge(n_arrival,arrival)
    selectInput("airline_pass","Choose airline", unique(c$CARRIER),selected = NULL )
  })
  
  output$passenger_airline_month <- renderPlot({
    
    passenger_arrival <- passenger_arrival()
    passenger_depart <- passenger_depart()
    n_arrival <- aggregate(PASSENGERS ~ UNIQUE_CARRIER_NAME + MONTH, data = passenger_arrival, sum)
    n_depart <- aggregate(PASSENGERS ~ UNIQUE_CARRIER_NAME + MONTH, data = passenger_depart, sum)
    n_arrival$type = "Arrival"
    n_depart$type = "DEPARTURE"
    data1 = merge(n_arrival,n_depart,all=TRUE)
    arrival <- group_by(allData2,CARRIER,MONTH=month(DEP_TIME_new))  %>% select(DEST,CARRIER ) %>% filter(DEST==input$arrivals_departures_passenger_airport) %>% summarise(Count=n())
    depart <- group_by(allData2,CARRIER,MONTH=month(DEP_TIME_new))  %>% select(ORIGIN,CARRIER ) %>% filter(ORIGIN==input$arrivals_departures_passenger_airport) %>% summarise(Count=n())
    arrival$type = "Arrival"
    depart$type = "DEPARTURE"
    
    data2 <- merge(arrival,depart,all=TRUE)
    colnames(data1)[1]="CARRIER"
    
    c <- merge(data1,data2)
    tt <- c[c$CARRIER== input$airline_pass, ]
    
    tt$MONTH <- month.abb[tt$MONTH]
    
    normalizer <- max(tt$Count) / max(tt$PASSENGERS)
    
    ggplot(tt, aes(x=MONTH,y = PASSENGERS , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="MONTH", y = "Number of passengers") +
      scale_fill_manual(values=colorsAD) +scale_x_discrete(limits = month.abb)+
      geom_text(aes(label=PASSENGERS), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
    
  })
  
  output$flight_airline_month <- renderPlot({
    
    passenger_arrival <- passenger_arrival()
    passenger_depart <- passenger_depart()
    n_arrival <- aggregate(PASSENGERS ~ UNIQUE_CARRIER_NAME + MONTH, data = passenger_arrival, sum)
    n_depart <- aggregate(PASSENGERS ~ UNIQUE_CARRIER_NAME + MONTH, data = passenger_depart, sum)
    n_arrival$type = "Arrival"
    n_depart$type = "DEPARTURE"
    data1 = merge(n_arrival,n_depart,all=TRUE)
    arrival <- group_by(allData2,CARRIER,MONTH=month(DEP_TIME_new))  %>% select(DEST,CARRIER ) %>% filter(DEST==input$arrivals_departures_passenger_airport) %>% summarise(Count=n())
    depart <- group_by(allData2,CARRIER,MONTH=month(DEP_TIME_new))  %>% select(ORIGIN,CARRIER ) %>% filter(ORIGIN==input$arrivals_departures_passenger_airport) %>% summarise(Count=n())
    arrival$type = "Arrival"
    depart$type = "DEPARTURE"
    
    data2 <- merge(arrival,depart,all=TRUE)
    colnames(data1)[1]="CARRIER"
    
    c <- merge(data1,data2)
    tt <- c[c$CARRIER== input$airline_pass, ]
    
    tt$MONTH <- month.abb[tt$MONTH]
    
    normalizer <- max(tt$Count) / max(tt$PASSENGERS)
    
    ggplot(tt, aes(x=MONTH,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="MONTH", y = "Number of Flight") +
      scale_fill_manual(values=colorsAD) +scale_x_discrete(limits = month.abb)+
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
    
  })
  
  
  
  ####### Arrivals & Departures: Airlines Dashboard Tab
  # Allow a user to compare how many arrivals and departures are to and from different distances away.
  output$distance <- renderUI({
    #justOneMonthReactive <- topAirportsMonth()
    tem <- allData2$DISTANCE
    allData2$DISTANCE <- switch_km(allData2$DISTANCE)
    n_arrival <- group_by(allData2,DISTANCE,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport) %>% summarise(Count=n()) %>% arrange(desc(DISTANCE))
    #top50_airport <- group_by(allData2,DEST)  %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(50)
    #n_depart <- group_by(allData2,DISTANCE,ORIGIN)  %>% select(ORIGIN,DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(Count=n()) %>% arrange(desc(count))
    allData2$DISTANCE <- tem
    #data1 <- merge(n_arrival,n_depart,all=TRUE)
    selectInput("distance",h3(paste("Choose Distance away from or to", input$arrivals_departures_airport)), n_arrival$DISTANCE,selected = NULL )
    #textOutput('text_distance')
    
  })
  output$numDistance_bar <- renderPlot({
    tem <- allData2$DISTANCE
    allData2$DISTANCE <- switch_km(allData2$DISTANCE)
    tem1 <- c(0,249,499,749,999,1249,1499,1749,1999,2249,2499,100000)
    tem1 <- switch_km(tem1)
    allData2$group = cut(allData2$DISTANCE,tem1)
    n_arrival <- group_by(allData2,group)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_depart <- group_by(allData2,group)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_depart$type ="Arrival"
    data1 <- merge(n_arrival,n_depart,all=TRUE)
    data1 <- data1[complete.cases(data1), ]
    data1 <- as.data.frame(data1)
    allData2$DISTANCE <- tem
    ggplot(data1, aes(x=group,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Distance Group", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
  }
  
  )
  
  output$numDistance <- renderPlot({
    #justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
    tem <- allData2$DISTANCE
    allData2$DISTANCE <- switch_km(allData2$DISTANCE)
    n_arrival <- group_by(allData2,DISTANCE)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_depart <- group_by(allData2,DISTANCE)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_depart$type ="Arrival"
    data1 <- merge(n_arrival,n_depart,all=TRUE)
    data1 <- data1[complete.cases(data1), ]
    data1 <- as.data.frame(data1)
    
    allData2$DISTANCE <- tem
    tt <- data1[data1$DISTANCE == input$distance, ]
    ggplot(data1, aes(x=DISTANCE,y = Count , color=type)) + 
      geom_point()+geom_text_repel(data=tt,aes(label=Count,color = type),fontface = 'bold', 
                                   box.padding = 0.35, point.padding = 0.5,
                                   segment.color = 'grey50')
  }
  
  )
  
  output$numFlightTime <- renderPlot({
    #justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
    
    n_arrival <- group_by(allData2,ACTUAL_ELAPSED_TIME)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_depart <- group_by(allData2,ACTUAL_ELAPSED_TIME)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_depart$type ="Arrival"
    data1 <- merge(n_arrival,n_depart,all=TRUE)
    #data1[is.na(data1)] <- 0
    data1 <- as.data.frame(data1)
    data1 <- data1[complete.cases(data1), ]
    ggplot(data1, aes(x=ACTUAL_ELAPSED_TIME,y=Count , color=type)) + 
      geom_point()+geom_line(aes(color=type))+scale_x_continuous(name="Flight Time(minutes)")+
      scale_y_continuous(name="The number of Flights")
  }
  
  )
  
  ##### Arrivals & Departures Airlines Tab
  ### O'Hare Charts
  # Table: Departure and Arrival Totals by Airline
  output$tab1_ord <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
      ord_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="ORD") %>% summarise(ord_arrival=n())
      ord_departure <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="ORD") %>% summarise(ord_departure=n())
      data1 <- merge(ord_arrival, ord_departure, all=TRUE)
      data1[is.na(data1)] <- 0
      data1 <- as.data.frame(data1)
      data1
    },
    options = list(pageLength = 11)
    )
  )
  
  # Bar Chart: Departure and Arrival Totals by Arline
  output$TotalDepAriORD<- renderPlot({
    
    justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
    n_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="ORD") %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_dep <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="ORD") %>% summarise(Count=n())
    n_dep$type = "Arrival"
    data1 <- merge(n_arrival,n_dep,all=TRUE)
    data1[is.na(data1)] <- 0
    data1 <- as.data.frame(data1)
    
    # Question: Can we use tab4 here instead of generating the table in the code segment above? Or are there scope issues? 
    ggplot(data1, aes(x=CARRIER,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Ariline", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 8300))
    
  })
  
  #### Midway Charts
  # Table: Departure and Arrival Totals by Airline
  output$tab1_mdw <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
      mdw_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="MDW") %>% summarise(ord_arrival=n())
      mdw_departure <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="MDW") %>% summarise(ord_departure=n())
      data1 <- merge(mdw_arrival, mdw_departure, all=TRUE)
      data1[is.na(data1)] <- 0
      data1 <- as.data.frame(data1)
      data1
    },
    options = list(pageLength = 11)
    )
  )
  
  # Bar Chart: Departure and Arrival Totals by Arline
  output$TotalDepAriMDW<- renderPlot({
    
    justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
    n_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="MDW") %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_dep <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="MDW") %>% summarise(Count=n())
    n_dep$type = "Arrival"
    data1 <- merge(n_arrival,n_dep,all=TRUE)
    data1[is.na(data1)] <- 0
    data1 <- as.data.frame(data1)
    
    # Question: Can we use tab4 here instead of generating the table in the code segment above? Or are there scope issues? 
    ggplot(data1, aes(x=CARRIER,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Ariline", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 8300))
    
  }) 
  
  
  ########### Arrivals and Departures Tab
  #### O'Hare Charts
  # Table: The total number of departures and total number of arrivals for each hour of the day across that month
  output$tab2_ohare <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN=="ORD") %>% summarise(count=n())
      #dep_hour$type = "Departure"
      arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
      #arr_hour$type = "Arrival"
      colnames(dep_hour)<-c("hour","Departures")
      colnames(arr_hour)<-c("hour","Arrivals")
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2$hour<-switch_hour(data2$hour)
      #set a factor for time baised on what clock we are in
      #data2$hour <- set_time_factor(data2$hour)
      data2 <- as.data.frame(data2)
      
      data2
      
    },
    options = list(pageLength = 12)
    )
    
  )
  
  # Bar Graph: Arrivals and Departures by Hour of the Day 
  output$BarByTimeOhare <- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN=="ORD") %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    ggplot(data2, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 2200))
    
  })
  
  
  # Table: The total number of departures and total number of arrivals for each day of the week across that month
  output$tab3_ohare <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN=="ORD") %>% summarise(number_dep=n())
      arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(number_arrival=n())
      colnames(dep_weekday)<-c("weekday","Departures")
      colnames(arr_weekday)<-c("weekday","Arrivals")
      data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
      data3 <- subset(data3,!is.na(data3$weekday))
      data3[is.na(data3)] <- 0
      data3 <- as.data.frame(data3)
      #reodder days of week
      data3$weekday <- factor(data3$weekday, levels = dayOfWeek )
      data3
      
    },
    options = list(pageLength = 12, order = list(list(1, 'asc')))
    )
    
  )
  
  # Bar Graph: Arrivals and Departures by Weekday
  output$BarByWeekdayOhare<- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN=="ORD") %>% summarise(Count=n())
    dep_weekday$type = "Departure"
    arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(Count=n())
    arr_weekday$type = "Arrival"
    colnames(dep_weekday)<-c("weekday","Count", "type")
    data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
    data3 <- subset(data3,!is.na(data3$weekday))
    data3[is.na(data3)] <- 0
    data3 <- as.data.frame(data3)
    data3$weekday <- factor(data3$weekday, levels = dayOfWeek)
    
    
    ggplot(data3, aes(x=weekday,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Day", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 4500))
    
  })
  #### Midway Charts
  # Table: The total number of departures and total number of arrivals for each hour of the day across that month
  output$tab2_mdw <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN=="MDW") %>% summarise(count=n())
      #dep_hour$type = "Departure"
      arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
      #arr_hour$type = "Arrival"
      colnames(dep_hour)<-c("hour","Departures")
      colnames(arr_hour)<-c("hour","Arrivals")
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2$hour<-switch_hour(data2$hour)
      #set a factor for time baised on what clock we are in
      #data2$hour <- set_time_factor(data2$hour)
      data2 <- as.data.frame(data2)
      
      data2
      
    },
    options = list(pageLength = 12)
    )
    
  )
  
  # Bar Graph: Arrivals and Departures by Hour of the Day 
  output$BarByTimeMDW <- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN=="MDW") %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    ggplot(data2, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 2200))
    
  })
  
  
  # Table: The total number of departures and total number of arrivals for each day of the week across that month
  output$tab3_mdw <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN=="MDW") %>% summarise(number_dep=n())
      arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(number_arrival=n())
      colnames(dep_weekday)<-c("weekday","Departures")
      colnames(arr_weekday)<-c("weekday","Arrivals")
      data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
      data3 <- subset(data3,!is.na(data3$weekday))
      data3[is.na(data3)] <- 0
      data3 <- as.data.frame(data3)
      #reodder days of week
      data3$weekday <- factor(data3$weekday, levels = dayOfWeek )
      data3
      
    },
    options = list(pageLength = 8, order = list(list(1, 'asc')))
    )
    
  )
  
  # Bar Graph: Arrivals and Departures by Weekday
  output$BarByWeekdayMDW<- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN=="MDW") %>% summarise(Count=n())
    dep_weekday$type = "Departure"
    arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(Count=n())
    arr_weekday$type = "Arrival"
    colnames(dep_weekday)<-c("weekday","Count", "type")
    data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
    data3 <- subset(data3,!is.na(data3$weekday))
    data3[is.na(data3)] <- 0
    data3 <- as.data.frame(data3)
    data3$weekday <- factor(data3$weekday, levels = dayOfWeek)
    
    
    ggplot(data3, aes(x=weekday,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Day", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 4500))
    
  })
  
  ######    
  output$interestingDays<- DT::renderDataTable(
    
    DT::datatable({
      
      
      
      HolidayType <- c("Christmas","New Year","Thanksgiving","Independence Day","Labor Day","Columbus Day","Lollapaloza", "Veterans Day")
      
      Date <- c("Maybe","Maybe","Yes","Maybe","Maybe","No","Yes","No")
      
      results <- table(HolidayType, Date)
      
      results
      
      
      
    })
    
  )
  
  ########### Delays Dashboard Tab
  #### O'Hare charts
  # Table: Delays for Each Hour
  output$tab4_ord<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- delaysMonth()
      
      delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == "ORD") %>% summarise(arrival_delays=n())
      delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN =="ORD" & DEP_DELAY <0 ) %>% summarise(departure_delays=n())
      colnames(delay_dep_hour)<-c("hour","number_dep_delay")
      data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
      data4 <- subset(data4,!is.na(data4$hour))
      data4[is.na(data4)] <- 0
      data4$hour<-switch_hour(data4$hour)
      data4 <- as.data.frame(data4)
      
      data4$total <- data4$number_dep_delay + data4$arrival_delays
      data4$percent <- percent(data4$total/sum(data4$total))
      
      data4
    },
    options = list(pageLength = 12))
  )
  
  # Bar Chart: Delays for Each Hour
  output$BarDelaybyHourORD<- renderPlot({
    justOneMonthReactive <- delaysMonth()
    
    delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == "ORD" ) %>% summarise(count=n())
    delay_arr_hour$type = "Departure"
    delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN == "ORD" & DEP_DELAY <0 ) %>% summarise(count=n())
    delay_dep_hour$type = "Arrival"
    colnames(delay_dep_hour)<-c("hour","count", "type")
    data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
    data4 <- subset(data4,!is.na(data4$hour))
    data4[is.na(data4)] <- 0
    data4$hour<-switch_hour(data4$hour)
    data4$hour <- set_time_factor(data4$hour)
    data4 <- as.data.frame(data4)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data4$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data4 <- merge(data4, empty, all= TRUE)
    
    
    
    ggplot(data4, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Time", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 1500))
  })
  
  #### Midway Charts
  # Table: Delays for Each Hour
  output$tab4_mdw<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- delaysMonth()
      
      delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == "MDW") %>% summarise(arrival_delays=n())
      delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN =="MDW" & DEP_DELAY <0 ) %>% summarise(departure_delays=n())
      colnames(delay_dep_hour)<-c("hour","number_dep_delay")
      data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
      data4 <- subset(data4,!is.na(data4$hour))
      data4[is.na(data4)] <- 0
      data4$hour<-switch_hour(data4$hour)
      data4 <- as.data.frame(data4)
      
      data4$total <- data4$number_dep_delay + data4$arrival_delays
      data4$percent <- percent(data4$total/sum(data4$total))
      
      data4
    },
    options = list(pageLength = 12))
  )
  
  # Bar Chart: Delays for Each Hour
  output$BarDelaybyHourMDW<- renderPlot({
    justOneMonthReactive <- delaysMonth()
    
    delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == "MDW" ) %>% summarise(count=n())
    delay_arr_hour$type = "Departure"
    delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN == "MDW" & DEP_DELAY <0 ) %>% summarise(count=n())
    delay_dep_hour$type = "Arrival"
    colnames(delay_dep_hour)<-c("hour","count", "type")
    data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
    data4 <- subset(data4,!is.na(data4$hour))
    data4[is.na(data4)] <- 0
    data4$hour<-switch_hour(data4$hour)
    data4$hour <- set_time_factor(data4$hour)
    data4 <- as.data.frame(data4)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data4$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data4 <- merge(data4, empty, all= TRUE)
    
    
    
    ggplot(data4, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Time", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 1500))
  })
  
  # select delay type 
  
  output$delay_type_month <- renderPlot({
    
    coln <- match(input$delay_type,names(allData2))
    temp <- subset(allData2,allData2[coln]>0)
    cd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(input$delay_type) %>% summarise(count=n())
    colnames(cd)<-c("month", "count")
    cd$month <- month.abb[cd$month]
    cd$month <- factor(cd$month, levels = month.abb)
    cd <- as.data.frame(cd)
    ggplot(cd, aes(x=month,y = count)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Month", y = "Number of Delay") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
    
    
    
  }
  
  )
  
  output$delay_type_hour <- renderPlot({
    
    coln <- match(input$delay_type,names(allData2))
    temp <- subset(allData2,allData2[coln]>0)
    
    dep_hour <- group_by(temp,hour_dep)  %>% select(input$delay_type) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(temp,hour)  %>% select(input$delay_type) %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    ggplot(data2, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
  }
  
  )
  
  ######### Top Airports Dashboard Tab
  ### O'Hare charts
  # Table: the number of flights for the most common 15 destination and airports airports for selected airport
  output$tab5_ord<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- topAirportsMonth()
      most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == "ORD") %>% summarise(departures=n()) %>% arrange(desc(departures)) %>% top_n(15)
      
      # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
      most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == "ORD") %>% summarise(arrivals=n()) %>% arrange(desc(arrivals)) %>% top_n(15)
      most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$arrivals)) 
      most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$departures))
      
      data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
      data5
      
    })
  )
  
  # Bar Chart: 15 most common destination and arrival aiports for selected airports
  output$BarTop10ORD <- renderPlot({
    justOneMonthReactive <- topAirportsMonth()
    most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == "ORD") %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    
    # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
    most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == "ORD" ) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$count)) 
    most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$count))
    
    most_common_15_destinations$type = "Departure"
    most_common_15_arrivals$type = "Arrival"
    
    colnames(most_common_15_destinations)[colnames(most_common_15_destinations)=="DEST"] <- "LOC"
    colnames(most_common_15_arrivals)[colnames(most_common_15_arrivals)=="ORIGIN"] <- "LOC"
    
    data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
    
    data5$LOC <- reorder(data5$LOC, -data5$count)
    
    
    ggplot(data5, aes(x= LOC,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Airport", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 850))
  })
  ### Midway charts
  # Table: the number of flights for the most common 15 destination and airports airports for selected airport
  output$tab5_mdw<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- topAirportsMonth()
      most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == "MDW") %>% summarise(departures=n()) %>% arrange(desc(departures)) %>% top_n(15)
      
      # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
      most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == "MDW") %>% summarise(arrivals=n()) %>% arrange(desc(arrivals)) %>% top_n(15)
      most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$arrivals)) 
      most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$departures))
      
      data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
      data5
      
    })
  )
  
  # Bar Chart: 15 most common destination and arrival aiports for selected airports
  output$BarTop10MDW <- renderPlot({
    justOneMonthReactive <- topAirportsMonth()
    most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == "MDW") %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    
    # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
    most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == "MDW" ) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$count)) 
    most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$count))
    
    most_common_15_destinations$type = "Departure"
    most_common_15_arrivals$type = "Arrival"
    
    colnames(most_common_15_destinations)[colnames(most_common_15_destinations)=="DEST"] <- "LOC"
    colnames(most_common_15_arrivals)[colnames(most_common_15_arrivals)=="ORIGIN"] <- "LOC"
    
    data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
    
    data5$LOC <- reorder(data5$LOC, -data5$count)
    
    
    ggplot(data5, aes(x= LOC,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Airport", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0) + scale_y_continuous(limits=c(0, 850))
  })
  
  # top 50 airport selected 
  output$top50Controls <- renderUI({
    #justOneMonthReactive <- topAirportsMonth()
    
    top50_airport <- group_by(allData2,DEST)  %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(50)
    selectInput("top50","Choose Cities", top50_airport$DEST,selected = NULL )
  }
  
  )
  
  output$Top50Airport_hour <- renderPlot({
    # justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_hour <- group_by(allData2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN==input$top50 ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(allData2,hour)  %>% select(DEST) %>% filter(DEST==input$top50) %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    ggplot(data2, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=5.0)
    
  })
  
  output$Top50Airports_month <- renderPlot(
    {
      
      dep_month <- group_by(allData2,month(allData2$DEP_TIME_new) ) %>% select(ORIGIN) %>% filter(ORIGIN==input$top50 ) %>% summarise(count=n())
      colnames(dep_month)[1]<-"month"
      dep_month$type = "Departure"
      
      arr_month <- group_by(allData2,month(allData2$ARR_TIME_new))  %>% select(DEST) %>% filter(DEST==input$top50) %>% summarise(count=n())
      colnames(arr_month)[1]<-"month"
      arr_month$type = "Arrival"
      
      data2 <- merge(dep_month,arr_month,all=TRUE)
      data2 <- subset(data2,!is.na(data2$month))
      #set a factor for time baised on what clock we are in
      data2$month <- month.abb[data2$month]
      data2$month <- factor(data2$month, levels = month.abb)
      data2 <- as.data.frame(data2)
      
      ggplot(data2, aes(x=month,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Month", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=-0.3,
                  position = position_dodge(0.9), size=5.0)
      
    }
  )
  
  ### heatmap of set###
  output$HeatArrHourMon <- renderPlot(
    {
      OhrArr <- subset(allData2, DEST== input$day_airport)
      OhrArr2 <- group_by(OhrArr,hour, month(OhrArr$ARR_TIME_new) ) %>% select(DEST,hour, ARR_TIME_new )  %>% summarise(count=n())
      OhrArr2[is.na(OhrArr2)] <- 0
      OhrArr2$hour<-switch_hour(OhrArr2$hour)
      OhrArr2$hour <- set_time_factor(OhrArr2$hour)
      
      colnames(OhrArr2)<-c("Hour","Month", "Count")
      OhrArr2$scale <- scale(OhrArr2$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2$Month <- month.abb[OhrArr2$Month]
      OhrArr2$Month <- factor(OhrArr2$Month, levels = month.abb)
      
      OhrArr2_delay <- group_by(OhrArr,hour, month(OhrArr$ARR_TIME_new) ) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST==input$day_airport ) %>% summarise(arrival_delays=n())
      OhrArr2_delay[is.na(OhrArr2_delay)] <- 0
      OhrArr2_delay$hour<-switch_hour(OhrArr2_delay$hour)
      OhrArr2_delay$hour <- set_time_factor(OhrArr2_delay$hour)
      
      colnames(OhrArr2_delay)<-c('Hour','Month', 'Count')
      OhrArr2_delay$scale <- scale(OhrArr2_delay$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2_delay$Month <- month.abb[OhrArr2_delay$Month]
      OhrArr2_delay$Month <- factor(OhrArr2_delay$Month, levels = month.abb)
      
      p1<-ggplot(OhrArr2, aes(x=Hour, y=Month )) +
        geom_tile(aes(fill = Count), colour = 'white') +
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Month, label = Count), color = 'black', size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      p2<-ggplot(OhrArr2_delay, aes(x=Hour, y=Month )) +
        geom_tile(aes(fill = Count), colour = 'white') +
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Month, label = Count), color = 'black', size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      OhrArr <- subset(allData2, ORIGIN== input$day_airport)
      OhrArr2 <- group_by(OhrArr,hour_dep, month(OhrArr$ARR_TIME_new) ) %>% select(DEST,hour, ARR_TIME_new )  %>% summarise(count=n())
      OhrArr2[is.na(OhrArr2)] <- 0
      OhrArr2$hour_dep<-switch_hour(OhrArr2$hour_dep)
      OhrArr2$hour_dep <- set_time_factor(OhrArr2$hour_dep)
      
      colnames(OhrArr2)<-c('Hour','Month', 'Count')
      OhrArr2$scale <- scale(OhrArr2$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2$Month <- month.abb[OhrArr2$Month]
      OhrArr2$Month <- factor(OhrArr2$Month, levels = month.abb)
      
      OhrArr2_delay <- group_by(OhrArr,hour_dep, month(OhrArr$ARR_TIME_new) ) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(DEP_DELAY<0 & ORIGIN==input$day_airport ) %>% summarise(arrival_delays=n())
      OhrArr2_delay[is.na(OhrArr2_delay)] <- 0
      OhrArr2_delay$hour_dep<-switch_hour(OhrArr2_delay$hour_dep)
      OhrArr2_delay$hour_dep <- set_time_factor(OhrArr2_delay$hour_dep)
      
      colnames(OhrArr2_delay)<-c("Hour","Month", "Count")
      OhrArr2_delay$scale <- scale(OhrArr2_delay$Count, center = FALSE, scale = max(OhrArr2_delay$Count, na.rm = TRUE))
      OhrArr2_delay$Month <- month.abb[OhrArr2_delay$Month]
      OhrArr2_delay$Month <- factor(OhrArr2_delay$Month, levels = month.abb)
      
      p3<-ggplot(OhrArr2, aes(x=Hour, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") +
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Month, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      p4<-ggplot(OhrArr2_delay, aes(x=Hour, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") +
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Month, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      grid.arrange(arrangeGrob(arrangeGrob(p1,p2,p3,p4),ncol=1))
      
    }
  )
  
  output$HeatArrHourWeek <- renderPlot(
    {
      OhrArr <- subset(allData2, DEST==input$day_airport)
      OhrArr2 <- group_by(OhrArr,hour, weekday) %>% select(DEST,hour, weekday )  %>% summarise(count=n())
      OhrArr2$hour<-switch_hour(OhrArr2$hour)
      OhrArr2$hour <- set_time_factor(OhrArr2$hour)
      
      colnames(OhrArr2)<-c("Hour","Week", "Count")
      OhrArr2$scale <- scale(OhrArr2$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2$Week <- factor(OhrArr2$Week, levels = dayOfWeek )
      
      OhrArr2_delay <- group_by(OhrArr,hour, weekday)%>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST==input$day_airport )%>% summarise(count=n())
      OhrArr2_delay$hour<-switch_hour(OhrArr2_delay$hour)
      OhrArr2_delay$hour <- set_time_factor(OhrArr2_delay$hour)
      
      colnames(OhrArr2_delay)<-c("Hour","Week", "Count")
      OhrArr2_delay$scale <- scale(OhrArr2_delay$Count, center = FALSE, scale = max(OhrArr2_delay$Count, na.rm = TRUE))
      OhrArr2_delay$Week <- factor(OhrArr2_delay$Week, levels = dayOfWeek )
      
      
      
      p1<-ggplot(OhrArr2, aes(x=Hour, y=Week )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Week, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      p2<-ggplot(OhrArr2_delay, aes(x=Hour, y=Week )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Week, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      OhrArr <- subset(allData2, ORIGIN==input$day_airport)
      OhrArr2 <- group_by(OhrArr,hour_dep, weekday) %>% select(DEST,hour_dep, weekday )  %>% summarise(count=n())
      OhrArr2$hour_dep<-switch_hour(OhrArr2$hour_dep)
      OhrArr2$hour_dep <- set_time_factor(OhrArr2$hour_dep)
      
      colnames(OhrArr2)<-c("Hour","Week", "Count")
      OhrArr2$scale <- scale(OhrArr2$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2$Week <- factor(OhrArr2$Week, levels = dayOfWeek )
      
      OhrArr2_delay <- group_by(OhrArr,hour_dep, weekday)%>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(DEP_DELAY<0 & ORIGIN==input$day_airport )%>% summarise(count=n())
      OhrArr2_delay$hour_dep<-switch_hour(OhrArr2_delay$hour_dep)
      OhrArr2_delay$hour_dep <- set_time_factor(OhrArr2_delay$hour_dep)
      
      colnames(OhrArr2_delay)<-c("Hour","Week", "Count")
      OhrArr2_delay$scale <- scale(OhrArr2_delay$Count, center = FALSE, scale = max(OhrArr2_delay$Count, na.rm = TRUE))
      OhrArr2_delay$Week <- factor(OhrArr2_delay$Week, levels = dayOfWeek )
      
      
      
      p3<-ggplot(OhrArr2, aes(x=Hour, y=Week )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Week, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      p4<-ggplot(OhrArr2_delay, aes(x=Hour, y=Week )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white'))+geom_text(aes(x=Hour, y=Week, label = Count), color = "black", size = 4)
      #scale_y_continuous(breaks=c(3,6,9,12))
      
      
      
      grid.arrange(arrangeGrob(arrangeGrob(p1,p2,p3,p4),ncol=1))
    }
  )
  
  output$HeatArrMonORD <- renderPlot(
    {
      OhrArr <- subset(allData2, DEST== "ORD")
      OhrArr2 <- group_by(OhrArr,AIRLINE_ID, month(OhrArr$ARR_TIME_new) ) %>% select(DEST,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      colnames(OhrArr2)<-c("AIRLINE_ID","Month", "Count")
      OhrArr2$scale <- scale(OhrArr2$Count, center = FALSE, scale = max(OhrArr2$Count, na.rm = TRUE))
      OhrArr2$Month <- month.abb[OhrArr2$Month]
      OhrArr2$Month <- factor(OhrArr2$Month, levels = month.abb)
      
      ggplot(OhrArr2, aes(x=AIRLINE_ID, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2])+
        theme(panel.background = element_rect(fill = 'white')) 
      #scale_y_continuous(breaks=c(3,6,9,12))
    }
  )
  
  output$HeatDepMonORD <- renderPlot(
    {
      OhrDep <- subset(allData2, ORIGIN== "ORD")
      OhrDep2 <- group_by(OhrDep,AIRLINE_ID, month(OhrDep$ARR_TIME_new) ) %>% select(ORIGIN,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      colnames(OhrDep2)<-c("AIRLINE_ID","Month", "Count")
      OhrDep2$scale <- scale(OhrDep2$Count, center = FALSE, scale = max(OhrDep2$Count, na.rm = TRUE))
      OhrDep2$Month <- month.abb[OhrDep2$Month]
      OhrDep2$Month <- factor(OhrDep2$Month, levels = month.abb)
      
      ggplot(OhrDep2, aes(x=AIRLINE_ID, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))s
    }
  )
  
  
  
  output$HeatArrMonMDW <- renderPlot(
    {
      MdwArr <- subset(allData2, DEST== "MDW")
      MdwArr2 <- group_by(MdwArr,AIRLINE_ID, month(MdwArr$ARR_TIME_new) ) %>% select(DEST,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      colnames(MdwArr2)<-c("AIRLINE_ID","Month", "Count")
      MdwArr2$scale <- scale(MdwArr2$Count, center = FALSE, scale = max(MdwArr2$Count, na.rm = TRUE))
      MdwArr2$Month <- month.abb[MdwArr2$Month]
      MdwArr2$Month <- factor(MdwArr2$Month, levels = month.abb)
      
      ggplot(MdwArr2, aes(x=AIRLINE_ID, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))s
    }
  )
  
  output$HeatDepMonMDW <- renderPlot(
    {
      MdwDep <- subset(allData2, ORIGIN== "MDW")
      MdwDep2 <- group_by(MdwDep,AIRLINE_ID, month(MdwDep$ARR_TIME_new) ) %>% select(ORIGIN,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      colnames(MdwDep2)<-c("AIRLINE_ID","Month", "Count")
      MdwDep2$scale <- scale(MdwDep2$Count, center = FALSE, scale = max(MdwDep2$Count, na.rm = TRUE))
      MdwDep2$Month <- month.abb[MdwDep2$Month]
      MdwDep2$Month <- factor(MdwDep2$Month, levels = month.abb)
      
      
      ggplot(MdwDep2, aes(x=AIRLINE_ID, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))s
    }
  )
  
  output$HeatArrHrORD <- renderPlot(
    {
      OhrArr <- subset(allData2, DEST== "ORD")
      OhrArr2 <- group_by(OhrArr,AIRLINE_ID, hour ) %>% select(DEST,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      OhrArr2$scale <- scale(OhrArr2$count, center = FALSE, scale = max(OhrArr2$count, na.rm = TRUE))
      OhrArr2$hour<- switch_hour(OhrArr2$hour)
      OhrArr2$hour <- set_time_factor(OhrArr2$hour)
      
      ggplot(OhrArr2, aes(x=AIRLINE_ID, y=hour )) +
        geom_tile(aes(fill = count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))
    }
  )
  
  output$HeatDepHrORD <- renderPlot(
    {
      OrdDep <- subset(allData2, ORIGIN== "ORD")
      OrdDep2 <- group_by(OrdDep,AIRLINE_ID, hour ) %>% select(AIRLINE_ID )  %>% summarise(count=n())
      OrdDep2$scale <- scale(OrdDep2$count, center = FALSE, scale = max(OrdDep2$count, na.rm = TRUE))
      OrdDep2$hour<-switch_hour(OrdDep2$hour)
      OrdDep2$hour <- set_time_factor(OrdDep2$hour)
      
      ggplot(OrdDep2, aes(x=AIRLINE_ID, y=hour )) +
        geom_tile(aes(fill = count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))
    }
  )
  
  
  
  output$HeatArrHrMdw <- renderPlot(
    {
      MdwArr <- subset(allData2, DEST== "MDW")
      MdwArr2 <- group_by(MdwArr,AIRLINE_ID, hour ) %>% select(DEST,AIRLINE_ID, ARR_TIME_new )  %>% summarise(count=n())
      MdwArr2$scale <- scale(MdwArr2$count, center = FALSE, scale = max(MdwArr2$count, na.rm = TRUE))
      MdwArr2$hour<- switch_hour(MdwArr2$hour)
      MdwArr2$hour <- set_time_factor(MdwArr2$hour)
      
      ggplot(MdwArr2, aes(x=AIRLINE_ID, y=hour )) +
        geom_tile(aes(fill = count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))
    }
  )
  
  output$HeatDepHrMdw <- renderPlot(
    {
      MdwDep <- subset(allData2, ORIGIN== "MDW")
      MdwDep2 <- group_by(MdwDep,AIRLINE_ID, hour ) %>% select(AIRLINE_ID )  %>% summarise(count=n())
      MdwDep2$scale <- scale(MdwDep2$count, center = FALSE, scale = max(MdwDep2$count, na.rm = TRUE))
      MdwDep2$hour<-switch_hour(MdwDep2$hour)
      MdwDep2$hour <- set_time_factor(MdwDep2$hour)
      
      ggplot(MdwDep2, aes(x=AIRLINE_ID, y=hour )) +
        geom_tile(aes(fill = count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      #scale_y_continuous(breaks=c(3,6,9,12))
    }
  )
  
  output$Top15DestAirportsOrd <- renderPlot(
    {
      OrdDep <- subset(allData2, ORIGIN== "ORD")
      
      most_common_15_destinations <- group_by(OrdDep,DEST,month(OrdDep$ARR_TIME_new) )  %>% select(ORIGIN) %>% filter(ORIGIN == "ORD") %>% summarise(departures=n()) %>% arrange(desc(departures))
      colnames(most_common_15_destinations)<-c("DEST","Month", "Count")
      most_common_15_destinations <-most_common_15_destinations %>%  group_by(Month) %>%  top_n(n=15, wt = Count)
      most_common_15_destinations$DEST <- rep(most_common_15_destinations$DEST)
      most_common_15_destinations$Month <- month.abb[most_common_15_destinations$Month]
      most_common_15_destinations$Month <- factor(most_common_15_destinations$Month, levels = month.abb)
      
      ggplot(most_common_15_destinations, aes(x=DEST, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      
    }
  )
  
  
  output$Top15DestAirportsMdw <- renderPlot(
    {
      MdwDep <- subset(allData2, ORIGIN== "MDW")
      
      most_common_15_destinations <- group_by(MdwDep,DEST,month(MdwDep$ARR_TIME_new) )  %>% select(ORIGIN) %>% filter(ORIGIN == "MDW") %>% summarise(departures=n()) %>% arrange(desc(departures))
      colnames(most_common_15_destinations)<-c("DEST","Month", "Count")
      most_common_15_destinations <-most_common_15_destinations %>%  group_by(Month) %>%  top_n(n=15, wt = Count)
      most_common_15_destinations$DEST <- rep(most_common_15_destinations$DEST)
      most_common_15_destinations$Month <- month.abb[most_common_15_destinations$Month]
      most_common_15_destinations$Month <- factor(most_common_15_destinations$Month, levels = month.abb)
      
      ggplot(most_common_15_destinations, aes(x=DEST, y=Month )) +
        geom_tile(aes(fill = Count), colour = "white") + 
        scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
        theme(panel.background = element_rect(fill = 'white'))
      
    }
  )
  
  output$Top15DestAirportsOrdChart<- renderPlot(
    {
      most_common_15_destinations <- group_by(OrdDep,DEST,month(OrdDep$ARR_TIME_new) )  %>% select(ORIGIN) %>% filter(ORIGIN == "ORD") %>% summarise(departures=n()) %>% arrange(desc(departures))
      colnames(most_common_15_destinations)<-c("DEST","Month", "Count")
      most_common_15_destinations <-most_common_15_destinations %>%  group_by(Month) %>%  top_n(n=15, wt = Count)
      most_common_15_destinations$DEST <- rep(most_common_15_destinations$DEST)
      most_common_15_destinations$Month <- month.abb[most_common_15_destinations$Month]
      most_common_15_destinations$Month <- factor(most_common_15_destinations$Month, levels = month.abb)
      
      ggplot(most_common_15_destinations, aes(reorder(DEST, Count), Count, color=DEST), fill=getPalette(colourCount)) +
        geom_point(stat = "identity") +
        facet_wrap(~ Month)
      
      
    }
  )
  
  output$Top15DestAirportsMdwChart<- renderPlot(
    {
      OrdDep <- subset(allData2, ORIGIN== "MDW")
      
      most_common_15_destinations <- group_by(OrdDep,DEST,month(OrdDep$ARR_TIME_new) )  %>% select(ORIGIN) %>% filter(ORIGIN == "MDW") %>% summarise(departures=n()) %>% arrange(desc(departures))
      colnames(most_common_15_destinations)<-c("DEST","Month", "Count")
      most_common_15_destinations <-most_common_15_destinations %>%  group_by(Month) %>%  top_n(n=15, wt = Count)
      most_common_15_destinations$DEST <- rep(most_common_15_destinations$DEST)
      most_common_15_destinations$Month <- month.abb[most_common_15_destinations$Month]
      
      
      ggplot(most_common_15_destinations, aes(reorder(DEST, Count), Count, color=DEST), fill=getPalette(colourCount)) +
        geom_point(stat = "identity") +
        facet_wrap(~ Month)
      
      
    }
  )
  
  
  
  
  output$NumAndTypeOfDelayChangeMDW <- renderPlot({
    
    data <- subset(allData2, DEST == "MDW" | ORIGIN == "MDW" )
    
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    colnames(cd)<-c("month", "count")
    cd$type = "Carrier Delay"
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    colnames(wd)<-c("month", "count")
    wd$type = "Weather Delay"
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(NAS_DELAY) %>% summarise(count=n())
    colnames(nd)<-c("month", "count")
    nd$type = "NAS Delay"
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    colnames(sd)<-c("month", "count")
    sd$type = "Security Delay"
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    colnames(lad)<-c("month", "count")
    lad$type = "Late Aircraft Delay"
    
    #join all delay types into one table
    
    delay_data <- rbind(cd, wd, nd, sd, lad)
    delay_data$month <- month.abb[delay_data$month]
    delay_data$month <- factor(delay_data$month, levels = month.abb)
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=month )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
    
  })
  
  
  output$NumAndTypeOfDelayChangeORD <- renderPlot({
    
    data <- subset(allData2, DEST == "ORD" | ORIGIN == "ORD" )
    
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    colnames(cd)<-c("month", "count")
    cd$type = "Carrier Delay"
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    colnames(wd)<-c("month", "count")
    wd$type = "Weather Delay"
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(NAS_DELAY) %>% summarise(count=n())
    colnames(nd)<-c("month", "count")
    nd$type = "NAS Delay"
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    colnames(sd)<-c("month", "count")
    sd$type = "Security Delay"
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    colnames(lad)<-c("month", "count")
    lad$type = "Late Aircraft Delay"
    
    #join all delay types into one table
    
    delay_data <- rbind(cd, wd, nd, sd, lad)
    delay_data$month <- month.abb[delay_data$month]
    delay_data$month <- factor(delay_data$month, levels = month.abb)
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=month )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
    
  })
  
  output$NumAndTypeOfDelayChangeStack <- renderPlot({
    
    #NAS_DELAY !=0 | LATE_AIRCRAFT_DELAY != 0
    
    #get all flights with delays
    temp <- subset(allData2, CARRIER_DELAY != 0)
    cd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    colnames(cd)<-c("month", "count")
    cd$type = "Carrier Delay"
    
    temp <- subset(allData2, WEATHER_DELAY != 0)
    wd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    colnames(wd)<-c("month", "count")
    wd$type = "Weather Delay"
    
    temp <- subset(allData2, NAS_DELAY != 0)
    nd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(NAS_DELAY) %>% summarise(count=n())
    colnames(nd)<-c("month", "count")
    nd$type = "NAS Delay"
    
    temp <- subset(allData2, SECURITY_DELAY != 0)
    sd <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    colnames(sd)<-c("month", "count")
    sd$type = "Security Delay"
    
    temp <- subset(allData2, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, month(temp$ARR_TIME_new) )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    colnames(lad)<-c("month", "count")
    lad$type = "Late Aircraft Delay"
    
    #join all delay types into one table
    
    delay_data <- rbind(cd, wd, nd, sd, lad)
    
    #stacked Area
    ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
      geom_area()
    
  })
  
  output$MapDeparturePercent <- renderPlotly({
    
    
    # We start in illinois
    temp <- subset(allData2, ORIGIN == "ORD" | ORIGIN == "MDW" )
    #figure out where people are going  DEST_STATE_NM
    dest <- group_by(temp, DEST_STATE_NM ) %>% summarise(count=n())
    colnames(dest) <- c("State", "CountDest")
    x <- dest$State
    dest$code<- state.abb[match(x,state.name)]
    dest$percent <- percent(dest$CountDest/sum(dest$CountDest))
    
    dest$hover <- with(dest, paste(State, '<br>', "Total Visits", CountDest , "<br>",
                                   "Percentage of total Visitors", percent))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(dest, locationmode = 'USA-states') %>%
      add_trace(
        z = ~CountDest, text = ~hover, locations = ~code,
        color = ~CountDest, colors = 'Purples'
      ) %>%
      colorbar(title = "Trips") %>%
      layout(
        title = 'Most Common US Destinations from Illinois<br>(Hover for breakdown and percent of travel)',
        geo = g
      )
    
  })
  
  
  output$MapArrivalePercent <- renderPlotly({
    
    
    # We start in illinois
    temp <- subset(allData2, DEST == "ORD" | DEST == "MDW" )
    #figure out where people are going  DEST_STATE_NM
    arriv <- group_by(temp, ORIGIN_STATE_NM ) %>% summarise(count=n())
    colnames(arriv) <- c("State", "CountDest")
    x <- arriv$State
    arriv$code<- state.abb[match(x,state.name)]
    arriv$percent <- percent(arriv$CountDest/sum(arriv$CountDest))
    
    arriv$hover <- with(arriv, paste(State, '<br>', "Total Visits", CountDest , "<br>",
                                     "Percentage of total Visitors", percent))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(arriv, locationmode = 'USA-states') %>%
      add_trace(
        z = ~CountDest, text = ~hover, locations = ~code,
        color = ~CountDest, colors = 'Purples'
      ) %>%
      colorbar(title = "Trips") %>%
      layout(
        title = 'Most Common US Destinations from Illinois<br>(Hover for breakdown and percent of travel)',
        geo = g
      )
    
  })
  
  
  output$AirlineDepartureHRMDW <- renderPlot({
    data2 <- subset(allData2, (ORIGIN == "MDW" | DEST == "MDW") )
    data2 <- subset(data2, input$Airline ==  CARRIER)
    
    if(!(is.data.frame(data2) && nrow(data2)==0 ))
    {
      dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "MDW" ) %>% summarise(count=n())
      colnames(dep_hour)[1]<-"hour"
      dep_hour$type = "Departure"
      
      arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
      arr_hour$type = "Arrival"
      
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2$hour<-switch_hour(data2$hour)
      
      
      #set a factor for time baised on what clock we are in
      data2$hour <- set_time_factor(data2$hour)
      data2 <- as.data.frame(data2)
      
      empty <- emptyarray("T")
      missingTimes <- empty$hour[!empty$hour %in% data2$hour]
      empty <- empty[is.element(empty$hour, missingTimes),]
      data2 <- merge(data2, empty, all= TRUE)
      
      
      data <- data2
      adj <- -0.3
      ggplot(data, aes(x=hour,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Hour", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)
    }
    else
    {
      data <- emptyarray("T")
      adj <- 15
      ggplot(data, aes(x=hour,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Hour", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)
    }#end Else statement
    
  })
  
  output$AirlineDepartureHrORD <- renderPlot({
    
    data2 <- subset(allData2, (ORIGIN == "ORD" | DEST == "ORD") )
    data2 <- subset(data2, input$Airline ==  CARRIER)
    
    if(!(is.data.frame(data2) && nrow(data2)==0 ))
    {
      dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "ORD" ) %>% summarise(count=n())
      colnames(dep_hour)[1]<-"hour"
      dep_hour$type = "Departure"
      
      arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
      arr_hour$type = "Arrival"
      
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2$hour<-switch_hour(data2$hour)
      
      
      #set a factor for time baised on what clock we are in
      data2$hour <- set_time_factor(data2$hour)
      data2 <- as.data.frame(data2)
      
      empty <- emptyarray("T")
      missingTimes <- empty$hour[!empty$hour %in% data2$hour]
      empty <- empty[is.element(empty$hour, missingTimes),]
      data2 <- merge(data2, empty, all= TRUE)
      
      
      data <- data2
      adj <- -0.3
      ggplot(data, aes(x=hour,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Hour", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)
    }
    else
    {
      data <- emptyarray("T")
      adj <- 15
      ggplot(data, aes(x=hour,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Hour", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)+
        ylim(0, 700)
    }#end Else statement
    
  })
  
  output$AirlineArrivalsMonthORD <- renderPlot({
    data2 <- subset(allData2, (ORIGIN == "ORD" | DEST == "ORD") )
    data2 <- subset(data2, input$Airline ==  CARRIER)
    
    if(!(is.data.frame(data2) && nrow(data2)==0 ))
    {
      dep_hour <- group_by(data2,month(data2$ARR_TIME_new))  %>% select(ORIGIN) %>% filter(ORIGIN== "ORD" ) %>% summarise(count=n())
      colnames(dep_hour)[1]<-"month"
      dep_hour$type = "Departure"
      
      arr_hour <- group_by(data2,month(data2$ARR_TIME_new))  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
      colnames(arr_hour)[1]<-"month"
      arr_hour$type = "Arrival"
      
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      
      data2 <- as.data.frame(data2)
      data2$month <- month.abb[data2$month]
      data2$month <- factor(data2$month, levels = month.abb)
      
      empty <- emptyarray("M")
      missing <- empty$month[!empty$month %in% data2$month]
      empty <- empty[is.element(empty$month, missing),]
      data2 <- merge(data2, empty, all= TRUE)
      
      data <- data2
      adj <- -0.3
      ggplot(data, aes(x=month,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Month", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)
    }
    else
    {
      data <- emptyarray("M")
      adj <- 15
      ggplot(data, aes(x=month,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Month", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)+
        ylim(0, 700)
    }#end Else statement
    
  })
  
  
  output$AirlineArrivalsMonthMDW <- renderPlot({
    data2 <- subset(allData2, (ORIGIN == "MDW" | DEST == "MDW") )
    data2 <- subset(data2, input$Airline ==  CARRIER)
    
    if(!(is.data.frame(data2) && nrow(data2)==0 ))
    {
      dep_hour <- group_by(data2,month(data2$ARR_TIME_new))  %>% select(ORIGIN) %>% filter(ORIGIN== "MDW" ) %>% summarise(count=n())
      colnames(dep_hour)[1]<-"month"
      dep_hour$type = "Departure"
      
      arr_hour <- group_by(data2,month(data2$ARR_TIME_new))  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
      colnames(arr_hour)[1]<-"month"
      arr_hour$type = "Arrival"
      
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      
      data2 <- as.data.frame(data2)
      data2$month <- month.abb[data2$month]
      data2$month <- factor(data2$month, levels = month.abb)
      
      empty <- emptyarray("M")
      missing <- empty$month[!empty$month %in% data2$month]
      empty <- empty[is.element(empty$month, missing),]
      data2 <- merge(data2, empty, all= TRUE)
      
      
      
      data <- data2
      adj <- -0.3
      ggplot(data, aes(x=month,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Month", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)
    }
    else
    {
      data <- emptyarray("M")
      adj <- 15
      ggplot(data, aes(x=month,y = count , fill=type)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(x="Month", y = "Number of Flights") +
        scale_fill_manual(values=colorsAD) +
        geom_text(aes(label=count), vjust=adj,
                  position = position_dodge(0.9), size=5.0)+
        ylim(0, 700)
    }#end Else statement
    
  })
  
  output$DateDepArrHrMDW <- renderPlot({
    #Get data for just midway on the selected date
    data2 <- subset(allData2, (ORIGIN == "MDW" | DEST == "MDW") )
    data2 <- subset(data2, input$date ==  date(ARR_TIME_new))
    
    dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "MDW" ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    
    
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data2$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data2 <- merge(data2, empty, all= TRUE)
    
    
    data <- data2
    adj <- -0.3
    ggplot(data, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=adj,
                position = position_dodge(0.9), size=5.0)
    
    
  })
  
  
  output$DateDepArrHrORD <- renderPlot({
    #Get data for just midway on the selected date
    data2 <- subset(allData2, (ORIGIN == "ORD" | DEST == "ORD") )
    data2 <- subset(data2, input$date ==  date(ARR_TIME_new))
    
    dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "ORD" ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    
    
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data2$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data2 <- merge(data2, empty, all= TRUE)
    
    data <- data2
    adj <- -0.3
    ggplot(data, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=adj,
                position = position_dodge(0.9), size=5.0)
    
    
  })
  
  output$DateDelayMDW <- renderPlot({
    data <- subset(allData2, DEST == "MDW" | ORIGIN == "MDW" )
    data <- subset(data, input$date ==  date(ARR_TIME_new))
    
    final <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(final)<-c("hour","count", "type")
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, temp$hour )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(cd) && nrow(cd)==0 ))
    {
      colnames(cd)<-c("hour", "count")
      cd$type = "Carrier Delay"
      final <- merge(final, cd, all= TRUE)
    }
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, temp$hour )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(wd) && nrow(wd)==0 ))
    {
      colnames(wd)<-c("hour", "count")
      wd$type = "Weather Delay"
      final <- merge(final, wd, all= TRUE)
    }
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, temp$hour )  %>% select(NAS_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(nd) && nrow(nd)==0 ))
    {
      colnames(nd)<-c("hour", "count")
      nd$type = "NAS Delay"
      final <- merge(final, nd, all= TRUE)
    }
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, temp$hour )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(sd) && nrow(sd)==0 ))
    {
      colnames(sd)<-c("hour", "count")
      sd$type = "Security Delay"
      final <- merge(final, sd, all= TRUE)
    }
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, temp$hour )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(lad) && nrow(lad)==0 ))
    {
      colnames(lad)<-c("hour", "count")
      lad$type = "Late Aircraft Delay"
      final <- merge(final, lad, all= TRUE)
    }
    
    delay_data <- final
    #join all delay types into one table
    
    delay_data$hour<-switch_hour(delay_data$hour)
    #set a factor for time baised on what clock we are in
    delay_data$hour <- set_time_factor(delay_data$hour)
    
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=hour )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
  })
  
  output$DateDelayORD <- renderPlot({
    data <- subset(allData2, DEST == "ORD" | ORIGIN == "ORD" )
    data <- subset(data, input$date ==  date(ARR_TIME_new))
    
    final <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(final)<-c("hour","count", "type")
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, temp$hour )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(cd) && nrow(cd)==0 ))
    {
      colnames(cd)<-c("hour", "count")
      cd$type = "Carrier Delay"
      final <- merge(final, cd, all= TRUE)
    }
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, temp$hour )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(wd) && nrow(wd)==0 ))
    {
      colnames(wd)<-c("hour", "count")
      wd$type = "Weather Delay"
      final <- merge(final, wd, all= TRUE)
    }
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, temp$hour )  %>% select(NAS_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(nd) && nrow(nd)==0 ))
    {
      colnames(nd)<-c("hour", "count")
      nd$type = "NAS Delay"
      final <- merge(final, nd, all= TRUE)
    }
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, temp$hour )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(sd) && nrow(sd)==0 ))
    {
      colnames(sd)<-c("hour", "count")
      sd$type = "Security Delay"
      final <- merge(final, sd, all= TRUE)
    }
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, temp$hour )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(lad) && nrow(lad)==0 ))
    {
      colnames(lad)<-c("hour", "count")
      lad$type = "Late Aircraft Delay"
      final <- merge(final, lad, all= TRUE)
    }
    
    delay_data <- final
    #join all delay types into one table
    
    delay_data$hour<-switch_hour(delay_data$hour)
    #set a factor for time baised on what clock we are in
    delay_data$hour <- set_time_factor(delay_data$hour)
    
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=hour )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
  })
  
  output$DayDepArrHrMDW <- renderPlot({
    #Get data for just midway on the selected date
    data2 <- subset(allData2, (ORIGIN == "MDW" | DEST == "MDW") )
    data2 <- subset(data2, input$day == weekday)
    
    dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "MDW" ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="MDW") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    
    
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data2$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data2 <- merge(data2, empty, all= TRUE)
    
    
    data <- data2
    adj <- -0.3
    ggplot(data, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=adj,
                position = position_dodge(0.9), size=5.0)
    
    
  })
  
  output$DayDepArrHrORD <- renderPlot({
    #Get data for just midway on the selected date
    data2 <- subset(allData2, (ORIGIN == "ORD" | DEST == "ORD") )
    data2 <- subset(data2, input$day == weekday)
    
    dep_hour <- group_by(data2,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN== "ORD" ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(data2,hour)  %>% select(DEST) %>% filter(DEST=="ORD") %>% summarise(count=n())
    arr_hour$type = "Arrival"
    
    data2 <- merge(dep_hour,arr_hour,all=TRUE)
    data2 <- subset(data2,!is.na(data2$hour))
    data2$hour<-switch_hour(data2$hour)
    
    
    #set a factor for time baised on what clock we are in
    data2$hour <- set_time_factor(data2$hour)
    data2 <- as.data.frame(data2)
    
    empty <- emptyarray("T")
    missingTimes <- empty$hour[!empty$hour %in% data2$hour]
    empty <- empty[is.element(empty$hour, missingTimes),]
    data2 <- merge(data2, empty, all= TRUE)
    
    
    data <- data2
    adj <- -0.3
    ggplot(data, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Hour", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=adj,
                position = position_dodge(0.9), size=5.0)
    
    
  })
  
  output$DayDelayMDW <- renderPlot({
    data <- subset(allData2, DEST == "MDW" | ORIGIN == "MDW" )
    data <- subset(data,input$day == weekday)
    
    final <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(final)<-c("hour","count", "type")
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, temp$hour )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(cd) && nrow(cd)==0 ))
    {
      colnames(cd)<-c("hour", "count")
      cd$type = "Carrier Delay"
      final <- merge(final, cd, all= TRUE)
    }
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, temp$hour )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(wd) && nrow(wd)==0 ))
    {
      colnames(wd)<-c("hour", "count")
      wd$type = "Weather Delay"
      final <- merge(final, wd, all= TRUE)
    }
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, temp$hour )  %>% select(NAS_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(nd) && nrow(nd)==0 ))
    {
      colnames(nd)<-c("hour", "count")
      nd$type = "NAS Delay"
      final <- merge(final, nd, all= TRUE)
    }
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, temp$hour )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(sd) && nrow(sd)==0 ))
    {
      colnames(sd)<-c("hour", "count")
      sd$type = "Security Delay"
      final <- merge(final, sd, all= TRUE)
    }
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, temp$hour )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(lad) && nrow(lad)==0 ))
    {
      colnames(lad)<-c("hour", "count")
      lad$type = "Late Aircraft Delay"
      final <- merge(final, lad, all= TRUE)
    }
    
    delay_data <- final
    #join all delay types into one table
    
    delay_data$hour<-switch_hour(delay_data$hour)
    #set a factor for time baised on what clock we are in
    delay_data$hour <- set_time_factor(delay_data$hour)
    
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=hour )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
  })
  
  output$DayDelayORD <- renderPlot({
    data <- subset(allData2, DEST == "ORD" | ORIGIN == "ORD" )
    data <- subset(data, input$day == weekday)
    
    final <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(final)<-c("hour","count", "type")
    #get all flights with delays
    temp <- subset(data, CARRIER_DELAY != 0)
    cd <- group_by(temp, temp$hour )  %>% select(CARRIER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(cd) && nrow(cd)==0 ))
    {
      colnames(cd)<-c("hour", "count")
      cd$type = "Carrier Delay"
      final <- merge(final, cd, all= TRUE)
    }
    
    temp <- subset(data, WEATHER_DELAY != 0)
    wd <- group_by(temp, temp$hour )  %>% select(WEATHER_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(wd) && nrow(wd)==0 ))
    {
      colnames(wd)<-c("hour", "count")
      wd$type = "Weather Delay"
      final <- merge(final, wd, all= TRUE)
    }
    
    temp <- subset(data, NAS_DELAY != 0)
    nd <- group_by(temp, temp$hour )  %>% select(NAS_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(nd) && nrow(nd)==0 ))
    {
      colnames(nd)<-c("hour", "count")
      nd$type = "NAS Delay"
      final <- merge(final, nd, all= TRUE)
    }
    
    temp <- subset(data, SECURITY_DELAY != 0)
    sd <- group_by(temp, temp$hour )  %>% select(SECURITY_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(sd) && nrow(sd)==0 ))
    {
      colnames(sd)<-c("hour", "count")
      sd$type = "Security Delay"
      final <- merge(final, sd, all= TRUE)
    }
    
    temp <- subset(data, LATE_AIRCRAFT_DELAY != 0)
    lad <- group_by(temp, temp$hour )  %>% select(LATE_AIRCRAFT_DELAY) %>% summarise(count=n())
    if(!(is.data.frame(lad) && nrow(lad)==0 ))
    {
      colnames(lad)<-c("hour", "count")
      lad$type = "Late Aircraft Delay"
      final <- merge(final, lad, all= TRUE)
    }
    
    delay_data <- final
    #join all delay types into one table
    
    delay_data$hour<-switch_hour(delay_data$hour)
    #set a factor for time baised on what clock we are in
    delay_data$hour <- set_time_factor(delay_data$hour)
    
    #stacked Area
    #ggplot(delay_data, aes(x=month, y=count, fill=type)) + 
    #  geom_area()
    
    ggplot(delay_data, aes(x=type, y=hour )) +
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradient(low = colorsLH[1], high = colorsLH[2]) +
      theme(panel.background = element_rect(fill = 'white'))
  })
  
}



shinyApp(ui = ui, server = server)