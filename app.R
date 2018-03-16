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
#Libaries needed to read in data faster.
library(data.table)
library(fasttime)
library(ggridges)
#libaries for the map
library(plotly)


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
# Define UI for application that draws a histogram
ui <- 
  
  dashboardPage( 
    
    dashboardHeader(title = "Learning to Fly"),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("About", tabName="about"),
        menuItem("Arrivals & Departures", tabName="arrivals_departures"),
        menuItem("Arrivals & Departures: Airlines", tabName="arrivals_departures_airlines"),
        menuItem("Delays", tabName="delays"),
        # menuItem("Delays: Date/Week Specific", tabName="delays_date_week"),
        menuItem("Top Airports", tabName="top_airports"),
        menuItem("Settings", tabName="dashboard"),
        menuItem("Map: Arrivals", tabName="map_arrivals")
        # menuItem("Top Airlines", tabName="top_airlines"),
        # menuItem("10 Interesting Days", tabName="interesting_days")
        
      )
      
    ),
    
    dashboardBody(
      
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="custom.css")
      ),
      
      tabItems(
        
        tabItem("dashboard",
                
                fluidRow(
                  box(
                    selectInput("Time", "12 hour am/pm time or 24 hour time ", choices=t, selected = '24 hour'), width=4
                  )
                  
                )
                
        ), #end of tab item
        
        tabItem("about",
                h1("Authors: Yang Hao, Guillermo Rojas Hernandez, Natasha Rice, Siddharth Basu"),
                a("Link to project website", href="https://guillermokrh.github.io/CS-424---Project-2-Website/")
        ),
        
        tabItem("arrivals_departures",
                fluidRow(
                  box(
                    selectInput("arrivals_departures_month", "Select the month to visualize", choices=months, selected = 8), width=3
                  ),
                  box(
                    selectInput("arrivals_departures_airport","Select airport:",choices=loc,selected='MDW'), width=9
                  ),
                  
                  textOutput("arrivals_departures_result")
                ),
                fluidRow(
                  box(title = "Arrivals and Departures by Hour", solidHeader = TRUE, status = "primary", width = 3,
                      
                      dataTableOutput("tab2")
                      
                  ),
                  box( title = "Total number of departures/arrivals by time", solidHeader = TRUE, status = "primary", width = 9,
                       plotOutput("BarByTime")
                  )
                ),
                fluidRow(
                  box(title = "Arrivals and Departures by Weekday", solidHeader = TRUE, status = "primary", width = 3,
                      dataTableOutput("tab3")
                  ),
                  box( title = "Total number of departures/arrivals by Day of Week", solidHeader = TRUE, status = "primary", width = 9,
                       plotOutput("BarByWeekday")
                  )
                ),
                fluidRow(
                  
                  box( title = "Arrivals in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                       plotOutput("HeatArrHrORD")
                  ),
                  box( title = "Departures in O'Hare By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                       plotOutput("HeatDepHrORD")
                  )
                  
                ),
                fluidRow(
                  
                  box( title = "Arrivals in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                       plotOutput("HeatArrHrMdw")
                  ),
                  box( title = "Departures in Midway By Airline and Month", solidHeader = TRUE, status = "primary", width = 6,
                       plotOutput("HeatDepHrMdw")
                  )
                  
                )
                
        ),
        
        tabItem("arrivals_departures_airlines",              
                
                fluidRow(
                  box(
                    selectInput("arrivals_departures_airlines_month", "Select the month to visualize", choices=months, selected = 8)
                  ),
                  box(
                    selectInput("arrivals_departures_airlines_airport", "Select the base airport to visualize", choices=loc, selected = 'MDW')
                  ),
                  
                  textOutput("arrivals_departures_airlines_result")
                  
                ), 
                fluidRow(
                  
                  box(title = "Departures and Arrivals By Airline", solidHeader = TRUE, status = "primary", width = 6,
                      
                      dataTableOutput("tab1")
                      
                  ),
                  box( title = "Departure and Arrival Totals By Airline", solidHeader = TRUE, status = "primary", width = 6,
                       plotOutput("TotalDepAri")
                  )
                  
                ),
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
        ),
        tabItem("delays",
                fluidRow(
                  box(
                    selectInput("delays_month", "Select the month to visualize", choices=months, selected = 4), width=4
                  ),
                  box(
                    selectInput("delays_airport", "Select the base airport to visualize", choices=loc, selected = 'MDW'), width=8
                  )
                ),
                fluidRow (
                  box(title = "Delays Per Hour", solidHeader = TRUE, status = "primary",width = 4,
                      dataTableOutput("tab4")),
                  
                  box( title = "Delays Per Hour By Chosen Airport", solidHeader = TRUE, status = "primary", width = 8,
                       plotOutput("BarDelaybyHour")
                  )
                ),
                fluidRow (
                  box(title = "Delays Change By month", solidHeader = TRUE, status = "primary",width = 6,
                      plotOutput("NumAndTypeOfDelayChange")
                  )
                )
        ),
        tabItem("top_airports",
                fluidRow(
                  box(
                    selectInput("top_airports_month", "Select the month to visualize", choices=months, selected = 4), width=4
                  ),
                  box(
                    selectInput("top_airports_airport", "Select the base airport to visualize", choices=loc, selected = 'MDW'), width=8
                  )
                ),
                fluidRow( 
                  box(title = "15 Most Common Airports ", solidHeader = TRUE, status = "primary",width = 4,
                      dataTableOutput("tab5")),
                  box( title = "15 Most Common Airports", solidHeader = TRUE, status = "primary", width = 8,
                       plotOutput("BarTop10")
                  )
                ),
                fluidRow(
                  box(title = "top 15 airports by month O'Hare", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("Top15DestAirportsOrd")),
                  box(title = "top 15 airports by month Midway", solidHeader = TRUE, status = "primary", width = 6,
                      plotOutput("Top15DestAirportsMdw"))
                  
                )
          ),
        tabItem("map_arrivals",
                #MapDeparturePercent
                box(title = "Percentage of Arrivals from Illinois", solidHeader = TRUE, status = "primary", width = 6,
                plotlyOutput("MapDeparturePercent")
                ),
                box(title = "Percentage of Departures from Illinois", solidHeader = TRUE, status = "primary", width = 6,
                    plotlyOutput("MapArrivalePercent")
                )
        )
        
      ) # end of TabItems 
      
    ), # end of DashboardBody
    skin = c("black")
  ) # end of DashboardPage



server <- function(input, output) {
  
  
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  
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
  
  output$result <- renderText({
    paste("You chose month:", input$Month, "and airport:", input$Airport)
  })
  
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
  
  ####### Arrivals & Departures: Airlines Dashboard Tab
  # Table: Departure and Arrival Totals by Airline
  output$tab1 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
      mdw_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="MDW") %>% summarise(mdw_arrival=n())
      mdw_departure <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="MDW") %>% summarise(mdw_departure=n())
      ord_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST=="ORD") %>% summarise(ord_arrival=n())
      ord_departure <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN=="ORD") %>% summarise(ord_departure=n())
      mdw_data <- merge(mdw_arrival,mdw_departure, all=TRUE)
      ord_data <- merge(ord_arrival,ord_departure, all=TRUE)
      data1 <- merge(mdw_data, ord_data, all=TRUE)
      data1[is.na(data1)] <- 0
      data1 <- as.data.frame(data1)
      data1
    },
    options = list(pageLength = 11)
    )
    
  )
  
  # Bar Chart: Departure and Arrival Totals by Arline
  output$TotalDepAri<- renderPlot({
    
    justOneMonthReactive <- arrivalsDeparturesAirlinesMonth()
    n_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST==input$arrivals_departures_airlines_airport) %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_dep <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN==input$arrivals_departures_airlines_airport) %>% summarise(Count=n())
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
                position = position_dodge(0.9), size=3.5)
    
  })
  
  
  ########### Arrivals and Departures Tab
  # Table: The total number of departures and total number of arrivals for each hour of the day across that month
  output$tab2 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport ) %>% summarise(count=n())
      #dep_hour$type = "Departure"
      arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(count=n())
      #arr_hour$type = "Arrival"
      colnames(dep_hour)<-c("hour","number_dep")
      colnames(arr_hour)<-c("hour","number_arr")
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2$hour<-switch_hour(data2$hour)
      #set a factor for time baised on what clock we are in
      #data2$hour <- set_time_factor(data2$hour)
      data2 <- as.data.frame(data2)
      
      data2
      
    },
    options = list(pageLength = 8)
    )
    
  )
  
  # Bar Graph: Arrivals and Departures by Hour of the Day 
  
  output$BarByTime <- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport ) %>% summarise(count=n())
    colnames(dep_hour)[1]<-"hour"
    dep_hour$type = "Departure"
    
    arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(count=n())
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
                position = position_dodge(0.9), size=3.5)
    
  })
  
  
  # Table: The total number of departures and total number of arrivals for each day of the week across that month
  output$tab3 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- arrivalsDeparturesMonth()
      
      dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport ) %>% summarise(number_dep=n())
      arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(number_arrival=n())
      colnames(dep_weekday)<-c("weekday","number_dep")
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
  output$BarByWeekday<- renderPlot({
    justOneMonthReactive <- arrivalsDeparturesMonth()
    
    dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN==input$arrivals_departures_airport) %>% summarise(Count=n())
    dep_weekday$type = "Departure"
    arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST==input$arrivals_departures_airport) %>% summarise(Count=n())
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
                position = position_dodge(0.9), size=3.5)
    
  })
  
  
  
  
  
  ########### Delays Dashboard Tab
  # Table: Delays for Each Hour
  output$tab4<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- delaysMonth()
      
      #MDW_delay_day_hour <- group_by(tem3,day,hour)  %>% select(DEST_AIRPORT_ID,ORIGIN_AIRPORT_ID,ARR_DELAY,DEP_DELAY) %>% filter((ARR_DELAY>0 | DEP_DELAY >0)&&(ORIGIN_AIRPORT_ID==13232 | DEST_AIRPORT_ID == 13232) ) %>% summarise(number_arrival_delay=n())
      delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == input$delays_airport) %>% summarise(arrival_delays=n())
      delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN ==input$delays_airport & DEP_DELAY <0 ) %>% summarise(departure_delays=n())
      colnames(delay_dep_hour)<-c("hour","number_dep_delay")
      data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
      data4 <- subset(data4,!is.na(data4$hour))
      data4[is.na(data4)] <- 0
      data4$hour<-switch_hour(data4$hour)
      data4 <- as.data.frame(data4)
      
      data4$total <- data4$number_dep_delay + data4$arrival_delays
      data4$percent <- percent(data4$total/sum(data4$total))
      #data4$total <- data4$arrival_delays +data4$departure_delays
      #data4$percent <- percent(data4$total/sum(data4$total))
      
      data4
    },
    options = list(pageLength = 24))
  )
  
  # Bar Chart: Delays for Each Hour
  output$BarDelaybyHour<- renderPlot({
    justOneMonthReactive <- delaysMonth()
    
    #MDW_delay_day_hour <- group_by(tem3,day,hour)  %>% select(DEST_AIRPORT_ID,ORIGIN_AIRPORT_ID,ARR_DELAY,DEP_DELAY) %>% filter((ARR_DELAY>0 | DEP_DELAY >0)&&(ORIGIN_AIRPORT_ID==13232 | DEST_AIRPORT_ID == 13232) ) %>% summarise(number_arrival_delay=n())
    delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == input$delays_airport ) %>% summarise(count=n())
    delay_arr_hour$type = "Departure"
    delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN ==input$delays_airport & DEP_DELAY <0 ) %>% summarise(count=n())
    delay_dep_hour$type = "Arrival"
    colnames(delay_dep_hour)<-c("hour","count", "type")
    data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
    data4 <- subset(data4,!is.na(data4$hour))
    data4[is.na(data4)] <- 0
    data4$hour<-switch_hour(data4$hour)
    data4$hour <- set_time_factor(data4$hour)
    data4 <- as.data.frame(data4)
    
    
    
    ggplot(data4, aes(x=hour,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Time", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=3.5)
  })
  
  ######### Top Airports Dashboard Tab
  # Table: the number of flights for the most common 15 destination and airports airports for selected airport
  output$tab5<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- topAirportsMonth()
      
      most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == input$top_airports_airport) %>% summarise(departures=n()) %>% arrange(desc(departures)) %>% top_n(15)
      
      # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
      most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == input$top_airports_airport) %>% summarise(arrivals=n()) %>% arrange(desc(arrivals)) %>% top_n(15)
      most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$arrivals)) 
      most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$departures))
      
      data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
      data5
      
    })
  )
  
  # Bar Chart: 15 most common destination and arrival aiports for selected airports
  output$BarTop10 <- renderPlot({
    justOneMonthReactive <- topAirportsMonth()
    
    most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == input$top_airports_airport) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    
    # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
    most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == input$top_airports_airport ) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
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
                position = position_dodge(0.9), size=3.5)
  })
  
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
  
  
  
  
  output$NumAndTypeOfDelayChange <- renderPlot({
    
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
    delay_data$month <- month.abb[delay_data$month]
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
  
}



shinyApp(ui = ui, server = server)