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

# assume all of the tsv files in this directory are data of the same kind that I want to visualize

ColNames<- c("FL_DATE","AIRLINE_ID","CARRIER","ORIGIN_AIRPORT_ID","ORIGIN","ORIGIN_CITY_NAME","ORIGIN_STATE_NM","DEST_AIRPORT_ID","DEST_CITY_NAME","DEST_STATE_NM","DEP_TIME","DEP_DELAY","DEP_DELAY_NEW","ARR_TIME","ARR_DELAY","ARR_DELAY_NEW","CANCELLED","CANCELLATION_CODE","DIVERTED","ACTUAL_ELAPSED_TIME","FLIGHTS","DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")



airport = list.files(pattern = "*.cleaned.csv")
allData <- lapply(airport, read.table, header = FALSE,sep='\t')
allData2 <- do.call(rbind, allData)
names(allData2)<-ColNames

allData2$ARR_TIME <- sprintf("%04d", allData2$ARR_TIME)
allData2$ARR_TIME_new <- as.POSIXct(paste(allData2$FL_DATE, allData2$ARR_TIME), format = "%Y-%m-%d %H%M" )
allData2$DEP_TIME_new <- as.POSIXct(paste(allData2$FL_DATE, allData2$DEP_TIME), format = "%Y-%m-%d %H%M" )

allData2$hour <- lubridate::hour( allData2$ARR_TIME_new)
#allData2$hour <-paste(allData2$hour,":00",sep = "")

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

months <- c(1:12)
t<-c("24 hour","12 hour am/pm")
loc <- c('MDW','ORD')
# Define UI for application that draws a histogram
ui <- 
  
  dashboardPage( 
  
  dashboardHeader(title = "CS 424: Project 2"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Dashboard", tabName="dashboard"),
      
      menuItem("About", tabName="about"),
      menuItem("Arrivals & Departures", tabName="arrivals_departures"),
      menuItem("Arrivals & Departures: Airlines", tabName="arrivals_departures_airlines"),
      menuItem("Delays", tabName="delays"),
      menuItem("Delays: Date/Week Specific", tabName="delays_date_week"),
      menuItem("Top Airports", tabName="top_airports"),
      menuItem("Top Airlines", tabName="top_airlines"),
      menuItem("10 Interesting Days", tabName="interesting_days")
      
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
                  selectInput("Time", "12 hour am/pm time or 24 hour time ", choices=t, selected = '24 hour')
                ),
                box(
                  selectInput("Month", "Select the month to visualize", choices=months, selected = 8)
                ),
                box(
                  selectInput("Airport","Select airport:",choices=loc,selected='MDW')),
                
                textOutput("result")
                
              ), 
              fluidRow(
                box(title = "the total number of departures and total number of arrivals for each hour", solidHeader = TRUE, status = "primary", width = 12,
                    
                    dataTableOutput("tab2")
                    
                ),
                box(title = "the total number of departures and total number of arrivals for each weekday", solidHeader = TRUE, status = "primary", width = 12,
                    
                    dataTableOutput("tab3")
                    
                ),
                box(title = "delay for each hour", solidHeader = TRUE, status = "primary",width = 12,
                    dataTableOutput("tab4")),
                box(title = "table showing the number of flights for the most common 15 airports ", solidHeader = TRUE, status = "primary",width = 12,
                    dataTableOutput("tab5"))
              ),
              fluidRow(
                box( title = "Total number of departures/arrivals", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("TotalDepAri")
                )
              ),
              fluidRow(
                box( title = "Total number of departures/arrivals by time", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("BarByTime")
                )
              ), 
              fluidRow(
                box( title = "Total number of departures/arrivals by Day of Week", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("BarByWeekday")
                )
              ), 
              fluidRow(
                box( title = "Total number of delay by hour", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("BarDelaybyHour")
                )
              ), 
              fluidRow(
                box( title = "Top 10 Airports", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("BarTop10")
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
        
        a("Link to project website", href="https://guillermokrh.github.io/CS-424---Project-2-Website/")
        
      ),
      
      tabItem("arrivals_departures",
                fluidRow(
                  
                  box(title = "total number of departures and arrivals for airlines", solidHeader = TRUE, status = "primary", width = 12,
                      
                      dataTableOutput("tab1")
                      
                  )
                  
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
  
  justOneMonthReactive <- reactive({subset(allData2, month(allData2$ARR_TIME_new) == input$Month)})
  output$result <- renderText({
    paste("You chose Month:", input$Month, "and Airport:", input$Airport)
  })
  
  switch_hour<- function(x){
    c <- x
    #ifelse(input$Time=="24 hour", c<-paste(c$hour,":00",sep=""), ifelse(c<12, paste(c,":00 AM",sep=""),paste(cc-12,":00 PM",sep = "")))
    if (input$Time!="24 hour"){
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
    
    temp <- unique(time)
    time <- factor(time, levels = temp)
    
    time
  }
  
  #table and chart showing the total number of departures and total number of arrivals for all of the domestic airlines
  output$tab1 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- justOneMonthReactive()
      n_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST==input$Airport) %>% summarise(number_arrival=n())
      n_dep <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN==input$Airport) %>% summarise(number_dep=n())
      data1 <- merge(n_arrival,n_dep,all=TRUE)
      data1[is.na(data1)] <- 0
      data1 <- as.data.frame(data1)
      data1
    },
    options = list(pageLength = 8)
    )
    
  )
  
  # table and chart showing the total number of departures and total number of arrivals for each hour of the day across that month (i.e. how many from 9am to 10am summed over every day of that month)
  
  output$tab2 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- justOneMonthReactive()
      
      
      dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(ORIGIN) %>% filter(ORIGIN==input$Airport ) %>% summarise(number_dep=n())
      arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST==input$Airport) %>% summarise(number_arrival=n())
      colnames(dep_hour)<-c("hour","number_dep")
      data2 <- merge(dep_hour,arr_hour,all=TRUE)
      data2 <- subset(data2,!is.na(data2$hour))
      data2[is.na(data2)] <- 0
      data2$hour<-switch_hour(data2$hour)
      data2 <- as.data.frame(data2)
      
      data2
      
    },
    options = list(pageLength = 8)
    )
    
  )
  
  # the total number of departures and total number of arrivals for each day of the week across that month
  
  output$tab3 <-DT::renderDataTable(
    DT::datatable({
      justOneMonthReactive <- justOneMonthReactive()
      
      dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN==input$Airport ) %>% summarise(number_dep=n())
      arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST==input$Airport) %>% summarise(number_arrival=n())
      colnames(dep_weekday)<-c("weekday","number_dep")
      data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
      data3 <- subset(data3,!is.na(data3$weekday))
      data3[is.na(data3)] <- 0
      data3 <- as.data.frame(data3)
      #reodder days of week
      data3$weekday <- factor(data3$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      data3
      
    },
    options = list(pageLength = 8, order = list(list(1, 'asc')))
    )
    
  )
  #delay for each hour
  
  output$tab4<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- justOneMonthReactive()
      
      #MDW_delay_day_hour <- group_by(tem3,day,hour)  %>% select(DEST_AIRPORT_ID,ORIGIN_AIRPORT_ID,ARR_DELAY,DEP_DELAY) %>% filter((ARR_DELAY>0 | DEP_DELAY >0)&&(ORIGIN_AIRPORT_ID==13232 | DEST_AIRPORT_ID == 13232) ) %>% summarise(number_arrival_delay=n())
      delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == input$Airport ) %>% summarise(number_arr_delay=n())
      delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN ==input$Airport & DEP_DELAY <0 ) %>% summarise(number_dep_delay=n())
      colnames(delay_dep_hour)<-c("hour","number_dep_delay")
      data4 <- merge(delay_arr_hour,delay_dep_hour,all=TRUE)
      data4 <- subset(data4,!is.na(data4$hour))
      data4[is.na(data4)] <- 0
      data4$hour<-switch_hour(data4$hour)
      data4 <- as.data.frame(data4)
      data4$number_delay <- data4$number_arr_delay +data4$number_dep_delay
      
      data4$percent_delay <- percent(data4$number_delay/sum(data4$number_delay))
      
      data4
    })
  )
  
  # table showing the number of flights for the most common 15 destination and airports airports for selected airport
  output$tab5<- DT::renderDataTable(
    DT::datatable({
      
      justOneMonthReactive <- justOneMonthReactive()
      
      most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == input$Airport) %>% summarise(number_of_dest_flights=n()) %>% arrange(desc(number_of_dest_flights)) %>% top_n(15)
      
      
      
      # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
      most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == input$Airport ) %>% summarise(number_of_arr_flights=n()) %>% arrange(desc(number_of_arr_flights)) %>% top_n(15)
      most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$number_of_arr_flights)) 
      most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$number_of_dest_flights))
      
      data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
      data5
      
    })
  )
  
  output$TotalDepAri<- renderPlot({
    
    justOneMonthReactive <- justOneMonthReactive()
    n_arrival <- group_by(justOneMonthReactive,CARRIER)  %>% select(DEST,CARRIER ) %>% filter(DEST==input$Airport) %>% summarise(Count=n())
    n_arrival$type = "Departure"
    n_dep <- group_by(justOneMonthReactive,CARRIER)  %>% select(ORIGIN,CARRIER) %>% filter(ORIGIN==input$Airport) %>% summarise(Count=n())
    n_dep$type = "Arrival"
    data1 <- merge(n_arrival,n_dep,all=TRUE)
    data1[is.na(data1)] <- 0
    data1 <- as.data.frame(data1)
    
    
    ggplot(data1, aes(x=CARRIER,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Ariline", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=3.5)
    
  })
  
  output$BarByTime <- renderPlot({
    justOneMonthReactive <- justOneMonthReactive()
    
    
    dep_hour <- group_by(justOneMonthReactive,hour)  %>% select(ORIGIN) %>% filter(ORIGIN==input$Airport ) %>% summarise(count=n())
    dep_hour$type = "Departure"
    arr_hour <- group_by(justOneMonthReactive,hour)  %>% select(DEST) %>% filter(DEST==input$Airport) %>% summarise(count=n())
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
  
  output$BarByWeekday<- renderPlot({
    justOneMonthReactive <- justOneMonthReactive()
    
    dep_weekday <- group_by(justOneMonthReactive,weekday_dep)  %>% select(ORIGIN_AIRPORT_ID,ORIGIN) %>% filter(ORIGIN==input$Airport ) %>% summarise(Count=n())
    dep_weekday$type = "Departure"
    arr_weekday <- group_by(justOneMonthReactive,weekday)  %>% select(DEST) %>% filter(DEST==input$Airport) %>% summarise(Count=n())
    arr_weekday$type = "Arrival"
    colnames(dep_weekday)<-c("weekday","Count", "type")
    data3 <- merge(dep_weekday,arr_weekday,all=TRUE)
    data3 <- subset(data3,!is.na(data3$weekday))
    data3[is.na(data3)] <- 0
    data3 <- as.data.frame(data3)
    data3$weekday <- factor(data3$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    
    ggplot(data3, aes(x=weekday,y = Count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Day", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=Count), vjust=-0.3,
                position = position_dodge(0.9), size=3.5)
    
  })
  
  output$BarDelaybyHour<- renderPlot({
    justOneMonthReactive <- justOneMonthReactive()
    
    #MDW_delay_day_hour <- group_by(tem3,day,hour)  %>% select(DEST_AIRPORT_ID,ORIGIN_AIRPORT_ID,ARR_DELAY,DEP_DELAY) %>% filter((ARR_DELAY>0 | DEP_DELAY >0)&&(ORIGIN_AIRPORT_ID==13232 | DEST_AIRPORT_ID == 13232) ) %>% summarise(number_arrival_delay=n())
    delay_arr_hour <- group_by(justOneMonthReactive,hour) %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ARR_DELAY<0 & DEST == input$Airport ) %>% summarise(count=n())
    delay_arr_hour$type = "Departure"
    delay_dep_hour <- group_by(justOneMonthReactive,hour_dep)  %>% select(DEST,ORIGIN,ARR_DELAY,DEP_DELAY) %>% filter(ORIGIN ==input$Airport & DEP_DELAY <0 ) %>% summarise(count=n())
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
  
  
  output$BarTop10 <- renderPlot({
    justOneMonthReactive <- justOneMonthReactive()
    
    most_common_15_destinations <- group_by(justOneMonthReactive,DEST)  %>% select(ORIGIN) %>% filter(ORIGIN == input$Airport) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    
    
    
    # table showing the number of flights for the most common 15 arrival airports (depart: other airport, arrive: MDW) 
    most_common_15_arrivals <- group_by(justOneMonthReactive,ORIGIN)  %>% select(DEST) %>% filter(DEST == input$Airport ) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(15)
    most_common_15_arrivals$Rank <- dense_rank(desc(most_common_15_arrivals$count)) 
    most_common_15_destinations$Rank <- dense_rank(desc(most_common_15_destinations$count))
    
    most_common_15_destinations$type = "Departure"
    most_common_15_arrivals$type = "Arrival"
    
    colnames(most_common_15_destinations)[colnames(most_common_15_destinations)=="DEST"] <- "LOC"
    colnames(most_common_15_arrivals)[colnames(most_common_15_arrivals)=="ORIGIN"] <- "LOC"
    
    #cheap fix- there is a 16th airport in the dataset so we'll remove it.
    most_common_15_destinations <- most_common_15_destinations[-c(16), ]
    data5 <- as.data.frame(merge(most_common_15_arrivals,most_common_15_destinations,all=TRUE))
    
    data5$LOC <- reorder(data5$LOC, -data5$count)
    
    
    ggplot(data5, aes(x= LOC,y = count , fill=type)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(x="Airport", y = "Number of Flights") +
      scale_fill_manual(values=colorsAD) +
      geom_text(aes(label=count), vjust=-0.3,
                position = position_dodge(0.9), size=3.5)
  })
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
