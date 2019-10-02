library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(lubridate)

# import data
DailyData       <- readRDS('DailyData.rds')
DailyFC         <- readRDS('DailyFC.rds')
MonthlyCost_FC  <- readRDS('MonthlyCost.rds')
MonthlyCost_FC  <- arrange(MonthlyCost_FC,month)

# import fake data
ShinyData <- readRDS('data_ready.rds')

# Create fake data per device
MicroData                 <- ShinyData %>% select( date, Kitchen_avg)
MicroData$Device          <- "Microwave"
MicroData$Kitchen_avg     <- MicroData$Kitchen_avg*0.6
MicroData                 <- rename(MicroData, device_usage = Kitchen_avg)

OvenData                  <- ShinyData %>% select( date, Kitchen_avg)
OvenData$Device           <- "Oven"
OvenData$Kitchen_avg      <- OvenData$Kitchen_avg*0.25
OvenData                  <- rename(OvenData, device_usage = Kitchen_avg)

DishData                  <- ShinyData %>% select( date, Kitchen_avg)
DishData$Device           <- "Dishwasher"
DishData$Kitchen_avg      <- DishData$Kitchen_avg*0.15
DishData                  <- rename(DishData, device_usage = Kitchen_avg)

WashingData               <- ShinyData %>% select( date, Laundry_avg)
WashingData$Device        <- "Washingmachine"
WashingData$Laundry_avg   <- WashingData$Laundry_avg*0.6
WashingData               <- rename(WashingData, device_usage = Laundry_avg)

DrierData                 <- ShinyData %>% select( date, Laundry_avg)
DrierData$Device          <- "Tumble-drier"
DrierData$Laundry_avg     <- DrierData$Laundry_avg*0.25
DrierData                 <- rename(DrierData, device_usage = Laundry_avg)

DeviceData <- rbind(MicroData,OvenData,DishData, WashingData, DrierData)

#create fake messages
message1 <- messageItem(from = "Energy Company",
                        message = "You will be billed Oct 28",
                        time = "2014-12-01")
message2 <- messageItem(from = "Regional Home Developer",
                        message = "Maintenance to be scheduled",
                        icon = icon("life-ring"),
                        time = "2014-12-01")
msglist <- list(message1, message2)

#create fake notifications
notif1 <- notificationItem(text = "tip: reduce standby mode consumption",
                           icon("clock-o"),
                           status = "success")
notif2 <- notificationItem(text = "Raise in monthly cost expected",
                           icon = icon("exclamation-triangle"),
                           status = "warning")
notiflist <- list(notif1, notif2)



ui <- dashboardPage( skin = "purple",
                     
  dashboardHeader(title = "Energy consumption dashboard",
                  dropdownMenuOutput("messageDropdown"),
                  dropdownMenuOutput("notificationDropdown")
    ), #close dashboarHeader
                  
  dashboardSidebar(
    sidebarMenu(menuItem("Dashboard", 
                         tabName = "dashboard", 
                         icon = icon("dashboard")),
                menuItem("Devices", 
                         tabName = "devices", 
                         icon = icon("bar-chart-o")),
                menuItem("Forecast", 
                         tabName = "forecast", 
                         icon = icon("database"),
                         badgeLabel = "tip", 
                         badgeColor = "orange")
    )              
  ), #close dashboardSideBar
                  
  dashboardBody(
    body <- dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard", 
                selected = T,
                h3("Energy consumption for selected period"),
                fluidRow(
                  box(dateRangeInput("daterange",
                                label="select period",
                                start="2010-10-01",
                                end="2010-10-31",
                                min="2007-01-01", 
                                max="2010-11-26"),
                      width=4,
                      status = "warning"),
                  infoBoxOutput("TotalKWH", 
                      width=3),
                  infoBoxOutput("TotalAmount", 
                      width=3),
                  box(imageOutput("myImage"),
                      width=2,
                      height = 90,
                      status= "success")
                ),
                hr(),
                box(plotOutput(outputId = "energydata"), 
                    width=6,
                    status = "primary"),
                box(plotOutput(outputId = "deviceplot"), 
                    width=6,
                    status = "primary")
        ),
        tabItem(tabName = "devices",
                h3("Device info")
        ),
        tabItem(tabName = "forecast",
                h3("Forecasts"),
                fluidRow(
                  box(plotOutput(outputId = "monthlyBarPlot"), 
                      width=6,
                      status = "primary",
                      height=400),
                  box(title="Expected Cost", 
                      status = "primary",
                      tableOutput("MonthFC"), 
                      width=6)
                ),
                hr(),
                fluidRow(
                  box(plotOutput(outputId = "forecastLinePlot"), 
                      width=6,
                      status = "primary"),
                  box(radioButtons("plotPeriod", "Select period:",
                                 c("This month" = "tm",
                                   "This quarter" = "tq",
                                   "This year" = "ty")), 
                      width=6,
                      status ="warning")
                )
        ) # close tabitem
      )   # close tabitems
    )     # close dashboardbody2
  )       # close dashboardbody1
)         # close dashboard page


server <- function(input, output) {
  
  # select data based on user inputs: Daily Data on Dashboard
  SelectedData <- reactive({
    DailyData %>% 
      filter(Date >input$daterange[1] & Date < input$daterange[2])
  })
 
  # select data based on user inputs: Device Data on Dashboard
  SelectedDeviceData <- reactive({
    DeviceData %>% 
      filter(date >input$daterange[1] & date < input$daterange[2])
  })
  
  # select data based on user inputs: daily Forecast
  SelectedForecastData <-reactive({
    #selection based on radio button (this month, quarter or year)
    ifelse(input$plotPeriod=="tm", 
          PlotFC <- DailyFC %>% 
                      filter(Date>="2010-11-01" & Date <="2010-11-30"),
          ifelse(input$plotPeriod=="tq",
                PlotFC <- DailyFC %>% 
                            filter(Date>="2010-10-01" & Date <="2010-12-31"),
                PlotFC <- DailyFC))
    # convert data to representable format
    PlotFC$Month <- month(ymd(PlotFC$Date), 
                          label = TRUE, 
                          abbr = TRUE)              # use monthname labels in graph
    PlotFC$Active_whm <- ifelse(PlotFC$Active_whm==0, 
                                NA,
                                PlotFC$Active_whm)  # set NA's to avoid line in graph
    PlotFC$Future <- ifelse(PlotFC$Future==0, 
                            NA,
                            PlotFC$Future)          # set NA's to avoid line in graph
    PlotFC$Future <- ifelse(PlotFC$Date=="2010-11-25",
                            PlotFC$Active_whm,
                            PlotFC$Future )         # set value for continuation in graph
    return(PlotFC)
  })
  
  # Create output Infoboxes
  output$TotalKWH <- renderInfoBox({
    infoBox(
      "KWH",
      round(SelectedData() %>%  
              summarise(sum = sum(Active_whm)/1000),
            0),
      icon = icon("bar-chart-o")
    )
  })
  
  output$TotalAmount <- renderInfoBox({
    infoBox(
      "euro",
      round(SelectedData() %>%  
              summarise(sum = sum(Active_whm)/1000)*0.17,
            0),
      icon = icon("credit-card")
    )
  })
  
  #create output graphs
  
  #dashboard plot energy used in time
  output$energydata <- renderPlot({
    ggplot(data = SelectedData()) + 
    geom_line(aes(x = Date, y = Active_whm/1000),color="slateblue")+
    ggtitle("Energy consumption in time") +
    labs( x="", y = "Consumption in kilowatt") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", 
                                face="bold", size=22),
      axis.title.y = element_text(size=14,face="bold", color="#666666"),
      axis.text.x = element_text(face="bold", color="#666666", size=16),
      axis.text.y = element_text(face="bold", color="#666666", size=16))
  })
  
  #dashboard plot devices
  output$deviceplot <- renderPlot({  
    ggplot(data=SelectedDeviceData() %>% 
                  group_by(Device) %>% 
                  summarise(Consump = sum(device_usage)), 
           aes(x=reorder(Device, Consump))) +
    geom_bar(aes(y=Consump),stat="identity", fill="mediumorchid") +
    coord_flip() +
    labs( y = "Consumption in watt-hour") +
    ggtitle("Top 5 Devices \nConsumption in selected period") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", 
                                face="bold", size=22),
      axis.title.y=element_blank(),
      axis.title.x = element_text(size=16,face="bold", color="#666666"),
      axis.text.x = element_text(size=16,face="bold", color="#666666"),
      axis.text.y = element_text(size=16,face="bold", color="#666666"))
  })
  
  #forecast plot daily usage
  output$forecastLinePlot <- renderPlot({
    ggplot(data = SelectedForecastData()) + 
    geom_line(aes(x = Date, y = Active_whm/1000),color="grey")+
    geom_line(aes(x = Date, y = Future/1000),color="slateblue")+
    ggtitle("Forecast of energy consumption") +
    labs(y="Energy consumption (kwh-m)") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", 
                                face="bold", size=22),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14,face="bold", color="#666666"),
      axis.text.x = element_text(size=14,face="bold", color="#666666"),
      axis.text.y = element_text(size=14,face="bold", color="#666666"))
  })
  
  #forecast plot full year
  output$monthlyBarPlot <- renderPlot({
    ggplot(data = MonthlyCost_FC %>%
             filter(Period !="TotalFC"), 
           aes(x=reorder(monthname,month), y=monthCost, fill=Period)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=c("slateblue", "grey")) +
    ggtitle("Monthly energy cost 2010 (in euro)") +
    labs(y="Monthly cost (euro)") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", 
                                face="bold", size=22),
      axis.title.x=element_blank(),
      axis.title.y = element_text(size=14,face="bold", color="#666666"),
      axis.text.x = element_text(size=14,face="bold", color="#666666"),
      axis.text.y = element_text(size=14,face="bold", color="#666666"))
  })
  
  #forecast table expected cost
  output$MonthFC <- renderTable({ 
    MonthlyCost_FC %>% 
      filter(Period=="TotalFC" & monthCost!=0) %>% 
      select(monthname, monthCost)
    })
  
  
  #create output messages
  output$messageDropdown <- renderMenu({
    dropdownMenu(type = "messages", .list = msglist)
  })
  
  #create output notifications
  output$notificationDropdown <- renderMenu({
    dropdownMenu(type = "notifications", .list = notiflist)
  })

  output$myImage <- renderImage({
    pfad <- "IOTanalytics.png"
    list(src = pfad,
         contentType = 'image/png',
         width = 140,
         height = 75,
         alt = "This is alternate text")
  }, deleteFile = F)
}

shinyApp(ui, server)
