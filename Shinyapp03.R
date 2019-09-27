library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(lubridate)

# import data
ShinyData <- readRDS('data_ready.rds')
MonthlyCost_FC <- readRDS('MonthlyCost.rds')
MonthlyCost_FC <- arrange(MonthlyCost_FC,month)

# Create fake data per device
MicroData <- ShinyData %>% select( date, Kitchen_avg)
MicroData$Device <- "Microwave"
MicroData$Kitchen_avg <- MicroData$Kitchen_avg*0.6
MicroData <- rename(MicroData, device_usage = Kitchen_avg)

OvenData <- ShinyData %>% select( date, Kitchen_avg)
OvenData$Device <- "Oven"
OvenData$Kitchen_avg <- OvenData$Kitchen_avg*0.25
OvenData <- rename(OvenData, device_usage = Kitchen_avg)

DishData <- ShinyData %>% select( date, Kitchen_avg)
DishData$Device <- "Dishwasher"
DishData$Kitchen_avg <- DishData$Kitchen_avg*0.15
DishData <- rename(DishData, device_usage = Kitchen_avg)

WashingData <- ShinyData %>% select( date, Laundry_avg)
WashingData$Device <- "Washingmachine"
WashingData$Laundry_avg <- WashingData$Laundry_avg*0.6
WashingData <- rename(WashingData, device_usage = Laundry_avg)

DrierData <- ShinyData %>% select( date, Laundry_avg)
DrierData$Device <- "Tumble-drier"
DrierData$Laundry_avg <- DrierData$Laundry_avg*0.25
DrierData <- rename(DrierData, device_usage = Laundry_avg)

DeviceData <- rbind(MicroData,OvenData,DishData, WashingData, DrierData)

#create fake data for forecasts 2010
Real2010 <- ShinyData %>% filter( date>"2009-12-31")
Fake2010 <- ShinyData %>% filter( date>"2009-11-26" & date <="2009-12-31")
year(Fake2010$date) <- 2010
Total2010 <- rbind(Real2010, Fake2010)

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
notif1 <- notificationItem(text = "consumption fridge reduced",
                           icon("clock-o"),
                           status = "success")
notif2 <- notificationItem(text = "80% of montly budget used",
                           icon = icon("exclamation-triangle"),
                           status = "warning")
notiflist <- list(notif1, notif2)



ui <- dashboardPage( skin = "purple",
  dashboardHeader(title = "Energy consumption dashboard",
                  dropdownMenuOutput("messageDropdown"),
                  dropdownMenuOutput("notificationDropdown")
    ), 
                  
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
  ),
                  
  dashboardBody(
    body <- dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard", 
                selected = T,
          fluidRow(
      
             h3("Energy consumption for selected period"),
             hr(),
             infoBoxOutput("TotalKWH", 
                           width=2),
             infoBoxOutput("TotalAmount", 
                           width=2),
             box(dateRangeInput("daterange",
                                label="select period",
                                start="2010-10-01",
                                end="2010-10-31",
                                min="2007-01-01", 
                                max="2010-11-26"),
                width=4,
                status = "primary")
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
            fluidRow(
                h3("Forecasts"),
                box(dateInput("actualdate", 
                              "Actual date:", 
                              value = "2010-11-26", 
                              min ="2010-01-01", 
                              max="2010-12-31", 
                              format = "yyyy-mm-dd"),
                    width=3,
                    status = "primary"),
                box(title="Expected Cost", 
                    status = "warning",
                    tableOutput("MonthFC"))
            ),
            hr(),
            box(plotOutput(outputId = "forecastLinePlot"), 
                width=6,
                status = "primary"),
            box(plotOutput(outputId = "monthlyBarPlot"), 
                width=6,
                status = "primary")
        )
      )
    )
  )  
)



server <- function(input, output) {
  
  # create dataframes for presentation based on user inputs
  SelectedData <- reactive({
    ShinyData %>% filter(date >input$daterange[1] & date < input$daterange[2])
  })
 
  SelectedDeviceData <- reactive({
    DeviceData %>% filter(date >input$daterange[1] & date < input$daterange[2])
  })
  
  SelectedForecastData <-reactive({
    Total2010$Past <- ifelse(Total2010$date<=input$actualdate,Total2010$ActiveEnergy_avg,0)
    Total2010$Future <- ifelse(Total2010$date>input$actualdate,Total2010$ActiveEnergy_avg,0)
    Total2010$Month <- month(ymd(Total2010$date), label = TRUE, abbr = FALSE)
    return(Total2010)
  })
  
  SelectedMonthlyData <- reactive({
    MonthlyActual <- SelectedForecastData() %>% group_by(Month) %>% summarise(monthCost = sum(Past)*0.17)
    MonthlyActual$Period <- "Spend"
    MonthlyPredict <- SelectedForecastData() %>% group_by(Month) %>% summarise(monthCost = sum(Future)*0.17)
    MonthlyPredict$Period <- "Expected"
    MonthlyCost <- rbind(MonthlyActual, MonthlyPredict)
    MonthlyCost <- MonthlyCost %>% filter(monthCost!=0)
    return(MonthlyCost)
    #SelectedForecastData() %>% group_by(Month) %>% summarise(monthEnergy = sum(ActiveEnergy_avg)*0.17)
  })
  
  # Create output Infoboxes
  output$TotalKWH <- renderInfoBox({
    infoBox(
      "KWH",
      round(SelectedData() %>%  summarise(sum = sum(ActiveEnergy_avg)),0),
      icon = icon("bar-chart-o")
    )
  })
  
  output$TotalAmount <- renderInfoBox({
    infoBox(
      "euro",
      round(SelectedData() %>%  summarise(sum = sum(ActiveEnergy_avg))*0.17,0),
      icon = icon("credit-card")
    )
  })
  
  #create output graphs
  output$energydata <- renderPlot({
    ggplot(data = SelectedData()) + 
    geom_line(aes(x = date, y = ActiveEnergy_avg),color="slateblue")+
    ggtitle("Energy consumption in time") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", face="bold", size=22))
  })
  
  output$deviceplot <- renderPlot({  
    ggplot(data=SelectedDeviceData() %>% group_by(Device) %>% summarise(Consump = sum(device_usage)), aes(x=reorder(Device, Consump))) +
    geom_bar(aes(y=Consump),stat="identity", fill="mediumorchid") +
    coord_flip() +
    labs( y = "Consumption in watt-hour") +
    ggtitle("Top 5 Devices \nConsumption in selected period") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", face="bold", size=22),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=16))
  })
  
  output$forecastLinePlot <- renderPlot({
    ggplot(data = SelectedForecastData()) + 
    geom_line(aes(x = date, y = Past),color="grey")+
    geom_line(aes(x = date, y = Future),color="slateblue")+
    ggtitle("Forecast of energy consumption") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", face="bold", size=22))
  })

  output$monthlyBarPlot <- renderPlot({
    ggplot(data = MonthlyCost_FC, aes(x=reorder(monthname,month),y=monthCost, fill=Period)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=c("slateblue", "grey")) +
    ggtitle("Monthly energy cost 2010 (in euro)") +
    theme(panel.background = element_blank(),
      plot.title = element_text(hjust=0.5,family = "Trebuchet MS", color="#666666", face="bold", size=22))
  })
  
  
  output$MonthFC <- renderTable({ 
    MonthlyCost_FC%>% #select(Month, monthCost, Period) %>% 
      filter(Period=="Expected" & monthCost!=0) %>% select(monthname, monthCost)
    })
  
  
  #create output messages
  output$messageDropdown <- renderMenu({
    dropdownMenu(type = "messages", .list = msglist)
  })
  
  #create output notifications
  output$notificationDropdown <- renderMenu({
    dropdownMenu(type = "notifications", .list = notiflist)
  })
  
  
}



shinyApp(ui, server)
