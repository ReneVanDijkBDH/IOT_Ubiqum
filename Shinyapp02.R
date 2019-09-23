library(ggplot2)
library(dplyr)
library(lubridate)
ShinyData <- readRDS('data_ready.rds')

ggplot(data = ShinyData) + geom_line(aes(x = date, y = ActiveEnergy_avg))


set.seed(122)
histdata <- rnorm(50000)

  data <- histdata[seq_len(50000)]
  hist(data)
  
  
  SelectedData <-    ShinyData %>% filter(date >"2010-10-01")
  
  
  Waarde <- as.character(round(SelectedData %>%
    summarise(mean = mean(ActiveEnergy_avg)),1))
  Waarde <-as.character(Waarde)
  
  MicroData <- SelectedData %>% select( date, Kitchen_avg)
  MicroData$Device <- "Microwave"
  MicroData$Kitchen_avg <- MicroData$Kitchen_avg*0.6
  MicroData <- rename(MicroData, device_usage = Kitchen_avg)
  
  OvenData <- SelectedData %>% select( date, Kitchen_avg)
  OvenData$Device <- "Oven"
  OvenData$Kitchen_avg <- MicroData$Kitchen_avg*0.25
  OvenData <- rename(OvenData, device_usage = Kitchen_avg)
  
  WashingData <- SelectedData %>% select( date, Laundry_avg)
  WashingData$Device <- "Washingmachine"
  WashingData$Laundry_avg <- WashingData$Laundry_avg*0.6
  WashingData <- rename(WashingData, device_usage = Laundry_avg)
  
  DeviceData <- rbind(MicroData,OvenData, WashingData)
  
  Forecast2010 <- ShinyData %>% filter(date>="2009-11-27" & date<="2009-12-31")
  year(Forecast2010$date) <- 2010
  