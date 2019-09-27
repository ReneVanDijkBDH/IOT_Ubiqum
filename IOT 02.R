library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(fpp2)
library(tidyverse)

# retrieve external energy data and bind in 1 dataframe
MinuteData <- RetrieveEnergyData()

#add numeric date value to use for loop 
MinuteData$NumValue <- as.numeric(as.Date(0, origin = as.Date(MinuteData$Date)))

# checks on data. dates with missing minutes etc
#arrange(MinuteData %>% group_by(Date) %>% 
#          summarise(minutecount=n()) %>%
#          filter(minutecount!=1440), minutecount)
#FullData %>% filter(is.na(Sub1))

#add missing data
LoopDate <- "2006-12-17"
EndDate <- "2010-11-25"
FullMinuteData <- CompleteMissingData(LoopDate, EndDate, MinuteData)

#Add multiple date-related columns to the dataframe
TotalMinuteData <- AddDateColumns(FullMinuteData)

#Add energy-related columns
TotalMinuteData <-AddEnergyColumns(TotalMinuteData)

# aggregate data to daily, weekly and monthly 
dailyData   <- AggregateMinuteData(TotalMinuteData,"Daily")
weekdayData  <- AggregateMinuteData(TotalMinuteData,"Weekly")
monthlyData <- AggregateMinuteData(TotalMinuteData,"Monthly")  

# create time-series
FullMonths <- monthlyData %>% filter(yearmonth>200612 & yearmonth<201011)
monthlyTS <- ts(FullMonths$Active_Daily,start=2007, frequency=12)
monthlyTS_Train <-  window(monthlyTS,start=2007, end =c(2009,12))
monthlyTS_Test  <-  window(monthlyTS,start=2010)
monthlyTS_Real <-  window(monthlyTS,start=2007, end =c(2010,10))
Test_Period <- 10 #months in test-period 
FC_Period   <- 2 #months to be forecasted

#analyse time-series
ggAcf(monthlyTS, lag=12)           #plot
acf(monthlyTS, lag=12, plot=FALSE) #numers


#Create forecasts for training period
FC_M_mean   <- meanf( monthlyTS_Train, h=Test_Period)
FC_M_naive  <- naive( monthlyTS_Train, h=Test_Period)
FC_M_snaive <- snaive(monthlyTS_Train, h=Test_Period)
FC_M_rwf    <- rwf(   monthlyTS_Train, h=Test_Period, drift = TRUE)

#analyse results
accuracy(FC_M_mean,monthlyTS_Test)
accuracy(FC_M_naive,monthlyTS_Test)
accuracy(FC_M_snaive,monthlyTS_Test)
accuracy(FC_M_rwf,monthlyTS_Test)

#analyse residials
res <- residuals(FC_M_snaive)

#gghistogram(res) + ggtitle("Histogram of residuals")
#ggAcf(res) + ggtitle("ACF of residuals")
checkresiduals(FC_M_snaive)
autoplot(FC_M_snaive) # graph with confidence intervals
autoplot(decompose(monthlyTS)) # decomposition of TS in trend and seasonality

#Forecast for the selected model
FC_Monthly <- snaive(monthlyTS_Real, h=FC_Period)

#convert to dataframe
FC_Monthly_TS <- FC_M$mean #Convert forecast to TS
FC_Monthly_DF <- data.frame(energy = c(FC_Monthly_TS), month = c(time(FC_Monthly_TS)))
FC_Monthly_DF$year <- FC_Monthly_DF$month - FC_Monthly_DF$month%%1
FC_Monthly_DF$month <- round((FC_Monthly_DF$month%%1) * 12,0)+1
FC_Monthly_DF$monthname <- as.character(month(ymd(010101) + months(FC_Monthly_DF$month-1),
                                        label=TRUE,
                                        abbr=TRUE))
FC_Monthly_DF$MonthDays <- with(FC_Monthly_DF, ifelse(month %in% c(1, 3, 5, 7, 8, 10, 21),31,
                                               ifelse(month %in% c(4,6,9,11),30,
                                               ifelse(year==2008,29,28))))
FC_Monthly_DF$monthCost <- with(FC_Monthly_DF, energy * MonthDays / 1000 *0.17 )
# Only Full-months
# Add current mont based on daily

MonthlyActual <- monthlyData %>% 
                    filter(year==2010) %>%
                    ungroup() %>%
                    select(month,monthname, monthCost )
MonthlyActual$Period <- "Spend"
MonthlyPredict <- FC_Monthly_DF %>% select(month, monthname, monthCost)
MonthlyPredict$Period <- "Expected"
#correction on current month for already used energy
CurrentMonthUsed <- as.numeric(MonthlyActual %>% 
                                 filter(month==11) %>%
                                 ungroup() %>%
                                 select(monthCost ))
MonthlyPredict$monthCost <- with(MonthlyPredict, ifelse(month==11, monthCost - CurrentMonthUsed, monthCost))

MonthlyCost <- rbind(MonthlyActual, MonthlyPredict)
saveRDS(MonthlyCost, file = "MonthlyCost.rds")



############# Create Plots ######################
# CreatePlots(dailyData, "dailyS1")              # Sub1 daily basis
# CreatePlots(dailyData, "dailyS2")              # Sub2 daily basis
# CreatePlots(dailyData, "dailyS3")              # Sub3 daily basis
# CreatePlots(monthlyData, "monthlyS1")          # Sub1 monthly basis
# CreatePlots(monthlyData, "monthlyS2")          # Sub2 monthly basis
# CreatePlots(monthlyData, "monthlyS3")          # Sub3 monthly basis
# CreatePlots(monthlyData, "monthlyTot")         # Total & other & subs on monthly basis
# CreatePlots(monthlyData, "monthlyTotDaily")    # Total vs on daily average (30-days)
# CreatePlots(weekdayData, "weekdayS1")          # Sub1 weekday basis
# ggseasonplot(monthlyTS, col=rainbow(12), year.labels=TRUE)
autoplot(monthlyTS) +
  autolayer(FC_M_mean, series="Mean", PI=FALSE) +
  autolayer(FC_M_rwf, series="Drift", PI=FALSE) +
  autolayer(FC_M_naive, series="Naïve", PI=FALSE) +
  autolayer(FC_M_snaive,series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly average daily energy consumption") +
  xlab("Year") + ylab("watt-hour per minute") +
  guides(colour=guide_legend(title="Forecast"))

############# Analyze results ######################
# accuracy(FC_M_mean,monthlyTS_Test)
# accuracy(FC_M_naive,monthlyTS_Test)
# accuracy(FC_M_snaive,monthlyTS_Test)
# accuracy(FC_M_rwf,monthlyTS_Test)