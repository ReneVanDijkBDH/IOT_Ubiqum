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

# checks on dates with missing minutes etc
#arrange(MinuteData %>% group_by(Date) %>% 
#          summarise(minutecount=n()) %>%
#          filter(minutecount!=1440), minutecount)

#add missing data
LoopDate <- "2006-12-17"
EndDate <- "2010-11-25"
FullMinuteData <- CompleteMissingData(LoopDate, EndDate, MinuteData)

#Add multiple date-related columns to the dataframe
TotalMinuteData <- AddDateColumns(FullMinuteData)

#Add energy-related columns
TotalMinuteData <-AddEnergyColumns(TotalMinuteData)

# aggregate data to daily, weekly and monthly 
dailyData    <- AggregateMinuteData(TotalMinuteData,"Daily")
weekdayData  <- AggregateMinuteData(TotalMinuteData,"Weekly")
monthlyData  <- AggregateMinuteData(TotalMinuteData,"Monthly")  

# create monthly time-series
FullMonths    <- monthlyData %>% 
                    filter(yearmonth>200612 & yearmonth<201011)
monthlyTS     <- ts(FullMonths$Active_Daily,start=2007, frequency=12)
monthlyTS_S1  <- ts(FullMonths$Sub1,start=2007, frequency=12)
monthlyTS_S2  <- ts(FullMonths$Sub2,start=2007, frequency=12)
monthlyTS_S3  <- ts(FullMonths$Sub3,start=2007, frequency=12)

# create daily time-series
FullDays    <- dailyData %>%  
                  filter(Date>"2006-12-27" & Date<"2010-11-26")
Daily_TS    <- ts(FullDays$Active_whm,frequency = 7)

# define train, test and forecast-periods
monthlyTS_Train <- window(monthlyTS,start=2007, end =c(2009,12))
monthlyTS_Test  <- window(monthlyTS,start=2010)
monthlyTS_Real  <- window(monthlyTS,start=2007, end =c(2010,10))
Test_Period     <- 10 #months in test-period 
FC_Period       <- 2  #months to be forecasted
FC_Period_Daily <- 36 #days to be forecasted (nov:5 + dec:31)

# APPLY: ANALYSIS MONTHLY FORECAST-MODEL TO SELECT PREFERRED MODEL
# => selected model: snaive
# Forecast for the selected model
FC_Monthly  <- snaive(monthlyTS_Real, h=FC_Period)
FC_Daily    <- snaive(Daily_TS, h=FC_Period_Daily)

# Create output format for monthly forecasts
MonthlyCost <- CreateMonthlyOutputFormat(FC_Monthly, monthlyData)
DailyUsage  <- CreateDailyOutputFormat(FC_Daily, FullDays)

# Create RDS files to use in the dashboard
saveRDS(MonthlyCost, file = "MonthlyCost.rds")
saveRDS(dailyData,   file = "DailyData.rds")
saveRDS(DailyUsage,  file = "DailyFC.rds")

############# Create Plots ######################
# CreatePlots(dailyData, "dailyS1")              # Sub1 daily basis
# CreatePlots(dailyData, "dailyS2")              # Sub2 daily basis
# CreatePlots(dailyData, "dailyS3")              # Sub3 daily basis
# CreatePlots(monthlyData, "monthlyS1")          # Sub1 monthly basis
# CreatePlots(monthlyData, "monthlyS2")          # Sub2 monthly basis
# CreatePlots(monthlyData, "monthlyS3")          # Sub3 monthly basis
# CreatePlots(monthlyData, "monthlyS3_2009")     # Sub3 monthly basis (2009 only)
# CreatePlots(monthlyData, "monthlyTot")         # Total & other & subs on monthly basis
# CreatePlots(monthlyData, "monthlyTotDaily")    # Total vs on daily average (30-days)
# CreatePlots(TotalMinuteData, "minuteS2")       # Sub2 minute-data. fraction of period
# CreatePlots(TotalMinuteData, "minuteS3")       # Sub3 minute-data. fraction of period
# CreatePlots(weekdayData, "weekdayS1")          # Sub1 weekday basis
# ggseasonplot(monthlyTS, col=rainbow(12), year.labels=TRUE)


############# Analyze results ######################
# accuracy(FC_M_mean,monthlyTS_Test)
# accuracy(FC_M_naive,monthlyTS_Test)
# accuracy(FC_M_snaive,monthlyTS_Test)
# accuracy(FC_M_rwf,monthlyTS_Test)