library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(fpp2)

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
dailyTS <- ts(daily789$sub1,start=2007, frequency=7)
ggseasonplot(dailyTS, col=rainbow(12), year.labels=TRUE)

monthlyTS <- ts(monthly789$sub3,start=2007, frequency=12)
ggseasonplot(monthlyTS, col=rainbow(12), year.labels=TRUE)

ggAcf(monthlyTS, lag=12)
acf(monthlyTS, lag=12, plot=FALSE)

res <- residuals(naive(monthlyTS))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")
checkresiduals(naive(monthlyTS))
autoplot(naive(monthlyTS))


# Create Plots
# CreatePlots(dailyData, "dailyS1")              # Sub1 daily basis
# CreatePlots(dailyData, "dailyS2")              # Sub2 daily basis
# CreatePlots(dailyData, "dailyS3")              # Sub3 daily basis
# CreatePlots(monthlyData, "monthlyS1")          # Sub1 monthly basis
# CreatePlots(monthlyData, "monthlyS2")          # Sub2 monthly basis
# CreatePlots(monthlyData, "monthlyS3")          # Sub3 monthly basis
# CreatePlots(monthlyData, "monthlyTot")         # Total & other & subs on monthly basis
# CreatePlots(monthlyData, "monthlyTotDaily")    # Total vs on daily average (30-days)
# CreatePlots(weekdayData, "weekdayS1")          # Sub1 weekday basis

