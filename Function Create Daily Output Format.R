CreateDailyOutputFormat <- function(FC_Daily, FullDays){
  # this function converts the daily forecasts and actuals to an output format for the dashboard
  
  # create forecasts in required format out of object type Forecast
  FC_Daily_TS             <- FC_Daily$mean # Convert forecast to TS
  FC_Daily_DF             <- data.frame(Future = c(FC_Daily_TS), 
                                        day = c(time(FC_Daily_TS))) # Convert TS to DF
  FC_Daily_DF$week        <- FC_Daily_DF$day - FC_Daily_DF$day%%1-204 #weeknr based on fraction
  FC_Daily_DF$dayNr       <- (FC_Daily_DF$week-1)*7+ round((FC_Daily_DF$day%%1) * 7,0)
  FC_Daily_DF$Date        <- ymd("2010-11-25") + FC_Daily_DF$dayNr 
  FC_Daily_DF$Active_whm  <- 0 # set active to 0. required for output
  DailyPredict            <- FC_Daily_DF %>%
                                select(Date, Active_whm, Future)
  
  # create actual usage in required format
  DailyActual         <- FullDays %>% 
                            filter(year(Date)==2010) %>%
                            select(Date, Active_whm)
  DailyActual$Future  <-  0
  
  # combine actual and predicten in 1 dataframe
  Daily2010 <- rbind(DailyActual, DailyPredict)
  return(Daily2010)
}