#CreateMonthlyOutputFormat <- function(FC_Daily, FullDays){
  FC_Daily_TS <- FC_Daily$mean #Convert forecast to TS
  FC_Daily_DF <- data.frame(Future = c(FC_Daily_TS), day = c(time(FC_Daily_TS)))
  FC_Daily_DF$week <- FC_Daily_DF$day - FC_Daily_DF$day%%1-204
  FC_Daily_DF$dayNr <- (FC_Daily_DF$week-1)*7+ round((FC_Daily_DF$day%%1) * 7,0)
  FC_Daily_DF$Date <- ymd("2010-11-25") + FC_Daily_DF$dayNr
  FC_Daily_DF$Active_whm <- 0 # set active to 0. required for output
  
  DailyActual <- FullDays %>% 
                    filter(year(Date)==2010) %>%
                    select(Date, Active_whm)
  DailyActual$Future <-0
  
  DailyPredict <- FC_Daily_DF %>%
                    select(Date, Active_whm, Future)
  # combine data
  Daily2010 <- rbind(DailyActual, DailyPredict)
 # return(MonthlyCost)
#}