CreateMonthlyOutputFormat <- function(FC_Monthly, monthlyData){
  FC_Monthly_TS <- FC_Monthly$mean #Convert forecast to TS
  FC_Monthly_DF <- data.frame(energy = c(FC_Monthly_TS), month = c(time(FC_Monthly_TS)))
  FC_Monthly_DF$year <- FC_Monthly_DF$month - FC_Monthly_DF$month%%1
  FC_Monthly_DF$month <- round((FC_Monthly_DF$month%%1) * 12,0)+1
  FC_Monthly_DF$monthname <- as.character(month(ymd(010101) + months(FC_Monthly_DF$month-1),
                                                label=TRUE,
                                                abbr=TRUE))
  FC_Monthly_DF$MonthDays <- with(FC_Monthly_DF, ifelse(month %in% c(1, 3, 5, 7, 8, 10, 12),31,
                                                        ifelse(month %in% c(4,6,9,11),30,
                                                               ifelse(year==2008,29,28))))
  FC_Monthly_DF$monthCost <- with(FC_Monthly_DF, energy * MonthDays / 1000 *0.17 )
  
  # Add actual consumption in output format
  MonthlyActual <- monthlyData %>% 
    filter(year==2010) %>%
    ungroup() %>%
    select(month,monthname, monthCost )
  MonthlyActual$Period <- "Spend"
  
  # Predictions in output format
  MonthlyPredict <- FC_Monthly_DF %>% select(month, monthname, monthCost)
  MonthlyPredict$Period <- "Expected"
  
  # Total consumption in output format
  MonthlyTotal <- MonthlyPredict
  MonthlyTotal$Period <- "TotalFC"
  # Make correction on current month's Prediction for already used energy
  CurrentMonthUsed <- as.numeric(MonthlyActual %>% 
                                   filter(month==11) %>%
                                   ungroup() %>%
                                   select(monthCost ))
  MonthlyPredict$monthCost <- with(MonthlyPredict, ifelse(month==11, monthCost - CurrentMonthUsed, monthCost))
  
  # combine data
  MonthlyCost <- rbind(MonthlyActual, MonthlyPredict, MonthlyTotal)
  return(MonthlyCost)
}