AggregateMinuteData <- function(FullData, Agg_interval) {
  #this function aggregates the minute data to daily, weekly or monthly data
  
  if (Agg_interval == "Daily") {
    Agg_data <- arrange(FullData %>%   
                      group_by(Date) %>% 
                      summarise(minutecount=n(), 
                                Sub1=sum(Sub1), 
                                Sub2=sum(Sub2), 
                                Sub3=sum(Sub3),
                                Active_whm=sum(Active_whm),
                                Reactive_whm=sum(Reactive_whm),
                                Other_whm=sum(Other_whm)), 
          Date)
  
  }
  
  if (Agg_interval == "Weekly") {
    Agg_data <- arrange(FullData %>%   
                          group_by(WD) %>% 
                          summarise(minutecount=n(), 
                                    Sub1=sum(Sub1), 
                                    Sub2=sum(Sub2), 
                                    Sub3=sum(Sub3),
                                    Active_whm=sum(Active_whm),
                                    Reactive_whm=sum(Reactive_whm),
                                    Other_whm=sum(Other_whm)), 
                        WD)
    
  }
  
  
  if (Agg_interval == "Monthly") {
    Agg_data <- arrange(FullData %>%   
                          group_by(yearmonth, year,month) %>% 
                          summarise(minutecount=n(), 
                                    Sub1=sum(Sub1), 
                                    Sub2=sum(Sub2), 
                                    Sub3=sum(Sub3),
                                    Active_whm=sum(Active_whm),
                                    Reactive_whm=sum(Reactive_whm),
                                    Other_whm=sum(Other_whm),
                                    Datadays=n_distinct(Date)), 
          yearmonth)
    Agg_data$MonthDays <- with(Agg_data, ifelse(month %in% c(1, 3, 5, 7, 8, 10, 21),31,
                                                ifelse(month %in% c(4,6,9,11),30,
                                                ifelse(yearmonth==200802,29,28))))
    Agg_data$Active_Daily <- with(Agg_data, Active_whm/Datadays)
    Agg_data$monthname <- as.character(month(ymd(010101) + months(Agg_data$month-1),
                                                  label=TRUE,
                                                  abbr=TRUE))
    Agg_data$monthCost <- Agg_data$Active_whm/1000*0.17 
    
    
  }
  
  return(Agg_data)
}