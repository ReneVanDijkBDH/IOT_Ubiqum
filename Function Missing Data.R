CompleteMissingData <- function(LoopDate, EndDate, data789) {
  #This function replaces the missing records (minute based) 
  #With average previous and following week's values
  
  # create Dataframe with just all 1440 minutes
  DailyMinutes <- data789 %>% filter(Date=="2007-01-01") %>% select(Time)
  
  
  # give dates numerical value
  LoopNr <- as.numeric(as.Date(0, origin = as.Date(LoopDate)))
  StartNr <- as.numeric(as.Date(0, origin = as.Date(LoopDate)))
  EndNr <- as.numeric(as.Date(0, origin = as.Date(EndDate)))
  
  # loop through all the dates and for every date create dataset
  while(LoopNr<=EndNr) {
    #print(LoopNr)
    #create dataframe with all minutes and all data
    LoopDay <- left_join(DailyMinutes, 
                         data789 %>% 
                           filter(NumValue==LoopNr), 
                         by = "Time")
    LoopDay$NumValue <- ifelse(is.na(LoopDay$NumValue),
                               LoopNr,
                               LoopDay$NumValue)
    # add the date for in case it was missing
    LoopDay$LoopDate<- as.Date(LoopNr, origin="1970-01-01")
    
    # add data day week before and after
    LoopDay <- left_join(
      LoopDay, 
      data789 %>% 
        filter(NumValue==LoopNr-7) %>% 
        select(Time,
               Sub_metering_1,
               Sub_metering_2,
               Sub_metering_3, 
               Global_active_power,
               Global_reactive_power) %>%
        rename(Sub1M7=Sub_metering_1,
               Sub2M7=Sub_metering_2,
               Sub3M7=Sub_metering_3,
               GAPM7=Global_active_power, 
               GRPM7=Global_reactive_power), 
      by = "Time") 
    LoopDay <- left_join(
      LoopDay, 
      data789 %>% 
        filter(NumValue==LoopNr+7) %>% 
        select(Time,
               Sub_metering_1,
               Sub_metering_2,
               Sub_metering_3, 
               Global_active_power,
               Global_reactive_power) %>%
        rename(Sub1P7=Sub_metering_1,
               Sub2P7=Sub_metering_2,
               Sub3P7=Sub_metering_3,
               GAPP7=Global_active_power, 
               GRPP7=Global_reactive_power), 
      by = "Time") 
    # use actual values of the day or average of week before and after
    LoopDay$Sub1 <- with(LoopDay,ifelse(is.na(Sub_metering_1),
                                        ifelse(is.na(Sub1M7) | is.na(Sub1P7),
                                               0,
                                               (Sub1M7+Sub1P7)/2),
                                        Sub_metering_1))
    LoopDay$Sub2 <- with(LoopDay,ifelse(is.na(Sub_metering_2),
                                        ifelse(is.na(Sub2M7) | is.na(Sub2P7),
                                               0,
                                               (Sub2M7+Sub2P7)/2),
                                        Sub_metering_2))
    LoopDay$Sub3 <- with(LoopDay,ifelse(is.na(Sub_metering_3),
                                        ifelse(is.na(Sub3M7) | is.na(Sub3P7),
                                               0,
                                               (Sub3M7+Sub3P7)/2),
                                        Sub_metering_3))
    LoopDay$GAP <- with(LoopDay,ifelse(is.na(Global_active_power),
                                        ifelse(is.na(GAPM7) | is.na(GAPP7),
                                               0,
                                               (GAPM7+GAPP7)/2),
                                       Global_active_power))
    LoopDay$GRP <- with(LoopDay,ifelse(is.na(Global_reactive_power),
                                        ifelse(is.na(GRPM7) | is.na(GRPP7),
                                               0,
                                               (GRPM7+GRPP7)/2),
                                       Global_reactive_power))
    LoopDay <- LoopDay %>% select(NumValue,LoopDate, Time, Sub1, Sub2, Sub3, GAP, GRP)
    ifelse(LoopNr==StartNr,
           FullData <- LoopDay,
           FullData <- bind_rows(FullData, LoopDay))
    LoopNr = LoopNr+1
  }
  
  # rename loopdate to date
  FullData <- FullData %>% 
                rename(Date=LoopDate)
  
  #sort FullData in time 
  FullData <- arrange(FullData, Date, Time) 
  
  return(FullData)
}
