
#############Devices#################
##### Sub3
#FullMinutes <- TotalMinuteData %>% filter(Date>"2006-12-27" & Date<"2010-11-26")
FullMinutes <- TotalMinuteData %>% filter(Date>"2010-07-01" & Date<"2010-08-08")
minute_S3_TS <- ts(FullMinutes$Sub3,frequency=1440)

minute_S3_TS %>%
  mstl(t.window=10800, s.window="periodic", robust=TRUE) %>%
  autoplot()


ggplot(data=TotalMinuteData %>% filter(Date>="2008-03-01" & Date <= "2008-03-02") , 
       aes(x=DateTime, y=Sub3)) + 
  geom_line(color="red")
#in hours
ggplot(data=TotalMinuteData %>% 
         filter(DateTime>="2008-03-01 13:56:00" & DateTime <= "2008-03-01 14:04:00") , 
       aes(x=DateTime, y=Sub3)) + 
  geom_line(color="red")


ggplot(TotalMinuteData, aes(x=Sub3)) +
  geom_histogram(binwidth=1, colour="black", fill="white")

ggplot(MinuteData, aes(x=Sub_metering_3)) +
  geom_histogram(binwidth=1, colour="black", fill="white")

Sub3 <- arrange(MinuteData %>% 
                  select (Date, Time, NumValue, Sub_metering_3) %>%
                  rename(Level =Sub_metering_3)
                , Date, Time)

SubMeter <- Sub3
SubMeter$MinuteNr <- as.numeric(substr(SubMeter$Time,1,2))*60+
                     as.numeric(substr(SubMeter$Time,4,5))
SubMeter$UniqueNr <- with(SubMeter,NumValue*10000+MinuteNr)
SubMeter$UniqueNrPrev <- with(SubMeter,
                              ifelse(MinuteNr==0,
                                    (NumValue-1)*10000 +1439,               
                                    UniqueNr-1))
SubMeter$UniqueNrNext <- with(SubMeter,
                              ifelse(MinuteNr==1439,
                                     (NumValue+1)*10000,               
                                     UniqueNr+1))


SubMeter <- SubMeter %>%   left_join(SubMeter %>%
                                                 select(UniqueNr,Level) %>%
                                                 rename(UniqueNrPrev = UniqueNr, LevelPrev = Level)
                                               , by = c("UniqueNrPrev")) %>%
                      select(Date,Time, NumValue, Level, MinuteNr,  UniqueNr, UniqueNrPrev, 
                             UniqueNrNext, LevelPrev)

SubMeter <- SubMeter %>%   left_join(SubMeter %>%
                                                 select(UniqueNr,Level) %>%
                                                 rename(UniqueNrNext = UniqueNr, LevelNext = Level)
                                               , by = c("UniqueNrNext")) %>%
                      select(Date,Time, NumValue, Level, MinuteNr,  UniqueNr, UniqueNrPrev, 
                             UniqueNrNext, LevelPrev,LevelNext) 
SubMeter$DifPrev <- SubMeter$Level - SubMeter$LevelPrev
SubMeter$DifNext <- with(SubMeter, LevelNext - Level)

head(arrange(SubMeter %>% group_by(Level, LevelPrev, LevelNext,DifPrev, DifNext ) %>%
  summarise(count = n()) %>%
    filter(Level==0)
  ,-count)
  ,20)


  
  SubMeter %>%
                        left_join(SubMeter, by = c("UniqueNr", "UniqueNrPrev")) %>%
                        select(Level.y)
         
head(SubMeter %>%   left_join(SubMeter %>%
                                select(UniqueNr,Level) %>%
                                rename(UniqueNrPrev = UniqueNr, LevelPrev = Level)
                              , by = c("UniqueNrPrev")),10)
###Levels: 
### -1 data not available
### -2 data not processed yet
SubMeter$PreviousLevel <- -2
SubMeter$NextLevel     <- -2
RowLoop <- 1
PreviousLevel <- -1


while(RowLoop<nrow(SubMeter)){
  
  RowLoop = RowLoop+1
}
print(RowLoop)