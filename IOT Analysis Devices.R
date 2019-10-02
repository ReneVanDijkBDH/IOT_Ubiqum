
#############Devices#################
##### Sub3
#FullMinutes <- TotalMinuteData %>% filter(Date>"2006-12-27" & Date<"2010-11-26")
FullMinutes <- TotalMinuteData %>% filter(Date>"2010-07-01" & Date<"2010-08-08")
minute_S3_TS <- ts(FullMinutes$Sub3,frequency=1440)

minute_S3_TS %>%
  mstl(t.window=10800, s.window="periodic", robust=TRUE) %>%
  autoplot()


ggplot(data=TotalMinuteData %>% filter(Date>="2007-09-01" & Date <= "2007-09-07") , 
       aes(x=DateTime, y=Sub3)) + 
  geom_line(color="red")

ggplot(TotalMinuteData, aes(x=Sub3)) +
  geom_histogram(binwidth=1, colour="black", fill="white")