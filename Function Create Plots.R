CreatePlots <- function(DataSet, PlotType) {
  if (PlotType=="dailyS1") {
    plot <- ggplot(data=DataSet, 
                   aes(x=Date, y=Sub1, group=1)) + 
              geom_line(color="red") 
  }
  if (PlotType=="dailyS2") {
    plot <- ggplot(data=DataSet, 
                   aes(x=Date, y=Sub1, group=1)) + 
              geom_line(color="blue") 
  }
  if (PlotType=="dailyS3") {
    plot <- ggplot(data=DataSet, 
                   aes(x=Date, y=Sub1, group=1)) + 
              geom_line(color="green") 
  }
  if (PlotType=="monthlyS1") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet, 
                   aes(x=yearmonth, y=Sub1, group=1)) + 
              geom_line(color="green") 
  }
  if (PlotType=="monthlyS2") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet, 
                  aes(x=yearmonth, y=Sub2, group=1)) + 
              geom_line(color="green") 
  }
  if (PlotType=="monthlyS3") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet, 
                  aes(x=yearmonth, y=Sub3, group=1)) + 
              geom_line(color="green") 
  }
  if (PlotType=="monthlyTot") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet, 
                  aes(x=yearmonth, group=1)) + 
              geom_line(aes(y=Active_whm/1000,color="Total",size=0.25)) +
              geom_line(aes(y=Other_whm/1000,color="Other")) +
              geom_line(aes(y=(Active_whm-Other_whm)/1000,color="Sub-Metered",size=0.25)) +
              scale_color_discrete(name = "Meter", labels = 
                  c("Total"="Total", "Other"="Other", "Sub-Metered" ="Sub-Metered")) +
              labs( x="time", y = "Energy consumption (in kwh)") +
              theme(panel.background = element_blank(),
                  axis.text.x = element_blank())
  }
  if (PlotType=="monthlyTotDaily") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet, 
                   aes(x=yearmonth, group=1)) + 
              geom_line(aes(y=Active_whm),color="green") +
              geom_line(aes(y=Active_Daily*30),color="blue") 
  }
  
  if (PlotType=="weekdayS1") {
    plot <- ggplot(data=DataSet, 
                  aes(x=WD,y = Sub1, 
                    fill=factor(ifelse(WD=="zo" | WD=="za","Weekend","Weekday")))) +
              geom_bar(stat = "identity") +
              scale_fill_manual(name = "part of week", values=c("grey50", "red")) 
  }
  if (PlotType=="minuteS2") {
    plot <- ggplot(data=DataSet %>% filter(Date>="2009-06-08" & Date <= "2009-06-08") , 
                   aes(x=DateTime, y=Sub2)) + 
              geom_line(color="red") 
  }
    if (PlotType=="minuteS3") {
    plot <- ggplot(data=DataSet %>% filter(Date>="2007-09-01" & Date <= "2007-09-07") , 
                   aes(x=DateTime, y=Sub3)) + 
              geom_line(color="red") +
              theme(panel.background = element_blank())
  }
  if (PlotType=="monthlyS3_2009") {
    DataSet$yearmonth <- as.character(DataSet$yearmonth)
    plot <- ggplot(data=DataSet %>% filter(year==2009), 
                   aes(x=yearmonth, y=Sub3, group=1)) + 
              geom_line(color="green") 
  }
  
  return(plot)  
}