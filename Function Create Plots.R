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
      geom_line(aes(y=Active_whm),color="green") +
      geom_line(aes(y=Other_whm),color="red") +
      geom_line(aes(y=Active_whm-Other_whm),color="blue")
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
  
  
  
  return(plot)  
}