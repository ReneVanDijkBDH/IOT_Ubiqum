AddDateColumns <- function(FullData) {
  #this function Date-related columns to dataframe
  
  ## Combine Date and Time attribute values in a new attribute column
  FullData <- cbind(FullData,
                    paste(FullData$Date,FullData$Time), 
                    stringsAsFactors=FALSE)
  ## Give the new attribute a header name 
  colnames(FullData)[ncol(FullData)] <-"DateTime"
  
  ## Move the DateTime attribute within the dataset
  FullData <- FullData[,c(ncol(FullData), 1:(ncol(FullData)-1))]
  #head(data789)
  
  ## Convert DateTime from POSIXlt to POSIXct 
  FullData$DateTime <- as.POSIXct(FullData$DateTime, "%Y/%m/%d %H:%M:%S")
  
  ## Add the time zone
  attr(FullData$DateTime, "tzone") <- "Europe/Paris"
  
  # add year and yearmonth
  FullData$year       <- year(FullData$Date)
  FullData$yearmonth  <- year(FullData$Date)*100+month(FullData$Date)
  FullData$month      <- month(FullData$Date)
  
  # add weekday
  FullData$Date <- as.Date(FullData$Date)
  FullData$WD   <- wday(FullData$Date, label=TRUE)
  
  return(FullData)
  
}