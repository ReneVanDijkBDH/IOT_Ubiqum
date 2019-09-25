library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(fpp2)

## Create a database connection 
con = dbConnect(MySQL(), 
              user='deepAnalytics', 
              password='Sqltask1234!', 
              dbname='dataanalytics2018', 
              host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'yr_2006')

## Use attribute names to specify specific attributes for download
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1 , Sub_metering_2, Sub_metering_3
                      FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1 , Sub_metering_2, Sub_metering_3
                      FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1 , Sub_metering_2, Sub_metering_3
                      FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1 , Sub_metering_2, Sub_metering_3
                      FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1 , Sub_metering_2, Sub_metering_3
                      FROM yr_2010")
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

data789 <- bind_rows(yr_2007, yr_2008, yr_2009)
data789 <- arrange(data789, Date, Time)

# dates with missing minutes
arrange(data789 %>% group_by(Date) %>% 
          summarise(minutecount=n()) %>%
          filter(minutecount!=1440), minutecount)


## Combine Date and Time attribute values in a new attribute column
data789 <- cbind(data789,paste(data789$Date,data789$Time), stringsAsFactors=FALSE)
## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(data789)[6] <-"DateTime"
## Move the DateTime attribute within the dataset
data789 <- data789[,c(ncol(data789), 1:(ncol(data789)-1))]
#head(data789)

## Convert DateTime from POSIXlt to POSIXct 
data789$DateTime <- as.POSIXct(data789$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
attr(data789$DateTime, "tzone") <- "Europe/Paris"

data789$year <- year(data789$DateTime)
data789$yearmonth <- year(data789$DateTime)*100+month(data789$DateTime)


# daily data
daily789 <- arrange(data789 %>%   group_by(Date) %>% 
                      summarise(minutecount=n(), 
                                sub1=sum(Sub_metering_1), 
                                sub2=sum(Sub_metering_2), 
                                sub3=sum(Sub_metering_3)), 
                    Date)

# create time-series
dailyTS <- ts(daily789,start=2007, frequency=365)
ggseasonplot(dailyTS, col=rainbow(12), year.labels=TRUE)


# visialize
ggplot(data=daily789, aes(x=Date, y=sub1, group=1)) + geom_line(color="red") 
ggplot(data=daily789, aes(x=Date, y=sub2, group=1)) + geom_line(color="blue") 
ggplot(data=daily789, aes(x=Date, y=sub3, group=1)) + geom_line(color="green") 



# monthly data
monthly789 <- arrange(data789 %>%   group_by(yearmonth) %>% 
                        summarise(minutecount=n(), 
                                  sub1=sum(Sub_metering_1), 
                                  sub2=sum(Sub_metering_2), 
                                  sub3=sum(Sub_metering_3)), 
                      yearmonth)
monthly789$yearmonth <- as.character(monthly789$yearmonth)

# visialize
ggplot(data=monthly789, aes(x=yearmonth, y=sub1, group=1)) + geom_line() 
ggplot(data=monthly789, aes(x=yearmonth, y=sub2, group=1)) + geom_line() 
ggplot(data=monthly789, aes(x=yearmonth, y=sub3, group=1)) + geom_line() 

# Weekday
data789$Date <- as.Date(data789$Date)
data789$WD <- wday(data789$Date, label=TRUE)

WD789 <- arrange(data789 %>%   group_by(WD) %>% 
                        summarise(minutecount=n(), 
                                  sub1=sum(Sub_metering_1), 
                                  sub2=sum(Sub_metering_2), 
                                  sub3=sum(Sub_metering_3)), 
                 WD)

ggplot(data=WD789, 
  aes(x=WD,y = sub1, fill=factor(ifelse(WD=="zo" | WD=="za","Weekend","Weekday")))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "part of week", values=c("grey50", "red")) 

