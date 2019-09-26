RetrieveEnergyData <- function() {

  ## Create a database connection 
  con = dbConnect(MySQL(), 
                  user='deepAnalytics', 
                  password='Sqltask1234!', 
                  dbname='dataanalytics2018', 
                  host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
  
  ## List the tables contained in the database 
  #dbListTables(con)
  
  ## Lists attributes contained in a table
  #dbListFields(con,'yr_2006')
  
  ## Use attribute names to specify specific attributes for download
  yr_2006 <- dbGetQuery(con, "SELECT Date, Time, 
                                    Sub_metering_1 , Sub_metering_2, Sub_metering_3,
                                    Global_active_power, Global_reactive_power, 
                                    Global_intensity, Voltage
                            FROM yr_2006")
  yr_2007 <- dbGetQuery(con, "SELECT Date, Time, 
                                    Sub_metering_1 , Sub_metering_2, Sub_metering_3,
                                    Global_active_power, Global_reactive_power, 
                                    Global_intensity, Voltage
                      FROM yr_2007")
  yr_2008 <- dbGetQuery(con, "SELECT Date, Time, 
                                    Sub_metering_1 , Sub_metering_2, Sub_metering_3,
                                    Global_active_power, Global_reactive_power, 
                                    Global_intensity, Voltage
                      FROM yr_2008")
  yr_2009 <- dbGetQuery(con, "SELECT Date, Time, 
                                    Sub_metering_1 , Sub_metering_2, Sub_metering_3,
                                    Global_active_power, Global_reactive_power, 
                                    Global_intensity, Voltage
                      FROM yr_2009")
  yr_2010 <- dbGetQuery(con, "SELECT Date, Time, 
                                    Sub_metering_1 , Sub_metering_2, Sub_metering_3,
                                    Global_active_power, Global_reactive_power, 
                                    Global_intensity, Voltage
                      FROM yr_2010")
  
  #create combined dataset 
  CombinedData <- bind_rows(yr_2006,yr_2007, yr_2008, yr_2009, yr_2010)
  CombinedData <- arrange(CombinedData, Date, Time)
  return(CombinedData)
}