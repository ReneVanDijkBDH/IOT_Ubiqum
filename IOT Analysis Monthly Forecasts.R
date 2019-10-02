#analyse time-series
ggAcf(monthlyTS, lag=12)           #plot
acf(monthlyTS, lag=12, plot=FALSE) #numers


#Create forecasts for training period
FC_M_mean   <- meanf( monthlyTS_Train, h=Test_Period)
FC_M_naive  <- naive( monthlyTS_Train, h=Test_Period)
FC_M_snaive <- snaive(monthlyTS_Train, h=Test_Period)
FC_M_rwf    <- rwf(   monthlyTS_Train, h=Test_Period, drift = TRUE)

#analyse results
accuracy(FC_M_mean,monthlyTS_Test)
accuracy(FC_M_naive,monthlyTS_Test)
accuracy(FC_M_snaive,monthlyTS_Test)
accuracy(FC_M_rwf,monthlyTS_Test)

#analyse residials
res <- residuals(FC_M_snaive)

#gghistogram(res) + ggtitle("Histogram of residuals")
#ggAcf(res) + ggtitle("ACF of residuals")
checkresiduals(FC_M_snaive)
autoplot(FC_M_snaive) # graph with confidence intervals
autoplot(decompose(monthlyTS)) # decomposition of TS in trend and seasonality
autoplot(decompose(monthlyTS_S1)) # decomposition of Sub1 TS in trend and seasonality
autoplot(decompose(monthlyTS_S2)) # decomposition of Sub2 TS in trend and seasonality
autoplot(decompose(monthlyTS_S3)) # decomposition of Sub3 TS in trend and seasonality

# plot multiple FC-methods in one graph
autoplot(monthlyTS) +
  autolayer(FC_M_mean, series="Mean", PI=FALSE) +
  autolayer(FC_M_rwf, series="Drift", PI=FALSE) +
  autolayer(FC_M_naive, series="Naïve", PI=FALSE) +
  autolayer(FC_M_snaive,series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly average daily energy consumption") +
  xlab("Year") + 
  ylab("watt-hour per minute") +
  guides(colour=guide_legend(title="Forecast"))
