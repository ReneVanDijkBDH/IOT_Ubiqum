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
