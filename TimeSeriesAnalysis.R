
# I found a real time series data set on presidential election polls 2016 from Huffington post
#
#Ans 1:  Source - http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton
#
# The data set has the following attributes
# Start Date,	End Date,	Number of Observations,	Population,	Mode,	Trump,	Clinton,	Other,	Undecided.
#
# The dataset contains the estimates based on the opinion polls. 
#


setwd("C:\\Users\\monis\\Desktop\\Time Series Analysis\\Assignment")
myData = read.csv("2016-general-election-trump-vs-clinton.csv")
#head(myData, n=2)

#Ans 2 Converting data into R time series format using ts command.
#I'm selecting first 100 rows of Trump column to plot
tsData = ts(myData$Trump[1:100])#, start = c(2015,9), end = c(2016,9) , frequency = 12)

#fix(tsData)

#Ans 3 Plotting the time series data
plot(tsData)
ts.plot(tsData, xlab="Time", ylab="Trump Data")

#Ans 4 - With the plot we can observe that, there are no seasonal components

df = data.frame(myData$Trump, myData$Clinton)

#Ans 5 - Performing decomposition on Trump and clinton components.
tsDecom = ts(df, start = c(2015,6), end = c(2016,9), frequency = 12)
tsComponents = decompose(tsDecom)

#plot(tsComponents)
#
#Ans 6 - Plot of the stochastic component of the data
acf(tsData)

#Ans 7 - Plot of the PACF 
pacf(tsData)

#Ans 8 - The auto correlation touches the significance bound with a lag ~ 0.02 for the ACF
#For PACF the lag touches the significance bound at 0.2

#ANs 9 Using Holt winters approach
tsForecast = HoltWinters(tsData, beta = FALSE, gamma = FALSE)
plot(tsForecast)

#Ans 10 using forecast component of Holt winters with Lag h=4
library("forecast")
electionForecast = forecast.HoltWinters(tsForecast, h=4)
plot.forecast(electionForecast)

#Ans 11 - Predictions are constant according to the blue line.


#Ans 12 - We see that the p-value is less which means the Ljung Box estimator provided favorable results.
Box.test(electionForecast$residuals, lag=4, type = "Ljung-Box")