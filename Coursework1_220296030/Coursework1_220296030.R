
install.packages("remotes")
remotes::install_github('facebook/prophet@*release', subdir='R')
library(Rcpp)
library(rlang)
library(prophet)
library(astsa)
library(readxl)
death_statistics <- read_excel("~//Coursework1_220296030/data/death_statistics.xlsx")
# I have installed prophet and imported my death statistics
m =prophet::prophet(death_statistics, weekly.seasonality = TRUE, daily.seasonality = FALSE)
# due to mine being weekly seasonality i have had to specify this in the code in the line above
f =prophet::make_future_dataframe(m, periods=28, freq="month")
p =predict(m, f)
plot(m,p)
# the code you can see above is to ensure everything is in shape. a test if you will with code copied
# from the section 1.2 in the coursework content information. (now we get into the real stuff!!!)
dd =death_statistics
# Create a data frame with time (ds) and death statistics (y)
dd.df<-data.frame(t=as.Date(dd$ds),x=dd$y)
# Check the first few rows and the structure of the data frame to make sure it is all looking good
head(dd.df)
str(dd.df)
#Plot the time series with all the labels
plot(dd.df$t, dd.df$x,type='l',xlab='Time',ylab='Deaths',main='Weekly Death Statistics')
# Compute a simple linear regression and plot the fitted line in green for comparison
model<-lm(x ~ t, data = dd.df)
lines(dd.df$t,fitted(model),col='green')
#after running this we can see the linear model from this data - in the green - is ever s slightly decreasing
#I decided the forcast on the graph was not the best to be able to seethe exact numbers for the projected time period
# Instead i am doing a table to show the high yhat low yhat and yhat
forecast_data<-p[p$ds >= max(death_statistics$ds), ]
forecast_table<-forecast_data[,c("ds", "yhat","yhat_lower","yhat_upper")]
# create the table and name the columns
colnames(forecast_table)<-c("Date","Forecast (yhat)","Lower Bound (yhat_lower)","Upper Bound (yhat_upper)")
# Printing the table to see if it works
print(forecast_table)
#create a time series for the data starting at the first with a frequency of 52 as we are in years
ts_data_for_decompose<-ts(dd.df$x,start=c(2020),frequency=52)
# plot
plot(decompose(ts_data_for_decompose))

