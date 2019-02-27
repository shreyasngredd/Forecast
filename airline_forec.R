#####TIME SERIES FORECASTING#####

#Forecast the Airlines Passengers data set.

library(forecast)
library(fpp)
library(smooth)

#Loading dataset
ad<-read.csv(file.choose())
View(ad)
ad <- ad$Passengers
ad <- as.ts(ad)
View(ad)
class(ad)

ad1 <- ts(ad,start=c(1995,1),end=c(2002,12),frequency=12)
start(ad1)
end(ad1)
class(ad1)

sum(is.na(ad1))
summary(ad1)
#Month: Mean= 48.50, Median= 48.50; As Mean=Median,it is has zero skewness.
#Passengers: Mean= 213.7, Median=200.0; As Mean>Median,it is skewed to the left.

View(ad1)

#Decomposition of multiplicative time series
decomdata<- decompose(ad1, "multiplicative")
plot(decomdata)

plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
plot(decomdata$figure)

#EDA (Original data)
plot(ad1)
abline(reg=lm(ad1~time(ad1)))

cycle(ad1)

#Monthly passengers from 1995 to 2002- Boxplot
boxplot(ad1~cycle(ad1))

adenoTS <- auto.arima(ad1)
adenoTS

# Trace function to determine the best p,d,q values that were selected.
auto.arima(ad1, ic = "aic", trace = TRUE)

# tseries evaluation
plot.ts(adenoTS$residuals)

# ACF Residual
acf(ts(adenoTS$residuals),main = 'ACF Residual')

#PACF Residual
pacf(ts(adenoTS$residuals),main = 'PACF Residual')

# Forecast for next 2 years
Pass_Forecast <- forecast(adenoTS,Level=c(95),h=10*12)
plot(Pass_Forecast)

#Testing the final model
Box.test(adenoTS$residuals, lag = 5, type = "Ljung-Box")
Box.test(adenoTS$residuals, lag = 15, type = "Ljung-Box")
Box.test(adenoTS$residuals, lag = 10, type = "Ljung-Box")
