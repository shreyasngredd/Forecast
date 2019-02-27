#####TIME SERIES FORECASTING#####

#Forecast the Plastic sales data set.

library(forecast)
library(fpp)
library(smooth)

#Loading dataset
ps<-read.csv(file.choose())
View(ps)
plot(ps$Sales,type="o")

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
#View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticsdata<-cbind(ps,X)
View(ps)
colnames(ps)

Plasticsdata["t"]<- 1:60
View(Plasticsdata)
Plasticsdata["log_Sales"]<-log(Plasticsdata["Sales"])
Plasticsdata["t_square"]<-Plasticsdata["t"]*Plasticsdata["t"]
attach(Plasticsdata)

#Data Partition
train_ps<-Plasticsdata[1:48,]
test_ps<-Plasticsdata[49:60,]

#Linear Model
linear_model<-lm(Sales~t,data=train_ps)
summary(linear_model)
summary(linear_model)$r.squared
summary(linear_model)$adj.r.squared*100
#Adjusted r-squared= 31.59%

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_ps))
View(linear_pred)
rmse_linear<-sqrt(mean((test_ps$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
#260.9378

#Exponential Model
exp_model<-lm(log_Sales~t,data = train_ps)
summary(exp_model)
summary(exp_model)$r.squared
summary(exp_model)$adj.r.squared*100
#Adjusted r-squared= 30.248%

exp_pred<-data.frame(predict(exp_model,interval='predict',newdata =test_ps))
exp_pred
rmse_exp<-sqrt(mean((test_ps$Sales-exp(exp_pred$fit))^2,na.rm = T))
rmse_exp 
#268.6938

#Quadratic Model
quad_model<-lm(Sales~t+t_square,data=train_ps)
summary(quad_model)
summary(quad_model)$r.squared
summary(quad_model)$adj.r.squared*100
#Adjusted r-squared= 30.484%

quad_pred<-data.frame(predict(quad_model,interval='predict',newdata =test_ps))
quad_pred
rmse_quad<-sqrt(mean((test_ps$Sales-quad_pred$fit)^2,na.rm = T))
rmse_quad
#297.4067

#Additive Seasonality
sd_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train_ps)
summary(sd_model)
summary(sd_model)$r.squared
summary(sd_model)$adj.r.squared*100
#Adjusted r-squared= 69.85142%

sd_pred<-data.frame(predict(sd_model,interval='predict',newdata=test_ps))
sd_pred
rmse_sd<-sqrt(mean((test_ps$Sales-sd_pred$fit)^2,na.rm = T))
rmse_sd
#235.6027

#Additive Seasonality with Linear
sdl_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train_ps)
summary(sdl_model)
summary(sdl_model)$r.squared
summary(sdl_model)$adj.r.squared*100
#Adjusted r-squared= 96.45333%

sdl_pred<-data.frame(predict(sdl_model,interval='predict',newdata=test_ps))
sdl_pred
rmse_sdl<-sqrt(mean((test_ps$Sales-sdl_pred$fit)^2,na.rm=T))
rmse_sdl
#135.5536

#Additive Seasonality with Quadrartic
adq_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train_ps)
summary(adq_model)
summary(adq_model)$r.squared
summary(adq_model)$adj.r.squared*100
#Adjusted r-squared= 97.67756%

adq_pred<-data.frame(predict(adq_model,interval='predict',newdata=test_ps))
adq_pred
rmse_adq<-sqrt(mean((test_ps$Sales-adq_pred$fit)^2,na.rm=T))
rmse_adq
#218.1939

#Multiplicative Seasonality
ms_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_ps)
summary(ms_model)
summary(ms_model)$r.squared
summary(ms_model)$adj.r.squared*100
#Adjusted r-squared= 72.79661%

ms_pred<-data.frame(predict(ms_model,newdata=test_ps,interval='predict'))
ms_pred
rmse_ms<-sqrt(mean((test_ps$Sales-exp(ms_pred$fit))^2,na.rm = T))
rmse_ms
#239.6543

# Multiplicative Seasonality Linear trend
mas_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_ps)
summary(mas_model) 
summary(mas_model)$r.squared
summary(mas_model)$adj.r.squared*100
#Adjusted r-squared= 97.51%

mas_pred<-data.frame(predict(mas_model,newdata=test_ps,interval='predict'))
mas_pred
rmse_mas<-sqrt(mean((test_ps$Sales-exp(mas_pred$fit))^2,na.rm = T))
rmse_mas
#160.6833

#Preparing table on model and it's RMSE values
table_rmse<-data.frame(c("rmse_linear","rmse_exp","rmse_quad","rmse_sd","rmse_adq"
                         ,"rmse_ms","rmse_mas"),
                       c(rmse_linear,rmse_exp,rmse_quad,rmse_sd,rmse_adq,rmse_ms,
                         rmse_mas))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticsdata)
new_model_pred<-data.frame(predict(new_model,newdata=Plasticsdata,interval='predict'))

new_model_fin <- exp(new_model$fitted.values)

Month <- as.data.frame(Plasticsdata$Month)

Final <- as.data.frame(cbind(Month,Plasticsdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")

View(Final)
