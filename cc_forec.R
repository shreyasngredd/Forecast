#####TIME SERIES FORECASTING#####

#Forecast the Coca-Cola prices data set.

library(forecast)
library(fpp)
library(smooth)

#Loading dataset
cc<-read.csv(file.choose())
View(cc)
plot(cc$Sales,type="o")

Q1 <-  ifelse(grepl("Q1",cc$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cc$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cc$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cc$Quarter),'1','0')


CocacolaData<-cbind(cc,Q1,Q2,Q3,Q4)
View(CocacolaData)
summary(CocacolaData)

CocacolaData["t"]<- 1:42
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
View(CocacolaData)
attach(CocacolaData)

#Data Partition
train_cc<-CocacolaData[1:36,]
test_cc<-CocacolaData[37:40,]

#Linear Model
linear_model<-lm(Sales~t,data=train_cc)
summary(linear_model)
summary(linear_model)$r.squared
summary(linear_model)$adj.r.squared*100
#Adjusted r-squared= 79.22%

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_cc))
linear_pred
rmse_linear<-sqrt(mean((test_cc$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 
#644.0188

#Exponential Model
exp_model<-lm(log_Sales~t,data = train_cc)
summary(exp_model)
summary(exp_model)$r.squared
summary(exp_model)$adj.r.squared*100
#Adjusted r-squared= 80.17%

exp_pred<-data.frame(predict(exp_model,interval='predict',newdata =test_cc))
exp_pred
rmse_exp<-sqrt(mean((test_cc$Sales-exp(exp_pred$fit))^2,na.rm = T))
rmse_exp 
#524.7351

#Quadratic Model
quad_model<-lm(Sales~t+t_square,data=train_cc)
summary(quad_model)
summary(quad_model)$r.squared
summary(quad_model)$adj.r.squared*100
#Adjusted r-squared= 85.95%

quad_pred<-data.frame(predict(quad_model,interval='predict',newdata =test_cc))
quad_pred
rmse_quad<-sqrt(mean((test_cc$Sales-quad_pred$fit)^2,na.rm = T))
rmse_quad
#434.7185

#Additive Seasonality
sd_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train_cc)
summary(sd_model)
summary(sd_model)$r.squared
summary(sd_model)$adj.r.squared*100

sd_pred<-data.frame(predict(sd_model,interval='predict',newdata=test_cc))
sd_pred
rmse_sd<-sqrt(mean((test_cc$Sales-sd_pred$fit)^2,na.rm = T))
rmse_sd 
#1785.135

#Additive Seasonality with Linear
sdl_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train_cc)
summary(sdl_model)
summary(sdl_model)$r.squared
summary(sdl_model)$adj.r.squared*100
#Adjusted r-squared= 87.61%

sdl_pred<-data.frame(predict(sdl_model,interval='predict',newdata=test_cc))
sdl_pred
rmse_sdl<-sqrt(mean((test_cc$Sales-sdl_pred$fit)^2,na.rm=T))
rmse_sdl
#534.6979

#Additive Seasonality with Quadrartic
adq_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train_cc)
summary(adq_model)
summary(adq_model)$r.squared
summary(adq_model)$adj.r.squared*100
#Adjusted r-squared= 95.48%

adq_pred<-data.frame(predict(adq_model,interval='predict',newdata=test_cc))
adq_pred
rmse_adq<-sqrt(mean((test_cc$Sales-adq_pred$fit)^2,na.rm=T))
rmse_adq
#236.7075

#Multiplicative Seasonality
ms_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train_cc)
summary(ms_model)
summary(ms_model)$r.squared
summary(ms_model)$adj.r.squared*100

ms_pred<-data.frame(predict(ms_model,newdata=test_cc,interval='predict'))
ms_pred
rmse_ms<-sqrt(mean((test_cc$Sales-exp(ms_pred$fit))^2,na.rm = T))
rmse_ms
#1871.203

# Multiplicative Seasonality Linear trend
mas_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train_cc)
summary(mas_model) 
summary(mas_model)$r.squared
summary(mas_model)$adj.r.squared*100
#Adjusted r-squared= 89.86%

mas_pred<-data.frame(predict(mas_model,newdata=test_cc,interval='predict'))
mas_pred
rmse_mas<-sqrt(mean((test_cc$Sales-exp(mas_pred$fit))^2,na.rm = T))
rmse_mas
#335.1026

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_exp","rmse_quad","rmse_sd","rmse_adq"
                         ,"rmse_ms","rmse_mas"),
                       c(rmse_linear,rmse_exp,rmse_quad,rmse_sd,rmse_adq,rmse_ms,
                         rmse_mas))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values

View(new_model_fin)

Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
View(Final)

#Actual Graph
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

#Predicted Graph
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

