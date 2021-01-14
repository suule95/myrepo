# Clear all variables in workspace
rm(list=ls())

# Set working directory
setwd('C:/Users/suu le min/Documents/LL Water Data/Original Data')

# # Install necessary packages
# install.packages("tseries")
# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("bsts")

# Load the libraries
library(tseries)
library(ggplot2)
library(forecast)
library(bsts)
library(dplyr)
library(xts)

# Read data from csv file
original.df = read.csv('complete_df.csv')
colnames(original.df) = c('DateTime','Pressure','Temperature','Actual_Conductivity','Specific_Conductivity', 'Salinity', 'Tot_Diss_Solids','Resistivity','Density','Turbidity','Dissolved_Oxygen_mgL','Dissolved_Oxygen_percent_sat','Oxygen_Part_Pressure','Chloride_mgL','Chloride_mV','Temperature_2nd_02','External_Voltage_02','Battery_Capacity_02','Barometric_Pressure_02','pH','pH_mV','ORP_mV','Ammonium_as_Nitrogen_mgL','Ammonium_mV','Ammonia_as_Nitrogen_mgL','Tot_Ammonia_as_Nitr_mgL','Nitrate_as_Nitrogen_mgL','Nitrate_mV','Temperature_03','Actual_Conductivity_03','Specific_Conductivity_03','Salinity_03','Tot_Diss_Solids_03','Resistivity_03','Temperature_2nd_03','External_Voltage_03','Battery_Capacity_03','Barometric_Pressure_03','Battery','Tilt','Temper')

# Slice the required variable/column from user input
variable = readline(prompt = "Enter a variable: ")
while (variable %in% colnames(original.df) == FALSE){
  variable = readline(prompt = "Enter a variable: ")
}
var.df = original.df[,c('DateTime',variable)]

# Convert 'DateTime' column to a datetime object
var.df$DateTime = as.POSIXct(var.df$DateTime, format = '%Y-%m-%d %H:%M:%S',tz='Asia/Kuala_Lumpur')
names(var.df) = c('DateTime',variable)
str(var.df)

# Check the start and end datetime stamps
min(var.df$DateTime)
max(var.df$DateTime)

# Plot for distribution(hist), boxplot and trend
# hist(var.df$Chloride_mgL)
# boxplot(var.df$Chloride_mgL)
plot(var.df)
summary(var.df)
mean.var = mean(data.matrix(var.df[variable]))

# Remove duplicated obs
# unique(var.df[duplicated(var.df),]) is the same as >>> var.df[duplicated(var.df),]
# Show repeated obs
var.df.final = var.df[!duplicated(var.df$DateTime),]
# var.df.final = unique(var.df)
rownames(var.df.final) = 1:nrow(var.df.final)
# end date for analysis: 2018-05-31 08:35:00
end.date.for.analysis = as.POSIXct("2018-05-31 08:35:00", format="%Y-%m-%d %H:%M:%S",tz='Asia/Kuala_Lumpur')
var.df = var.df.final[1:which(var.df.final['DateTime'] == "2018-05-31 08:35:00"),]

# Generate a consistent timeseries
x = seq(from=min(var.df$DateTime), to=max(var.df$DateTime), by="5 min")
df = as.data.frame(x)
names(df) = c("DateTime")
str(df)
nrow(df)

# Find missing timestamps in the original df(var.df)
missing.timestamps = anti_join(df, var.df, by="DateTime")
# var.df.copy = as.data.frame(df[!(df$DateTime %in% diff.ts$DateTime),])
# names(var.df.copy) = "DateTime"
# missing.timestamps = anti_join(df,var.df.copy,by="DateTime")
missing.timestamps[variable] = 0
missing.timestamps[missing.timestamps==0] = mean.var

var.df.complete = rbind(var.df, missing.timestamps)
var.df.complete = var.df.complete[order(var.df.complete$DateTime),]
rownames(var.df.complete) = 1:nrow(var.df.complete)

# var.df.complete[var.df.complete==0] = mean.var
str(var.df.complete)
summary(var.df.complete)
min(var.df.complete$DateTime)
max(var.df.complete$DateTime)

# ===================================================
# ********** Create Time Series object *************
# ===================================================
# var.df.complete.copy = var.df.complete
# start.ts = min(var.df.complete.copy$DateTime)
# end.ts = max(var.df.complete.copy$DateTime)
# var.ts = ts(var.df.complete.copy[,2],start=start.ts,freq=365)
# print(var.ts)
# plot(var.ts)
# time(var.ts)
# 
# var.df.complete.copy[2] = tsclean(var.ts)
# ggplot() + geom_line(data=var.df.complete.copy, aes(x=DateTime, y = var.df.complete.copy[,2]))+ylab(paste0('Cleaned ', variable))
# component.ts= decompose(var.ts)
# plot(component.ts)




# ==============================================================================
############################ TS using xts() ####################################
# ==============================================================================
# Make the consistent time series into a time series object in R using entensible func: xts
var.xts = xts(var.df.complete[variable], order.by=var.df.complete$DateTime, frequency=300)
class(var.xts)
periodicity(var.xts)
plot(var.xts)
var.xts.core = coredata(var.xts)
class(var.xts.core)
var.xts.index = index(var.xts)
class(var.xts.index)
tzone(var.xts)

var.weekly = apply.weekly(var.xts,mean)

plot(var.xts)
lines(TTR::SMA(var.xts,n=288),col="blue")
lines(TTR::SMA(var.xts,n=1440),col="red",lty=2)

training.size = as.integer(0.66*length(var.xts))
train = var.xts[1:training.size]
test = var.xts[(training.size+1):length(var.xts)]

# ==============================================================================
############################### ARIMA model ####################################
# ==============================================================================
# 
# fit = auto.arima(train)
# # auto.arima(train)
# order = arimaorder(fit)
# 
# periodicity(train)
# autoplot(train) + labs(title="Time Series Plot")+labs(x="DateTime")+labs(y="Pressure")
# summary(train)
# 
# # Normal one step forecast [ Not good] 
# arima = arima(coredata(train),order=order)
# forecast.var = forecast(arima, h = 1000)
# autoplot(forecast.var)
# # Plot the residuals over time to see congruence or variance
# plot(forecast.var$residuals)
# # Plor the residuals (sample vs. theoretical)
# qqnorm(forecast.var$residuals)
# 
# acf(forecast.var$residuals)
# pacf(forecast.var$residuals)
# 
# ### Get accuracy by MAPE and other leading indicators - each dataset is different
# # method 1:
# summary(arima)
# # method 2:
# accuracy(arima)
# # replot the forecast
# plot(forecast.var)
# 
# d1 = data.frame(c(fitted(arima), predict(arima, n.ahead=length(test))$pred), coredata(var.xts), index(var.xts))
# names(d1) = c("Fitted","Actual","DateTime")
#                 
# MAPE = filter(d1, index(var.xts)>length(train)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
#                               
# ggplot(data=d1, aes(x=DateTime))+ geom_line(aes(y=Actual, colour="Actual"),size=1.2) + geom_line(aes(y=Fitted,colour="Fitted"),size=1.2,linetype=2) + theme_bw() + theme(legend.title=element_blank()) + ylab("") + xlab("") + geom_vline(xintercept=index(var.xts)[length(train)+1], linetype=2) + ggtitle(paste0("ARIMA -- Holdout MAPE = ",round(100*MAPE,2),"%")) +theme(axis.text.x=element_text(angle=-90,hjust=0))

# prediction = predict(arima, n.ahead=length(test))

# ========= ARIMA in R ==============
### - 1.Visualize the time series

ggplot(var.df.complete, aes(x=DateTime, y=var.df.complete[,2])) + geom_line() + ylab(variable)
class(var.df.complete[,2])

### - 2. Stationarize the time series
adf.test(var.df.complete[,2], alternative="stationary",k=0)

### - 3. ACF/PACF
acf(var.df.complete[,2])
pacf(var.df.complete[,2])


auto.arima(coredata(train),trace= TRUE)
### - 4. Build the ARIMA model
fit.arima = arima(coredata(train), c(5,1,3))
fit.arima
summary(fit.arima)


plot(train)
lines(fitted(fit.arima),col="red",lwd=5)


future.val = forecast(fit.arima, h=3428, level=c(95))
plot(future.val)
future.val$mean
periodicity(train)

d1 = data.frame(c(fitted(fit.arima), predict(fit.arima, n.ahead=length(test))$pred), coredata(var.xts), index(var.xts))
names(d1) = c("Fitted","Actual","DateTime")

MAPE = filter(d1, index(var.xts)>length(train)) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

ggplot(data=d1, aes(x=DateTime))+ geom_line(aes(y=Actual, colour="Actual"),size=1.2) + geom_line(aes(y=Fitted,colour="Fitted"),size=1.2,linetype=2) + theme_bw() + theme(legend.title=element_blank()) + ylab("") + xlab("") + geom_vline(xintercept=index(var.xts)[length(train)+1], linetype=2) + ggtitle(paste0("ARIMA -- Holdout MAPE = ",round(100*MAPE,2),"%")) +theme(axis.text.x=element_text(angle=-90,hjust=0))
# ----------------------- Rolling Forecast -------------------------------------

# https://machinelearningmastery.com/simple-time-series-forecasting-models/


# ==============================================================================
############################ BSTS model ########################################
# ==============================================================================
# install.packages("prophet")
library(prophet)
var.df.prophet = var.df.complete
names(var.df.prophet) = c("ds","y")
train_p_size =  as.integer(0.66*nrow(var.df.prophet))
train_p = var.df.prophet[1:train_p_size,]
test_p = var.df.prophet[(train_p_size+1):nrow(var.df.prophet),]

m = prophet(train_p)

future = make_future_dataframe(m, periods = nrow(test_p),freq=300)
forecast = predict(m, future)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(m,forecast)
prophet_plot_components(m,forecast)
dyplot.prophet(m, forecast)

fitted = data.frame(forecast['yhat'][1:train_p_size,])
model_accuracy = cbind(train_p, fitted)
names(model_accuracy) = c("DateTime","Actual","Fitted")
MAPE_train = mean(abs(model_accuracy$Actual-model_accuracy$Fitted)/model_accuracy$Actual)
MAPE_train*100

predicted = data.frame(forecast['yhat'][(train_p_size+1):nrow(var.df.prophet),])
eval_df = cbind(test_p, predicted)
names(eval_df) = c("DateTime","Actual","Predicted")
str(eval_df)
eval_df['DateTime'] = as.POSIXct(eval_df$DateTime, format="%Y-%m-%d %H:%M:%S")

ggplot(data=eval_df, aes(x=DateTime)) + geom_line(aes(y=Actual, colour="Actual"),size=1.2) + geom_line(aes(y=Predicted,colour="Predicted"),size=1.2,linetype=2)

MAPE = mean(abs(eval_df$Actual-eval_df$Predicted)/eval_df$Actual)
MAPE*100



# ==============================================================================
############################ BSTS model BSTS ###################################
# ==============================================================================
library(bsts)
ss = AddLocalLinearTrend(list(), train)
ss = AddSeasonal(ss, train, nseasons = 288)
bsts.model = bsts(train, state.specification = ss, niter=500, ping=0, seed = 2020)
burn = SuggestBurn(0.1, bsts.model)
p = predict.bsts(bsts.model, horizon=length(test), burn=burn, quantiles=c(0.025, 0.975))
d2 = data.frame(c(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+train, p$mean), coredata(var.xts), index(var.xts))
names(d2) = c("Fitted","Actual","DateTime")

MAPE = filter(d2, DateTime>var.xts.index[training.size]) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

posterior.interval = cbind.data.frame(p$interval[1,],p$interval[2,],subset(d2,DateTime>var.xts.index[training.size]))
names(posterior.interval) = c("LL","UL","DateTime")

d3 = left_join(d2, posterior.interval, by="DateTime")

ggplot(data = d3, aes(x=DateTime)) + 
  geom_line(aes(y=Actual, colour="Actual"),size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2)+
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + geom_vline(xintercept = var.xts.index[training.size], linetype = 2) + 
  geom_ribbon(aes(ymin=LL,ymax=UL), fill = "grey", alpha=0.5) +
  ggtitle(paste0("BSTS -- Holdout MAPE = ", round(100*MAPE,2),"%")) + theme(axis.text.x = element_text(angle=-90, hjust=0))