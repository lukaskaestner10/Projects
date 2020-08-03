## I. Shure Data

# Clear environment
rm(list=ls())

#Load forecast package and readxl package
library(forecast)
library(readxl)

# Load Shure data
Shure_data <- read_excel("ShureData.xlsx", sheet = "CustOrders")
Shure_forecast <- read_excel("ShureData.xlsx", sheet = "ShureForecasts")
head(Shure_data)

# Convert all to time series object
FLX012_ts<-ts(Shure_data$FLX012, start=c(2006,1),frequency=12)
FLX019_ts<-ts(Shure_data$FLX019, start=c(2006,1),frequency=12)
FLX022_ts<-ts(Shure_data$FLX022, start=c(2006,1),frequency=12)
FLX026_ts<-ts(Shure_data$FLX026, start=c(2006,1),frequency=12)
FLX078_ts<-ts(Shure_data$FLX078, start=c(2006,1),frequency=12)
FLX081_ts<-ts(Shure_data$FLX081, start=c(2006,1),frequency=12)
FLX102_ts<-ts(Shure_data$FLX102, start=c(2006,1),frequency=12)
FLX105_ts<-ts(Shure_data$FLX105, start=c(2006,1),frequency=12)
FLX107_ts<-ts(Shure_data$FLX107, start=c(2006,1),frequency=12)
Total_ts<-ts(Shure_data$Total, start=c(2006,1),frequency=12)

Fore_FLX012_ts<-ts(Shure_forecast$FLX012, start=c(2014,6),frequency=12)
Fore_FLX019_ts<-ts(Shure_forecast$FLX019, start=c(2014,6),frequency=12)
Fore_FLX022_ts<-ts(Shure_forecast$FLX022, start=c(2014,6),frequency=12)
Fore_FLX026_ts<-ts(Shure_forecast$FLX026, start=c(2014,6),frequency=12)
Fore_FLX078_ts<-ts(Shure_forecast$FLX078, start=c(2014,6),frequency=12)
Fore_FLX081_ts<-ts(Shure_forecast$FLX081, start=c(2014,6),frequency=12)
Fore_FLX102_ts<-ts(Shure_forecast$FLX102, start=c(2014,6),frequency=12)
Fore_FLX105_ts<-ts(Shure_forecast$FLX105, start=c(2014,6),frequency=12)
Fore_FLX107_ts<-ts(Shure_forecast$FLX107, start=c(2014,6),frequency=12)
Fore_Total_ts<-ts(Shure_forecast$Total, start=c(2014,6),frequency=12)

# Create Train and Test data for all 10 models
FLX012_in <- window(FLX012_ts,end=c(2014,5))
FLX012_out <- window(FLX012_ts,start=c(2014,6))
FLX012_in

FLX019_in <- window(FLX019_ts,end=c(2014,5))
FLX019_out <- window(FLX019_ts,start=c(2014,6))

FLX022_in <- window(FLX022_ts,end=c(2014,5))
FLX022_out <- window(FLX022_ts,start=c(2014,6))

FLX026_in <- window(FLX026_ts,end=c(2014,5))
FLX026_out <- window(FLX026_ts,start=c(2014,6))

FLX078_in <- window(FLX078_ts,end=c(2014,5))
FLX078_out <- window(FLX078_ts,start=c(2014,6))

FLX081_in <- window(FLX081_ts,end=c(2014,5))
FLX081_out <- window(FLX081_ts,start=c(2014,6))

FLX102_in <- window(FLX102_ts,end=c(2014,5))
FLX102_out <- window(FLX102_ts,start=c(2014,6))

FLX105_in <- window(FLX105_ts,end=c(2014,5))
FLX105_out <- window(FLX105_ts,start=c(2014,6))

FLX107_in <- window(FLX107_ts,end=c(2014,5))
FLX107_out <- window(FLX107_ts,start=c(2014,6))

Total_in <- window(Total_ts,end=c(2014,5))
Total_out <- window(Total_ts,start=c(2014,6))


accuracy(FLX012_out,Fore_FLX012_ts)
accuracy(FLX019_out,Fore_FLX019_ts)
accuracy(FLX022_out,Fore_FLX022_ts)
accuracy(FLX026_out,Fore_FLX026_ts)
accuracy(FLX078_out,Fore_FLX078_ts)
accuracy(FLX081_out,Fore_FLX081_ts)
accuracy(FLX102_out,Fore_FLX102_ts)
accuracy(FLX105_out,Fore_FLX105_ts)
accuracy(FLX107_out,Fore_FLX107_ts)
accuracy(Total_out,Fore_Total_ts)


# Create models, find the best one and test using RMSE metric

# FLX012

FLX012_modann <- ets(FLX012_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX012_modann,h=3)
accuracy(fore_ann_in,FLX012_in)

FLX012_modaan <- ets(FLX012_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX012_modaan,h=3)
accuracy(fore_aan_in,FLX012_in)

FLX012_modana <- ets(FLX012_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX012_modana,h=3)
accuracy(fore_ana_in,FLX012_in)

FLX012_modaaa <- ets(FLX012_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX012_modaaa,h=3)
accuracy(fore_aaa_in,FLX012_in)

FLX012_modana_refit <- ets(FLX012_ts, model=FLX012_modana, use.initial.values=TRUE)
FLX012_myforeana_3 <- fitted(FLX012_modana_refit,h=3)
accuracy(fore_ana_in,FLX012_in)
accuracy(FLX012_myforeana_3,FLX012_out)
summary(FLX012_modana)

# FLX019

FLX019_modann <- ets(FLX019_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX019_modann,h=3)
accuracy(fore_ann_in,FLX019_in)

FLX019_modaan <- ets(FLX019_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX019_modaan,h=3)
accuracy(fore_aan_in,FLX019_in)

FLX019_modana <- ets(FLX019_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX019_modana,h=3)
accuracy(fore_ana_in,FLX019_in)

FLX019_modaaa <- ets(FLX019_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX019_modaaa,h=3)
accuracy(fore_aaa_in,FLX019_in)

FLX019_modana_refit <- ets(FLX019_ts, model=FLX019_modana, use.initial.values=TRUE)
FLX019_myforeana_3 <- fitted(FLX019_modana_refit,h=3)
accuracy(fore_ana_in,FLX019_in)
accuracy(FLX019_myforeana_3,FLX019_out)
summary(FLX019_modana)

# FLX022

FLX022_modann <- ets(FLX022_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX022_modann,h=3)
accuracy(fore_ann_in,FLX022_in)

FLX022_modaan <- ets(FLX022_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX022_modaan,h=3)
accuracy(fore_aan_in,FLX022_in)

FLX022_modana <- ets(FLX022_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX022_modana,h=3)
accuracy(fore_ana_in,FLX022_in)

FLX022_modaaa <- ets(FLX022_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX022_modaaa,h=3)
accuracy(fore_aaa_in,FLX022_in)

FLX022_modana_refit <- ets(FLX022_ts, model=FLX022_modana, use.initial.values=TRUE)
FLX022_myforeana_3 <- fitted(FLX022_modana_refit,h=3)
accuracy(fore_ana_in,FLX022_in)
accuracy(FLX022_myforeana_3,FLX022_out)
summary(FLX022_modana)

# FLX026

FLX026_modann <- ets(FLX026_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX026_modann,h=3)
accuracy(fore_ann_in,FLX026_in)

FLX026_modaan <- ets(FLX026_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX026_modaan,h=3)
accuracy(fore_aan_in,FLX026_in)

FLX026_modana <- ets(FLX026_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX026_modana,h=3)
accuracy(fore_ana_in,FLX026_in)

FLX026_modaaa <- ets(FLX026_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX026_modaaa,h=3)
accuracy(fore_aaa_in,FLX026_in)

FLX026_modana_refit <- ets(FLX026_ts, model=FLX026_modana, use.initial.values=TRUE)
FLX026_myforeana_3 <- fitted(FLX026_modana_refit,h=3)
accuracy(fore_ana_in,FLX026_in)
accuracy(FLX026_myforeana_3,FLX026_out)
summary(FLX026_modana)

# FLX078

FLX078_modann <- ets(FLX078_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX078_modann,h=3)
accuracy(fore_ann_in,FLX078_in)

FLX078_modaan <- ets(FLX078_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX078_modaan,h=3)
accuracy(fore_aan_in,FLX078_in)

FLX078_modana <- ets(FLX078_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX078_modana,h=3)
accuracy(fore_ana_in,FLX078_in)

FLX078_modaaa <- ets(FLX078_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX078_modaaa,h=3)
accuracy(fore_aaa_in,FLX078_in)

FLX078_modana_refit <- ets(FLX078_ts, model=FLX078_modana, use.initial.values=TRUE)
FLX078_myforeana_3 <- fitted(FLX078_modana_refit,h=3)
accuracy(fore_ana_in,FLX078_in)
accuracy(FLX078_myforeana_3,FLX078_out)
summary(FLX078_modana)

# FLX081

FLX081_modann <- ets(FLX081_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX081_modann,h=3)
accuracy(fore_ann_in,FLX081_in)

FLX081_modaan <- ets(FLX081_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX081_modaan,h=3)
accuracy(fore_aan_in,FLX081_in)

FLX081_modana <- ets(FLX081_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX081_modana,h=3)
accuracy(fore_ana_in,FLX081_in)

FLX081_modaaa <- ets(FLX081_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX081_modaaa,h=3)
accuracy(fore_aaa_in,FLX081_in)

FLX081_modana_refit <- ets(FLX081_ts, model=FLX081_modana, use.initial.values=TRUE)
FLX081_myforeana_3 <- fitted(FLX081_modana_refit,h=3)
accuracy(fore_ana_in,FLX081_in)
accuracy(FLX081_myforeana_3,FLX081_out)
summary(FLX081_modana)

# FLX102

FLX102_modann <- ets(FLX102_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX102_modann,h=3)
accuracy(fore_ann_in,FLX102_in)

FLX102_modaan <- ets(FLX102_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX102_modaan,h=3)
accuracy(fore_aan_in,FLX102_in)

FLX102_modana <- ets(FLX102_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX102_modana,h=3)
accuracy(fore_ana_in,FLX102_in)

FLX102_modaaa <- ets(FLX102_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX102_modaaa,h=3)
accuracy(fore_aaa_in,FLX102_in)

FLX102_modana_refit <- ets(FLX102_ts, model=FLX102_modana, use.initial.values=TRUE)
FLX102_myforeana_3 <- fitted(FLX102_modana_refit,h=3)
accuracy(fore_ana_in,FLX102_in)
accuracy(FLX102_myforeana_3,FLX102_out)
summary(FLX102_modana)

# FLX105

FLX105_modann <- ets(FLX105_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX105_modann,h=3)
accuracy(fore_ann_in,FLX105_in)

FLX105_modaan <- ets(FLX105_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX105_modaan,h=3)
accuracy(fore_aan_in,FLX105_in)

FLX105_modana <- ets(FLX105_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX105_modana,h=3)
accuracy(fore_ana_in,FLX105_in)

FLX105_modaaa <- ets(FLX105_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX105_modaaa,h=3)
accuracy(fore_aaa_in,FLX105_in)

FLX105_modana_refit <- ets(FLX105_ts, model=FLX105_modana, use.initial.values=TRUE)
FLX105_myforeana_3 <- fitted(FLX105_modana_refit,h=3)
accuracy(fore_ana_in,FLX105_in)
accuracy(FLX105_myforeana_3,FLX105_out)
summary(FLX105_modana)

# FLX107

FLX107_modann <- ets(FLX107_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(FLX107_modann,h=3)
accuracy(fore_ann_in,FLX107_in)

FLX107_modaan <- ets(FLX107_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(FLX107_modaan,h=3)
accuracy(fore_aan_in,FLX107_in)

FLX107_modana <- ets(FLX107_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(FLX107_modana,h=3)
accuracy(fore_ana_in,FLX107_in)

FLX107_modaaa <- ets(FLX107_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(FLX107_modaaa,h=3)
accuracy(fore_aaa_in,FLX107_in)

FLX107_modana_refit <- ets(FLX107_ts, model=FLX107_modana, use.initial.values=TRUE)
FLX107_myforeana_3 <- fitted(FLX107_modana_refit,h=3)
accuracy(fore_ana_in,FLX107_in)
accuracy(FLX107_myforeana_3,FLX107_out)
summary(FLX107_modana)

# Total

Total_modann <- ets(Total_in, model="ANN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ann_in<-fitted(Total_modann,h=3)
accuracy(fore_ann_in,Total_in)

Total_modaan <- ets(Total_in, model="AAN", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aan_in<-fitted(Total_modaan,h=3)
accuracy(fore_aan_in,Total_in)

Total_modana <- ets(Total_in, model="ANA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_ana_in<-fitted(Total_modana,h=3)
accuracy(fore_ana_in,Total_in)

Total_modaaa <- ets(Total_in, model="AAA", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=3)
fore_aaa_in<-fitted(Total_modaaa,h=3)
accuracy(fore_aaa_in,Total_in)

Total_modana_refit <- ets(Total_ts, model=Total_modana, use.initial.values=TRUE)
Total_myforeana_3 <- fitted(Total_modana_refit,h=3)
accuracy(fore_ana_in,Total_in)
accuracy(Total_myforeana_3,Total_out)
summary(Total_modana)

# 2. 1 week and 5 week RMSE for each best model

FLX012_myforeana_1 <- fitted(FLX012_modana_refit,h=1)
FLX012_myforeana_5 <- fitted(FLX012_modana_refit,h=5)
accuracy(FLX012_myforeana_1,FLX012_out)
accuracy(FLX012_myforeana_5,FLX012_out)

FLX019_myforeana_1 <- fitted(FLX019_modana_refit,h=1)
FLX019_myforeana_5 <- fitted(FLX019_modana_refit,h=5)
accuracy(FLX019_myforeana_1,FLX019_out)
accuracy(FLX019_myforeana_5,FLX019_out)

FLX022_myforeana_1 <- fitted(FLX022_modana_refit,h=1)
FLX022_myforeana_5 <- fitted(FLX022_modana_refit,h=5)
accuracy(FLX022_myforeana_1,FLX022_out)
accuracy(FLX022_myforeana_5,FLX022_out)

FLX026_myforeana_1 <- fitted(FLX026_modana_refit,h=1)
FLX026_myforeana_5 <- fitted(FLX026_modana_refit,h=5)
accuracy(FLX026_myforeana_1,FLX026_out)
accuracy(FLX026_myforeana_5,FLX026_out)

FLX078_myforeana_1 <- fitted(FLX078_modana_refit,h=1)
FLX078_myforeana_5 <- fitted(FLX078_modana_refit,h=5)
accuracy(FLX078_myforeana_1,FLX078_out)
accuracy(FLX078_myforeana_5,FLX078_out)

FLX081_myforeana_1 <- fitted(FLX081_modana_refit,h=1)
FLX081_myforeana_5 <- fitted(FLX081_modana_refit,h=5)
accuracy(FLX081_myforeana_1,FLX081_out)
accuracy(FLX081_myforeana_5,FLX081_out)

FLX102_myforeana_1 <- fitted(FLX102_modana_refit,h=1)
FLX102_myforeana_5 <- fitted(FLX102_modana_refit,h=5)
accuracy(FLX102_myforeana_1,FLX102_out)
accuracy(FLX102_myforeana_5,FLX102_out)

FLX105_myforeana_1 <- fitted(FLX105_modana_refit,h=1)
FLX105_myforeana_5 <- fitted(FLX105_modana_refit,h=5)
accuracy(FLX105_myforeana_1,FLX105_out)
accuracy(FLX105_myforeana_5,FLX105_out)

FLX107_myforeana_1 <- fitted(FLX107_modana_refit,h=1)
FLX107_myforeana_5 <- fitted(FLX107_modana_refit,h=5)
accuracy(FLX107_myforeana_1,FLX107_out)
accuracy(FLX107_myforeana_5,FLX107_out)

Total_myforeana_1 <- fitted(Total_modana_refit,h=1)
Total_myforeana_5 <- fitted(Total_modana_refit,h=5)
accuracy(Total_myforeana_1,Total_out)
accuracy(Total_myforeana_5,Total_out)

# Calculating safety stock for L = 1,3,5

#FLX012
accuracy(FLX012_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX012_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX012_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX019
accuracy(FLX019_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX019_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX019_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX022
accuracy(FLX022_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX022_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX022_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX026
accuracy(FLX026_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX026_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX026_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX078
accuracy(FLX078_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX078_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX078_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX081
accuracy(FLX081_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX081_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX081_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX102
accuracy(FLX102_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX102_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX102_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX105
accuracy(FLX105_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX105_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX105_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#FLX107
accuracy(FLX107_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(FLX107_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(FLX107_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)

#Total
accuracy(Total_myforeana_1,FLX012_out)[2]*1.64*sqrt(2)
accuracy(Total_myforeana_3,FLX012_out)[2]*1.64*sqrt(4)
accuracy(Total_myforeana_5,FLX012_out)[2]*1.64*sqrt(6)


## II. Dish Network Data

# Clear environment
rm(list=ls())

#Load forecast package and readxl package
library(forecast)

# Load Dish data
Dish_data <- read.csv("DishWeekly.csv")
Dish_data$X.Week.of.Ra.Receipt.Date <- as.Date(Dish_data$X.Week.of.Ra.Receipt.Date,format="%m/%d/%Y")
Dish_data

# Convert all to time series object
Dish_ts<-ts(Dish_data$Records, start=c(2014,3),frequency=52)
Dish_ts

# Create Train and Test data for model
Dish_in <- window(Dish_ts,end=c(2014,16))
Dish_out <- window(Dish_ts,start=c(2014,17))
Dish_in
Dish_out

# Create model

Dish_modzzz <- ets(Dish_in, model="ZZZ", damped=FALSE, lower=c(.05,.05,.05),upper=c(.3,.3,.3), opt.crit="amse", nmse=1)
fore_zzz_in<-fitted(Dish_modzzz,h=1)
accuracy(fore_zzz_in,Dish_in)

Dish_modzzz_refit <- ets(Dish_ts, model=Dish_modzzz, use.initial.values=TRUE)
Dish_myforezzz_3 <- fitted(Dish_modzzz_refit,h=1)
accuracy(Dish_myforezzz_3,Dish_out)
summary(Dish_modzzz)

Dish_modzzz_refit$fitted

