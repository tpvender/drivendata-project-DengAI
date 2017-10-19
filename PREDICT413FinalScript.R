train_features <- read.csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_features_train.csv")
head(train_feaures)
train_labels <- read.csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_labels_train.csv")
merge_train <- merge(x=train_features, y=train_labels, by=c("city","year","weekofyear"), all=TRUE)

newtest <-read.csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_features_test.csv")

#create function to find missing variable
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}
propmiss(merge_train)

#separate data by city
sj_train <- subset(merge_train, city=="sj")
iq_train <- subset(merge_train, city=="iq")
iq_train_ordered<- iq_train[order(as.Date(iq_train$week_start_date, format ="%Y/%m/%d")),,drop=FALSE]
sj_train_ordered<- sj_train[order(as.Date(sj_train$week_start_date, format ="%Y/%m/%d")),,drop=FALSE]

#summary statistics
library(pastecs)
stat.desc(sj_train_ordered$total_cases)
stat.desc(iq_train_ordered$total_cases)

#boxplot for dist of total cases by week of year for each city
boxplot(total_cases ~ weekofyear, data=sj_train, main= "San Juan total cases by week of year")
boxplot(total_cases ~ weekofyear, data=iq_train, main= "Iquitos total cases by week of year")

hist(sj_train_ordered$total_cases)
hist(iq_train_ordered$total_cases)

#seasonal plot
newmatiq=xtabs(total_cases~weekofyear+year, data=iq_train_ordered)
matplot(newmatiq, type=c("b"), pch=1,col=1:15,xlim=c(1,53), ylim=c(0,120), xlab="Week of Year", ylab="Total Cases")matplot(newmatiq, type=c("b"), pch=1,col=1:15,xlim=c(1,53), ylim=c(0,120), xlab="Week of Year", ylab="Total Cases", main="Iquitos")
legend(0, 120, inset=c(5,0), legend=2000:2010,col=1:15,pch=1, ncol=4)

newmatsj=xtabs(total_cases~weekofyear+year, data=sj_train_ordered)
matplot(newmatsj, type=c("b"), pch=1,col=1:15,xlim=c(1,53), ylim=c(0,500), xlab="Week of Year", ylab="Total Cases", main="San Juan")
legend(0, 500, inset=c(5,0), legend=1990:2008,col=1:15,pch=1, ncol=4)

library(stats)
library(forecast)
library(lubridate)
#timeseries
ts_iq <- ts(iq_train_ordered$total_cases, freq=365.25/7, start=decimal_date(ymd("2000-07-01")))
ts_sj <- ts(sj_train_ordered$total_cases, freq=365.25/7, start=decimal_date(ymd("1990-04-30")))

plot.ts(ts_sj)
plot.ts(ts_iq)

#split training and test set
indexesiq = sample(1:nrow(iq_train_ordered), size=0.2*nrow(iq_train_ordered))
indexessj = sample(1:nrow(sj_train_ordered), size=0.2*nrow(sj_train_ordered))

test_iq = iq_train_ordered[indexesiq,]
train_iq = iq_train_ordered[-indexesiq,]

test_sj = sj_train_ordered[indexessj,]
train_sj = sj_train_ordered[-indexessj,]

#choose predictors
regress_selsj <- glm(total_cases~ndvi_ne
                     +ndvi_nw
                     +ndvi_se
                     +ndvi_sw
                     +precipitation_amt_mm
                     +reanalysis_air_temp_k
                     +reanalysis_avg_temp_k
                     +reanalysis_dew_point_temp_k
                     +reanalysis_max_air_temp_k
                     +reanalysis_min_air_temp_k
                     +reanalysis_precip_amt_kg_per_m2
                     +reanalysis_relative_humidity_percent
                     +reanalysis_sat_precip_amt_mm
                     +reanalysis_specific_humidity_g_per_kg
                     +reanalysis_tdtr_k
                     +station_avg_temp_c
                     +station_diur_temp_rng_c
                     +station_max_temp_c
                     +station_min_temp_c
                     +station_precip_mm, data=na.omit(sj_train_ordered))
sjmod <- step(regress_selsj)

regress_seliq <- glm(total_cases~ndvi_ne
                     +ndvi_nw
                     +ndvi_se
                     +ndvi_sw
                     +precipitation_amt_mm
                     +reanalysis_air_temp_k
                     +reanalysis_avg_temp_k
                     +reanalysis_dew_point_temp_k
                     +reanalysis_max_air_temp_k
                     +reanalysis_min_air_temp_k
                     +reanalysis_precip_amt_kg_per_m2
                     +reanalysis_relative_humidity_percent
                     +reanalysis_sat_precip_amt_mm
                     +reanalysis_specific_humidity_g_per_kg
                     +reanalysis_tdtr_k
                     +station_avg_temp_c
                     +station_diur_temp_rng_c
                     +station_max_temp_c
                     +station_min_temp_c
                     +station_precip_mm, data=na.omit(iq_train_ordered))
iqmod <- step(regress_seliq)
sjregressors = subset(sj_train_ordered, select=c("ndvi_nw","reanalysis_avg_temp_k","station_diur_temp_rng_c","reanalysis_tdtr_k","reanalysis_dew_point_temp_k","station_max_temp_c","reanalysis_specific_humidity_g_per_kg","reanalysis_max_air_temp_k"))
iqregressors = subset(iq_train_ordered, select=c("ndvi_se","ndvi_nw","reanalysis_precip_amt_kg_per_m2","station_min_temp_c","ndvi_sw","reanalysis_dew_point_temp_k","reanalysis_specific_humidity_g_per_kg"))

#check for missing values 
propmiss(iqregressors)
propmiss(sjregressors)

#impute missing
library(imputeMissings)
rforestsj = compute(sjregressors, method="randomForest")
rforestsiq = compute(iqregressors, method="randomForest")
imp_sjregressors = impute(sjregressors, object=rforestsj)
imp_iqregressors = impute(iqregressors, object=rforestsiq)


#work with test data
sj_test <- subset(newtest, city=="sj")
iq_test <- subset(newtest, city=="iq")
iq_test_ordered<- iq_test[order(as.Date(iq_test$week_start_date, format ="%Y/%m/%d")),,drop=FALSE]
sj_test_ordered<- sj_test[order(as.Date(sj_test$week_start_date, format ="%Y/%m/%d")),,drop=FALSE]
test_sjregressors = subset(sj_test_ordered, select=c("ndvi_nw","reanalysis_avg_temp_k","station_diur_temp_rng_c","reanalysis_tdtr_k","reanalysis_dew_point_temp_k","station_max_temp_c","reanalysis_specific_humidity_g_per_kg","reanalysis_max_air_temp_k"))
test_iqregressors = subset(iq_test_ordered, select=c("ndvi_se","ndvi_nw","reanalysis_precip_amt_kg_per_m2","station_min_temp_c","ndvi_sw","reanalysis_dew_point_temp_k","reanalysis_specific_humidity_g_per_kg"))
test_rforestsj = compute(test_sjregressors, method="randomForest")
test_rforestsiq = compute(test_iqregressors, method="randomForest")
test_imp_sjregressors = impute(test_sjregressors, object=test_rforestsj)
test_imp_iqregressors = impute(test_iqregressors, object=test_rforestsiq)

#check distribtutions of variables
library(reshape2)
library(ggplot2)
imp_iqregressorsM <- melt(imp_iqregressors)
ggplot(data = imp_iqregressorsM, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

imp_sjregressorsM <- melt(imp_sjregressors)
ggplot(data = imp_sjregressorsM, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#arima
arima_sj <- auto.arima(ts_sj, xreg=imp_sjregressors, stepwise=FALSE, parallel = TRUE)
fcast_arimasj <- forecast(arima_sj, xreg=imp_sjregressors, newxreg=test_imp_sjregressors)
write.csv(fcast_arimasj, "/Users/Taylor/Desktop/fcast_arimasj.csv")
#add column name "Date" to first column and rename csv file
arima_sjdate <- read.csv("/Users/Taylor/Desktop/fcast_arimasjdate.csv")
arima_sjdate$Date <- format(date_decimal(arima_sjdate$Date), "%Y-%m-%d")
write.csv(arima_sjdate, "/Users/Taylor/Desktop/arimasjdate.csv")

arima_iq <- auto.arima(ts_iq, xreg=imp_iqregressors, stepwise=FALSE, parallel = TRUE)
fcast_arimaiq <- forecast(arima_iq, xreg=imp_iqregressors, newxreg=test_imp_iqregressors)
write.csv(fcast_arimaiq, "/Users/Taylor/Desktop/fcast_arimaiq.csv")
arima_iqdate <- read.csv("/Users/Taylor/Desktop/fcast_arimaiqdate.csv")
arima_iqdate$Date <- format(date_decimal(arima_iqdate$Date), "%Y-%m-%d")
write.csv(arima_iqdate, "/Users/Taylor/Desktop/arimaiqdate.csv")

#neural net
NNsj <- nnetar(ts_sj, xreg=imp_sjregressors)
NNsj_fcast <- forecast(NNsj, xreg=imp_sjregressors, newxreg=test_imp_sjregressors)
NNsj_fcastdf <- as.data.frame(NNsj_fcast)
NNsj_fcastdf$Date <- rownames(NNsj_fcastdf) 
NNsj_fcastdf$Date <- as.numeric(NNsj_fcastdf$Date)
NNsj_fcastdf$Date <- format(date_decimal(NNsj_fcastdf$Date), "%Y-%m-%d")
NNsj_fcastdf$`Point Forecast`<- as.integer(NNsj_fcastdf$`Point Forecast`)

NNiq <- nnetar(ts_iq, xreg=imp_iqregressors)
NNiq_fcast <- forecast(NNiq, xreg=imp_iqregressors, newxreg=test_imp_iqregressors)
NNiq_fcastdf <- as.data.frame(NNiq_fcast)
NNiq_fcastdf$Date <- rownames(NNiq_fcastdf) 
NNiq_fcastdf$Date <- as.numeric(NNiq_fcastdf$Date)
NNiq_fcastdf$Date <- format(date_decimal(NNiq_fcastdf$Date), "%Y-%m-%d")
NNiq_fcastdf$`Point Forecast`<- as.integer(NNiq_fcastdf$`Point Forecast`)

write.csv(NNiq_fcastdf, "/Users/Taylor/Desktop/NNiq_fcastdf.csv")
write.csv(NNsj_fcastdf, "/Users/Taylor/Desktop/NNsj_fcastdf.csv")

#fix entire train set for both cities
sj_train_orderedsub = subset(sj_train_ordered, select=c("total_cases", "ndvi_nw","reanalysis_avg_temp_k","station_diur_temp_rng_c","reanalysis_tdtr_k","reanalysis_dew_point_temp_k","station_max_temp_c","reanalysis_specific_humidity_g_per_kg","reanalysis_max_air_temp_k"))
iq_train_orderedsub = subset(iq_train_ordered, select=c("total_cases", "ndvi_se","ndvi_nw","reanalysis_precip_amt_kg_per_m2","station_min_temp_c","ndvi_sw","reanalysis_dew_point_temp_k","reanalysis_specific_humidity_g_per_kg"))

propmiss(sj_train_orderedsub)
propmiss(iq_train_orderedsub)

rforestsj_train = compute(sj_train_orderedsub, method="randomForest")
rforestsiq_train = compute(iq_train_orderedsub, method="randomForest")
imp_sjsub_train = impute(sj_train_orderedsub, object=rforestsj_train)
imp_iqsub_train = impute(iq_train_orderedsub, object=rforestsiq_train)

nb_sj <- glm.nb(total_cases~
                  ndvi_nw
                +reanalysis_avg_temp_k
                +reanalysis_dew_point_temp_k
                +reanalysis_max_air_temp_k
                +reanalysis_specific_humidity_g_per_kg
                +reanalysis_tdtr_k
                +station_diur_temp_rng_c
                +station_max_temp_c, data=imp_sjsub_train, link=log)
summary(nb_sj)

sj_bn_predictions <- test_imp_sjregressors
sj_bn_predictions$total_cases <-predict(nb_sj, sj_bn_predictions, type="response")
sj_bn_predictions$total_cases <- as.integer(sj_bn_predictions$total_cases)
write.csv(sj_bn_predictions, "/Users/Taylor/Desktop/sj_bn_predictions.csv")

nb_iq <- glm.nb(total_cases~
    ndvi_nw
    +ndvi_se
    +ndvi_sw
    +reanalysis_dew_point_temp_k
    +reanalysis_precip_amt_kg_per_m2
    +reanalysis_specific_humidity_g_per_kg
    +station_min_temp_c, data=imp_iqsub_train, link=log)

summary(nb_iq)

iq_bn_predictions <- test_imp_iqregressors
iq_bn_predictions$total_cases <-predict(nb_iq, iq_bn_predictions, type="response")
iq_bn_predictions$total_cases <- as.integer(iq_bn_predictions$total_cases)
write.csv(iq_bn_predictions, "/Users/Taylor/Desktop/iq_bn_predictions.csv")
