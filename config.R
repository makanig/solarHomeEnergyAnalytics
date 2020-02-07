# ---- config_preamble
## setup code that is required for config.R
## to run but not for main.Rmd

# ---- first_config_chunk
library(tidyverse)
library(forecast)
library(plotly)
library(data.table)
library(lubridate)
library(tidyquant)
library(timetk)
library(zoo)
library(tsibble)
library(imputeTS)
library(geosphere)
library(ggcorrplot)
library(swirl)
library(FinCal)
library(tidyr)
library(TSstudio)

nest <- nest_legacy
unnest <- unnest_legacy


#dataDir = "C:/Users/gauta/repos/solarHomeEnergyAnalytics/data2"
dataDir = "data"
gasSkipLines = 5
electricSkipLines = 5

useRDSFiles = T # use saved RDSFiles

gasUsageDaily = "pge_gas_interval_data.csv"
solarProductionDaily = "Enphase_system_energy_20150708_to_20190808.csv"
solarProductionHourly = "system_power_20190831.csv"

pgeElectricBilling = "pge_electric_billing_data.csv"
pgeGasBilling = "pge_gas_billing_data.csv"
weatherData = "WeatherDailyData_2014-Current.csv"

# PG&E plan as of 2019
gasTier1Pricing = 1.284 + 0.09  # $ per Therm, the 0.09 is a surcharge
gasTier1DailyAllocation = 0.59
gasTier2Pricing = 1.884 + 0.09  

myLatitude = 37.877


solarInvestment  = 6513  # $$ cost of your solar upfront investment post tax credit
solarInstallDate = as.Date("2015/07/08")
solarGenDataBadValues = c(0, 14873)
maxDailySolarGen = 24000 # any readings over this are suspect and discarded
minDailySolarGen = 1 # constrain this to allow for log transforms
solarSuspectDateRange = seq(from=as.Date("2018/6/2"), to=as.Date("2019/1/16"), by='day')
limitSolarDate = as.Date("2018/6/2") # limit to this value because later date values are suspect
lastGoodSolarReading = as.Date("2019-08-02")
electricCarDeployDate = as.Date("2018-10-15")


# this is used to rank the days as a percentage, used as a predictor for solar generation
yDayLongest = yday("2018-06-21")
yDayShortest = yday("2018-12-21")

# get the dayOfYear on a scale of 0-1 based on length of day
rankDay <- function(yDay) {
  y = min(abs(yDay - yDayShortest), abs(yDay - yDayShortest + 365))
  y/abs(yDayLongest - yDayShortest)
}

# calculate the estimated cost based on aggregate usage for a specified number of days
calculateMonthlyCharge <- function(usage, numDays) {
  
  retVal = NA
  
  if (!is.na(usage)) {
    avgDaily= usage/numDays
    
    dailyAvgCharge = 0
    if (avgDaily < gasTier1DailyAllocation) {
      dailyAvgCharge = avgDaily*gasTier1Pricing
    } else {
      dailyAvgCharge = gasTier1DailyAllocation*gasTier1Pricing + (avgDaily - gasTier1DailyAllocation)*gasTier2Pricing
    }
    retVal = dailyAvgCharge*numDays
  }
  retVal
}


# get raw Solar generation data
getRawSolarDf <- function() {
  
  # get   generation
  solarDf = tbl_df(read.csv(solarProductionDaily,  stringsAsFactors = F))
  colnames(solarDf) <- c("DATE", "energyProducedWh")
  
  # skip the total row
  solarDf = solarDf %>% mutate(DATE = as.Date(DATE,"%Y-%m-%d")) %>% # %>% filter(DATE <= lastSolarReading) %>%
    na.omit()
  #solarDf = addMissingTimeValues(solarDf, "DATE", "energyProducedWh") 
  
}



# Get cleaned and filtered solar generation
# For missing/suspect values, get the mean values for that specific day of the year
# 
getSolarDf <- function(clean=T) {
  
  # get   generation
  solarDf = tbl_df(read.csv(solarProductionDaily,  stringsAsFactors = F))
  colnames(solarDf) <- c("DATE", "energyProducedWh")
  
  
  # skip the total row
  solarDf = solarDf %>% mutate(DATE = as.Date(DATE,"%Y-%m-%d")) %>% # %>% filter(DATE <= lastSolarReading) %>%
    na.omit()
  solarDf = addMissingTimeValues(solarDf, "DATE", "energyProducedWh") 
  
  solarDf = solarDf %>% mutate(yDay = yday(DATE),
                               year = year(DATE)) %>% rowwise() %>% 
    mutate(dRank = rankDay(yDay),
           DATE_Numeric = as.numeric(DATE)) %>% ungroup()
  
  # # check for outliers and bad data
  # (ggplot(solarDf, aes(x = DATE, y = energyProducedWh)) + geom_point(color = "green") +
  #     ggtitle("Solar generation  - Enphase inverter data")) %>% ggplotly()
  
  # look at suspect values, and also any values above a threshold
  solarDfSuspect = solarDf %>% filter (energyProducedWh %in% solarGenDataBadValues| 
                                         energyProducedWh > maxDailySolarGen |
                                         DATE %in% solarSuspectDateRange |
                                         is.na(energyProducedWh))
  # 
  # (ggplot(solarDfSuspect, aes(x = DATE, y = energyProducedWh), color="green") + geom_point(color="red") + 
  #     ggtitle("Solar generation  - suspect Enphase inverter data")) %>% ggplotly()
  # 
  
  # remove these suspect values from solarDf
  solarDf = solarDf %>% anti_join(solarDfSuspect, by = "DATE")
  
  solarDfSuspectFixed = solarDfSuspect  %>% rowwise() %>% mutate(energyProducedWh = 
                                                                   meanReadingByYday(yDay, "yDay", "energyProducedWh", solarDf)) %>% ungroup()
  
  solarDf = bind_rows(solarDf, solarDfSuspectFixed) %>% arrange(DATE)
  
  # constrain values between max and min
  solarDf$energyProducedWh <- pmax(minDailySolarGen+1, 
                                   pmin( solarDf$energyProducedWh, maxDailySolarGen+1 ))
  # add a log transform to this between the upper and lower range of production so ARIMA forecasts work
  # https://robjhyndman.com/hyndsight/forecasting-within-limits/
  solarDf = solarDf %>% mutate(energyProducedWhLog = 
                                 log((energyProducedWh - minDailySolarGen)/(maxDailySolarGen-energyProducedWh)))
  
}


# Specific to solar installation. Figure out the  production by type (peak/offpeak/partPeak) so you can 
# calculate $$ contribution of solar generation

getTimeOfDayPercentages <- function() {
  
  # 5 min generation data
  solarHourlyDf = tbl_df(read.csv(solarProductionHourly,  stringsAsFactors = F))
  colnames(solarHourlyDf) <- c("DATE", "energyProducedWh")
  
  solarHourlyDf$DATE = as.POSIXct(solarHourlyDf$DATE, format = "%Y-%m-%d %H:%M:%S")
  
  solarHourlyDf = solarHourlyDf %>% mutate(hour = hour(DATE),
                                           minute = minute(DATE),
                                           DATE = date(DATE))
  
  
  solarHourlyDf = solarHourlyDf %>% group_by(DATE, hour) %>%
    summarise(energyProducedWh = sum(energyProducedWh)) %>%
    mutate(wday = weekdays(DATE)) %>% mutate(type = "offPeak") %>% ungroup()
  
  
  solarHourlyDf = solarHourlyDf %>% mutate(
    type = case_when(
      hour < 10 | hour >= 21 ~ "offPeak",
      hour >= 10 & hour < 13 ~ "partPeak",
      hour >= 19 & hour < 21 ~ "partPeak",
      hour >= 13 & hour < 19 ~ "peak"
    )
  )
  # Adjust for weekends
  
  setDT(solarHourlyDf)[wday == "Saturday" | wday == "Sunday",
                       type := "offPeak"]
  
  setDT(solarHourlyDf)[(wday == "Saturday" |
                          wday == "Sunday") & (hour >= 17 & hour < 20),
                       type := "partPeak"]
  
  solarHourlyDf %>% group_by(DATE, type) %>% summarise(energyProducedWhTotal = sum(energyProducedWh))
  
  summerPercent = solarHourlyDf %>% group_by(type) %>%
    summarise(energyProducedWhTotal = sum(energyProducedWh)) %>% ungroup() %>%
    mutate(typePercentSummer = energyProducedWhTotal / sum(energyProducedWhTotal)) %>%
    select(type, typePercentSummer)
  
  # Don't have the winter 5 min intervals, just use a reasonable approximation
  # Winter part Peak is 3 hours for 5 days a week(1-3 actual). 2*5/(7*7) ~ 15%-20%
  winterPercent = as_tibble(list(
    type = c("peak", "partPeak", "offPeak"),
    typePercentWinter = c(0, 0.15, 0.85)
  ))
  genPercentByType = summerPercent %>% left_join(winterPercent, by = "type")
  
}


# # Back-transform forecasts
# fc$mean <- (b-a)*exp(fc$mean)/(1+exp(fc$mean)) + a
backTransformLogValues <- function(varVector) {
  (maxDailySolarGen - minDailySolarGen)*exp(varVector)/(1+exp(varVector))
}


# Used on time series data sets to ensure missing time series rows are added back in
# Sets the missing time series values to NA

addMissingTimeValues  <- function(df, dateColumnName, valueColumnName) {
  
  # Add in missing times from the time series
  df1.zoo <- zoo(x = as.vector(df[,valueColumnName]), order.by = unlist(df[,dateColumnName]))
  
  df2 <- merge(df1.zoo,zoo(x=NULL,seq(start(df1.zoo),end(df1.zoo),by=1)), all=TRUE)
  
  fixedDf = data.frame(as.Date(zoo::index(df2)),zoo::coredata(df2))
  colnames(fixedDf) = c(dateColumnName, valueColumnName)
  tbl_df(fixedDf)
}

# Given a dayOfYear, figures out in the provided df the mean of the valueColumn 
# yDayColumn, valueColumn are string column names
#
meanReadingByYday <- function(dayOfYear, yDayColumn, valueColumn, df) { 
  # df= solarDf
  # dayOfYear = 1
  # valueColumn = as.symbol("energyProducedWh")
  valueColumn = as.symbol(valueColumn)
  yDayColumn = as.symbol(yDayColumn)
  
  # print(paste("DayofYear:", dayOfYear, " numRowsDf ", nrow(df)))
  #df1 = df %>% filter(yDay == dayOfYear) %>% summarise(s = mean(!!valueColumn))
  
  df1 = df %>% filter(!!yDayColumn == dayOfYear) %>% summarise(s = mean(!!valueColumn))
  # print(paste(" numRowsDf ", nrow(df1)))
  # print(df1$s)
  df1$s
}

# From StackOverflow - useful utility function to nest multiple attributes
# Uses nest_legacy (defined in beginning of this file)
# Modified to add missing time value rows and use moving average to impute NAs
# https://stackoverflow.com/questions/48232638/dplyr-programming-how-to-access-columns-of-x-in-map
nest_AddMissingRows_xts <- function(df_in, nest_var, t_var, value_var) {
  
  require(rlang)
  # 
  # df_in = weatherDfGather
  # nest_var = "attribute"
  # t_var = "DATE"
  # value_var = "value"
  
  dfNest = df_in %>% group_by(.dots = nest_var) %>%  nest(.key = "data.tbl") %>% ungroup()
  
  dfTblFixed = dfNest %>% group_by(.dots = nest_var) %>% 
    mutate(data.tblFixed = map(.x = data.tbl, 
                               .f       = addMissingTimeValues, 
                               dateColumnName = t_var, 
                               valueColumnName = value_var)) %>% ungroup()
  
  
  
  #https://stackoverflow.com/questions/49371260/using-variables-as-arguments-in-summarize
  t_varSym= as.symbol(t_var)
  
  #tMaxTsTibble = as_tsibble(tMaxTs[[1]] )
  
  # print(paste("is.na[1] with missing time values", sum(is.na(dfTblFixed$data.tblFixed[[1]]))))
  
  dfTblFixedTs = dfTblFixed  %>% mutate(data.ts = map(.x = data.tblFixed, 
                                                      .f       = tk_ts,
                                                      select = c(-!!t_var),
                                                      start = as.Date(data.tblFixed[[1]]$DATE[1]),
                                                      frequency = 365))
  
  # # print(paste("is.na[1] in ts", sum(is.na(dfTblFixedTs$data.ts[[1]]))))
  # 
  dfTblFixedTsNoNas =   dfTblFixedTs %>%  mutate(data.tsClean =  map(.x = data.ts,
                                                                     .f       = imputeTS::na_ma,
                                                                     k = 4))
  
  # print(paste("is.na[1] in tsNoNas", sum(is.na(dfTblFixedTsNoNas$data.tsClean[[1]]))))
  
  
  dfTblFixedTsNoNas %>% select(-data.ts, -data.tblFixed, -data.tbl )
  
}


# Set NA values to the mean on the dataframe
# Usage: fixNAs(df[ valueColumns]
# changes Date type so don't pass in the full dataset
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))


lmPredictCost <- function(o, costDf, monthlyDf) {
  # print(paste("oFormula model:", costDf$model[1], " o:", o))
  # print(paste("nrow(monthlyDf):", nrow(monthlyDf)))
  # print(paste("nrow(costDf):", nrow(costDf)))
  # 
  if (costDf$model[1] == "linear") {
    f = as.formula(sprintf("%s ~ DATE", o))
  } else {
    f = as.formula(sprintf("%s ~ log(as.numeric(DATE))", o))
  }
  
  fit = lm(f, data= costDf)
  
  costDfFull = predict(fit, newdata=monthlyDf, type="response") %>% tibble::enframe() # %>%
  costDfFull$DATE = monthlyDf$DATE
  costDfFull$name = o
  
  costDfFull
}
# Templatized way of charting different costKwh profiles
predictMonthlyGen <- function(costDf, genMonthlyDf) {
  
  # print(paste(costDf$costProfile, " predictMonthlyGen: entry"))
  
  #costDf = electricityCostDf3
  costOutcomes = as.vector(colnames(costDf %>% select(-DATE, -costProfile, -model)))
  
  responseList = map(costOutcomes, .f=lmPredictCost,  costDf = costDf, monthlyDf = genMonthlyDf)
  
  r = rbindlist(responseList)
  KwhCostDf = spread(r, key=name, value=value)
  
  KwhCostDf$monthlyGenKwh = genMonthlyDf$monthlyGenKwh
  KwhCostDf$month = month(KwhCostDf$DATE)
  KwhCostDf$costProfile = costDf$costProfile[1]
  
  summerKwhCostDf  = KwhCostDf %>% filter(month >= 5 & month < 11)
  winterKwhCostDf = KwhCostDf %>% anti_join(summerKwhCostDf, by="DATE")
  
  touDf = getTimeOfDayPercentages()
  touDfWinter  = touDf %>% select(type, typePercentWinter) %>% spread(type, typePercentWinter)
  touDfSummer  = touDf %>% select(type, typePercentSummer) %>% spread(type, typePercentSummer)
  
  summerKwhCostDf = summerKwhCostDf %>% mutate(monthlySavings = 
                                                 monthlyGenKwh*(costPerKwhOffPeak * touDfSummer$offPeak +
                                                                  costPerKwhPartPeak * touDfSummer$partPeak +
                                                                  costPerKwhPeak * touDfSummer$peak ))
  
  winterKwhCostDf = winterKwhCostDf %>% mutate(monthlySavings = 
                                                 monthlyGenKwh*(costPerKwhOffPeak * touDfWinter$offPeak +
                                                                  costPerKwhPartPeak * touDfWinter$partPeak +
                                                                  costPerKwhPeak * touDfWinter$peak ))
  
  retDf = as_tibble(rbind(summerKwhCostDf, winterKwhCostDf))
  
  #print(paste("CostProfile:", retDf$costProfile[1], " totalSavings:", sum(retDf$monthlySavings)))
  retDf
  
}

# ---- second_config_chunk
## Start of program
## Check raw data - exploratory analysis
setwd(dataDir)

# get   generation
solarDf = tbl_df(read.csv(solarProductionDaily,  stringsAsFactors = F))
colnames(solarDf) <- c("DATE", "energyProducedWh")

# skip the total row
solarDf = solarDf %>% mutate(DATE = as.Date(DATE,"%Y-%m-%d")) %>%   na.omit()

solarDfTsibble = solarDf %>% select(DATE, energyProducedWh) %>% as_tsibble()
ts_plot(solarDfTsibble, 
        title = "Daily Solar Generation",
        Ytitle = "Wh Daily Production",
        # Xtitle = "Date",
        slider = TRUE)


# ---- third_config_chunk
# Look at the fixed data
setwd(dataDir)
solarDf = getSolarDf()

solarDfTsibble = solarDf %>% select(DATE, energyProducedWh) %>% as_tsibble()
ts_plot(solarDfTsibble, 
        title = "Daily Solar Generation",
        Ytitle = "Wh Daily Production",
        # Xtitle = "Date",
        slider = TRUE)

# Get the daily temperature and precipitation
weatherDf = tbl_df(read.csv(weatherData, stringsAsFactors = F)) %>% 
  select( DATE, TMAX, TMIN, PRCP) 

weatherDf$DATE = as.Date(weatherDf$DATE,"%Y-%m-%d")
weatherDf$TEMP = (weatherDf$TMAX + weatherDf$TMIN)/2


# break up each of the attributes with gather
weatherDfGather = weatherDf %>% gather("attribute", "value", -DATE)
weatherDfGatherTs  = nest_AddMissingRows_xts(weatherDfGather, "attribute", "DATE", "value") 
weatherDfDaily = weatherDfGatherTs %>% spread(key = attribute, value=data.tsClean) %>% unnest() %>%
  mutate(DATE=tk_index(weatherDfGatherTs$data.tsClean[[1]] , timetk_idx = TRUE)) 


# include daily weather to Solar dataframe
solarDf = solarDf %>% inner_join(weatherDfDaily) %>% rowwise() %>% 
  mutate(DayLength = daylength(myLatitude, DATE))


# generate time series and plot seasonal
solarTs = ts(solarDf$energyProducedWh, start=c(solarDf$year[[1]], solarDf$yDay[[1]]), frequency = 365)
ts_seasonal(solarTs)




# ---- fourth_config_chunk
# examine the plot below for stationarity and order 


setwd(dataDir)
solarTsAdj = solarTs %>% stl(s.window='periodic')%>% seasadj() 
autoplot(solarTsAdj) # shows non-stationary, so use diff()
solarTsAdj %>% diff() %>% ggtsdisplay(main="") 

# do the same for the log transformed, bounded values
# To forecast with ARIMA, first look at the profile of entire series, the ts decompose to seasonal shows tiny weekly
# fluctuations, look for a better model
solarTsLog = ts(solarDf$energyProducedWhLog, start=c(solarDf$year[[1]], solarDf$yDay[[1]]), frequency = 365)
# comp = decompose(solarTsLog)
# plot(comp)
# # the seasonal pattern above is not too good, look for a better one


# examine the log plot below for stationarity and order 
solarTsLogAdj = solarTsLog %>% stl(s.window='periodic')%>% seasadj() 
autoplot(solarTsLogAdj) # shows non-stationary, so use diff()
solarTsLogAdj %>% diff() %>% ggtsdisplay(main="") 

# ---- fifth_config_chunk
setwd(dataDir)
nReadings = nrow(solarDf)

# Use this to fit a sinusoidal pattern
solarDf$xc<-cos(2*pi*solarDf$DATE_Numeric/365)
solarDf$xs<-sin(2*pi*solarDf$DATE_Numeric/365)

# Split into training and test
halfWay = floor(nReadings/2)
trainingDf = as_tibble(solarDf[1:halfWay,]) %>% na.omit()
testDf = as_tibble(solarDf[halfWay+1:nReadings,]) %>% na.omit()
trainingLen = halfWay
testLen = nReadings - (halfWay+1) +1

# ---- sixth_config_chunk
setwd(dataDir)
decompose_df <- glm(energyProducedWhLog ~  xc+PRCP +DATE_Numeric*xc +
                      TMAX + TMIN, data=solarDf)

summary(decompose_df)


trend <- coef(decompose_df)[1] + coef(decompose_df)['DATE_Numeric']*solarDf$DATE_Numeric
components <- cbind(
  data = solarTsLog,
  fitted = decompose_df$fitted.values,
  trend = trend,
  season = coef(decompose_df)['xc']*solarDf$xc +
    coef(decompose_df)['xc:DATE_Numeric']*solarDf$xc*solarDf$DATE_Numeric,
  prcp = coef(decompose_df)['PRCP']*solarDf$PRCP,
  TMAX = coef(decompose_df)['TMAX']*solarDf$TMAX,
  TMIN = coef(decompose_df)['TMIN']*solarDf$TMIN,
  remainder = residuals(decompose_df)
)

p = (autoplot(components, facet=TRUE) + ggtitle("GLM model components")) 
p %>% ggplotly()


RSS <- c(crossprod(decompose_df$residuals))
#Mean squared error:
MSE <- RSS / length(decompose_df$residuals)
#Root MSE:
RMSE <- sqrt(MSE)
#Pearson estimated residual variance (as returned by summary.lm):
sig2 <- RSS / decompose_df$df.residual
gof = 1 - (decompose_df$deviance/decompose_df$null.deviance)

print(paste("RMSE:", RMSE, " Pearson residual variance", sig2, " GOF:", gof))

# ---- seventh_config_chunk
setwd(dataDir)
tsStationary = diff(solarTsLog,differences=1)

# Forecast with  ARIMA, use the training set for the fit
solarDfTrainingTsLog = ts(data=trainingDf$energyProducedWhLog, start=c(trainingDf$year[[1]], trainingDf$yDay[[1]]),
                          frequency=365) 


if (useRDSFiles) {
  ar_211_010 = readRDS("arModel.rds")
} else {
  ar_211_010 = Arima(solarDfTrainingTsLog, order = c(2,1,1),seasonal = c(0,1,0))
  saveRDS(ar_211_010, "arModel.rds")
}

summary(ar_211_010)


plot(x=ar_211_010$x, y = ar_211_010$fitted)
checkresiduals(ar_211_010)
# Ljung-Box test
# 
# data:  Residuals from ARIMA(2,1,1)(0,1,0)[365]
# Q* = 226.45, df = 146, p-value = 2.22e-05
# 
# Model df: 3.   Total lags used: 149

if (useRDSFiles) {
  aF = readRDS("aF_Files.rds")
} else {
  aF = forecast(ar_211_010,h = testLen)
  saveRDS(aF, "aF_Files.rds")
}

plot(aF$mean)


#arimaForecastValues = as_tibble(aF$mean)


# GLM Weather independent
# https://stats.stackexchange.com/questions/47840/linear-model-with-log-transformed-response-vs-generalized-linear-model-with-log

weatherIndependentGlmModel = glm(energyProducedWhLog ~ DATE_Numeric +  xc+ DATE_Numeric*xc + DATE_Numeric*xs + xs + 
                                   DATE_Numeric*dRank  + xc*dRank  + xs*dRank, 
                                 # family="gaussian"( link="log" ), 
                                 data = trainingDf)
summary(weatherIndependentGlmModel)




weatherIndependentGlmForecast <- stats::predict(weatherIndependentGlmModel, type = "response", newdata=testDf) %>% 
  tibble::enframe()
weatherIndependentErr <- stats::predict(weatherIndependentGlmModel, newdata=testDf, se = TRUE)





weatherBasedGlmModel = glm(energyProducedWhLog ~  xc+PRCP +DATE_Numeric*xc +
                             TMAX + TMIN, 
                           # energyProducedWhLog ~  DATE_Numeric + dRank + PRCP  +  xc + xc*dRank + DATE_Numeric*xc +
                           #   TMAX + TMIN + DATE_Numeric*xs + DATE_Numeric*dRank + xs*dRank, 
                           #family="gaussian"( link="log" ), 
                           data = trainingDf)
summary(weatherBasedGlmModel) 


weatherBasedGlmForecast <- stats::predict(weatherBasedGlmModel, type = "response", newdata=testDf) %>% 
  tibble::enframe()
weatherBasedErr <- stats::predict(weatherBasedGlmModel, newdata=testDf, se = TRUE)

allTestVals = tibble(DATE = testDf$DATE,
                     ArimaVals= backTransformLogValues(aF$mean),
                     WeatherIndependent =backTransformLogValues(weatherIndependentGlmForecast$value),
                     WeatherBasedSeasonal =backTransformLogValues(weatherBasedGlmForecast$value ))

dfAllTestVals = solarDf %>% dplyr::left_join(allTestVals, by = "DATE") 


r = dfAllTestVals %>%
  select(DATE, energyProducedWh, ArimaVals, WeatherIndependent,  WeatherBasedSeasonal) %>% 
  mutate(Actuals=energyProducedWh) %>% select(-energyProducedWh) %>%
  melt(id.vars = c("DATE"), variable.name="Model")

g <- ggplot(data = r, 
            aes(x=DATE, y=value)) + ggtitle("Solar generation - Model comparison") + 
  xlab(NULL) + ylab("Watt hours Daily Production") +
  #  geom_line(aes(colour=variable, group=variable)) + 
  geom_point(aes(colour=Model, shape=Model, group=Model), size=1)
g %>% ggplotly()


dfAllTestDiffs = dfAllTestVals %>% mutate(ArimaValsDiff = energyProducedWh - ArimaVals,
                                          WeatherIndependentDiff = energyProducedWh - WeatherIndependent,
                                          WeatherBasedSeasonalDiff = energyProducedWh - WeatherBasedSeasonal)
rDiff = dfAllTestDiffs %>%
  select(DATE, ArimaValsDiff, WeatherIndependentDiff,  WeatherBasedSeasonalDiff) %>% 
  melt(id.vars = c("DATE"), variable.name="Model")

g <- ggplot(data = rDiff, 
            aes(x=DATE, y=value)) + ggtitle("Solar generation - Model difference comparison") + 
  xlab(NULL) + ylab("Actual vs estimated daily Watt hrs") +
  #  geom_line(aes(colour=variable, group=variable)) + 
  geom_point(aes(colour=Model, shape=Model, group=Model), size=1)
g %>% ggplotly()


modelSummary = tibble(modelMSPE = c("ArimaMSPE","WeatherIndependentGLM_MSPE", "WeatherBasedSeasonalGLM_MSPE"),
                      modelMSPEVals = c(mean(dfAllTestDiffs$ArimaValsDiff ^ 2, na.rm =T),
                                        mean(dfAllTestDiffs$WeatherIndependentDiff ^ 2, na.rm=T),
                                        mean(dfAllTestDiffs$WeatherBasedSeasonalDiff ^ 2, na.rm=T))) %>%
  arrange(modelMSPEVals)

# print out the final model summary                
modelSummary



# ---- eighth_config_chunk
setwd(dataDir)
######
##solarTsLog is the transformed time series
# redo the ARIMA model with the complete set of generated values

if (useRDSFiles) {
  ar_FullDfModel = readRDS("ar_FullDfModel.rds")
} else {
  ar_FullDfModel  = Arima(solarTsLog, order = c(2,1,1),seasonal = c(0,1,0))
  saveRDS(ar_FullDfModel, "ar_FullDfModel.rds")
}


year_15_EndDate =  solarInstallDate %m+% years(15)
year_20_EndDate =  solarInstallDate %m+% years(20)
year_25_EndDate =  solarInstallDate %m+% years(25)

# forecast once upto the longest period and take the subset of gen values
fLen = as.integer(year_25_EndDate - as.Date(last(solarDf$DATE)))
fLen15 = as.integer(year_15_EndDate - as.Date(last(solarDf$DATE)))
fLen20 = as.integer(year_20_EndDate - as.Date(last(solarDf$DATE)))

if (useRDSFiles) {
  ar_fullForecast = readRDS("ar_fullForecast.rds")
} else {
  ar_fullForecast = forecast(ar_FullDfModel, h = fLen)
  saveRDS(ar_fullForecast, "ar_fullForecast.rds")
}

## aF = forecast(ar_211_010,h = testLen)
#saveRDS(aF, "aF_Files.rds")

# getting dates back from timeseries is tricky
myForecastTimes  = as.vector(time(ar_fullForecast$mean))
myForecastedDates = as.Date(format(date_decimal(myForecastTimes), "%Y-%m-%d"))

myActualTimes  = as.vector(time(solarTs))
myActualDates = as.Date(format(date_decimal(myActualTimes), "%Y-%m-%d"))

forecastDf  = tibble(DATE=myForecastedDates,
                     ArimaPredictedDailyWh= as.numeric(backTransformLogValues(ar_fullForecast$mean)))
actualDf = tibble(DATE = myActualDates,
                  ActualDailyGenerationWh =  as.numeric( as.numeric(solarTs)))

fDif1  = forecastDf %>% melt(id.vars = c("DATE"), variable.name="Attribute")
actualDf1 = actualDf %>% melt(id.vars = c("DATE"), variable.name="Attribute")

fullGenDf = rbind(actualDf1,fDif1) # %>% mutate(DATE=format(date_decimal(DATE),"%Y-%m-%d"))
# x axis gets too busy, translate after plotting

g <- ggplot(data = fullGenDf, 
            aes(x=DATE, y=value)) + ggtitle("Solar generation ARIMA forecast") + 
  xlab(NULL) + ylab("Actual and forecasted daily Watt hrs") +
  #  geom_line(aes(colour=variable, group=variable)) + 
  geom_point(aes(colour=Attribute, shape=Attribute, group=Attribute), size=1)
g %>% ggplotly()

# ---- ninth_config_chunk
setwd(dataDir)
# Financial and TOU calculations

fullGenDf = fullGenDf  %>% mutate(month = month(DATE),
                                  year  = year(DATE))
fullGenDf$DATE = as.Date(fullGenDf$DATE)

fullGenMonthlyDf = fullGenDf %>% group_by(year, month) %>% summarize(monthlyGenKwh=sum(value)/1000) %>% 
  mutate(DATE=make_date(year, month, 1)) %>% ungroup()

# Model 3 scenarios, two increasing electricity prices, another flat 
electricityCostDf1 = as_tibble(list(DATE=c(as.Date("2018-07-01"),as.Date("2019-07-01")), 
                                    costPerKwhPeak = c(0.371, 0.382),
                                    costPerKwhPartPeak = c(0.256, 0.267),
                                    costPerKwhOffPeak = c(0.18, 0.19),
                                    costProfile = "CostProfile1",
                                    model = "linear"))

electricityCostDf2 = as_tibble(list(DATE=c(as.Date("2015-07-01"), as.Date("2019-07-01"), as.Date("2025-07-01")),
                                    costPerKwhPeak = c(0.338, 0.382, 0.382),
                                    costPerKwhPartPeak = c(0.221, 0.267, 0.267),
                                    costPerKwhOffPeak = c(0.16, 0.19, 0.19),
                                    costProfile = "CostProfile2",
                                    model = "linear"))

electricityCostDf3 = as_tibble(list(DATE=c(as.Date("2018-07-01"), as.Date("2019-07-01"), as.Date("2025-07-01")),
                                    costPerKwhPeak = c(0.372, 0.382, 0.382*2),
                                    costPerKwhPartPeak = c(0.256, 0.267, 0.267*2),
                                    costPerKwhOffPeak = c(0.18, 0.19, 0.19*2),
                                    costProfile = "CostProfile3",
                                    model = "log"))

electricityCostDf4 = as_tibble(list(DATE=c(as.Date("2018-07-01"), as.Date("2019-07-01"), 
                                           as.Date("2022-07-01"), as.Date("2038-07-01")),
                                    costPerKwhPeak = c(0.372, 0.382, 0.19, 0.19),
                                    costPerKwhPartPeak = c(0.256, 0.267, 0.15, 0.15),
                                    costPerKwhOffPeak = c(0.18, 0.19, 0.11, 0.11),
                                    costProfile = "CostProfile4",
                                    model = "linear"))



costProfileList = list(electricityCostDf1,
                       electricityCostDf2,
                       electricityCostDf3,
                       electricityCostDf4)


# costProfileList = c(nest(electricityCostDf1),
#                     nest(electricityCostDf2),
#                     nest(electricityCostDf3))

genPreds = map(costProfileList,predictMonthlyGen,genMonthlyDf = fullGenMonthlyDf)
#r = rbindlist(genPreds)
# %>% group_by(costProfile) %>% arrange(DATE) %>%
#   mutate(cumMonthlySavings = cumsum(monthlySavings) - solarInvestment) %>% ungroup()

allScenarios = rbindlist(genPreds) %>% group_by(costProfile) %>% arrange(DATE) %>%
  mutate(cumMonthlySavings = cumsum(monthlySavings) - solarInvestment,
         realizedPricePerKwh = monthlySavings/monthlyGenKwh) %>% ungroup()

g1 <- ggplot(data = allScenarios,aes(x=DATE, y=cumMonthlySavings)) + 
  ggtitle("Solar investment scenarios") + 
  xlab(NULL) + ylab("Cumulative net savings") + 
  #  geom_line(aes(colour=variable, group=variable)) + 
  geom_line(aes(colour=costProfile), 
            size=1) +  theme(legend.position="none") #+ geom_text()
# geom_dl(aes(label=costProfile), method="last.points") 
#  geom_line(aes(y=solarInvestment, color='Investment')) 

g2 <- ggplot(data = allScenarios,aes(x=DATE, y=realizedPricePerKwh)) + 
  ggtitle("Solar investment scenarios") + 
  xlab(NULL) + ylab("Generated Kwh price") + # theme(legend.position="none") +
  #  geom_line(aes(colour=variable, group=variable)) + 
  geom_line(aes(colour=costProfile), 
            size=1) 
# geom_dl(aes(label=costProfile), method="last.points") 


#multiplot(g1, g2, cols=1) %>% ggplotly()

#multiplot(g1, g2, cols = 1) %>% ggplotly()

allScenariosPayback = allScenarios %>% group_by(costProfile) %>% 
  arrange(DATE) %>% filter(cumMonthlySavings > 0) %>% 
  filter(cumMonthlySavings == min(cumMonthlySavings)) %>% 
  mutate(paybackPeriodDays = DATE - solarInstallDate,
         paybackYrs = as.numeric(paybackPeriodDays)/365) %>% 
  select(costProfile, paybackYrs, DATE)

allScenariosReturnROI = allScenarios %>% group_by(costProfile) %>% 
  arrange(DATE) %>% summarise(IRR = irr(c(-solarInvestment,
                                          monthlySavings))*12*100,
                              NetReturn=last(cumMonthlySavings),
                              ROI=NetReturn/solarInvestment*100)

mySummary = allScenariosPayback %>% left_join(allScenariosReturnROI)

# round off lengthy decimal points
is.num <- sapply(mySummary, is.numeric)
mySummary[is.num] <- lapply(mySummary[is.num], round, 2)

mySummary = mySummary %>% arrange(-ROI) %>% mutate(NetReturn = sprintf("$%.0f",NetReturn),
                                                   ROI = sprintf("%.0f %%", ROI),
                                                   IRR = sprintf("%.0f %%", IRR)) 


sumPlot <- plot_ly(
  type = 'table',
  header = list(
    values = c(colnames(mySummary)),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(width = 1, color = 'black'),
    fill = list(color = '#daf2ed'),
    font = list(family = "Arial", size = 14, color = "black")
  ),
  cells = list(
    values = rbind(
      # rownames(mtcars), 
      t(as.matrix(unname(mySummary)))
    ),
    align = c('left', rep('center', ncol(mtcars))),
    line = list(color = "black", width = 1),
    fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))

subplot(g1,g2, nrows=2, titleY = TRUE)
sumPlot

# ---- tenth_config_chunk 
setwd(dataDir)

# Billing data analysis

pgeElectricBill = tbl_df(read.csv(pgeElectricBilling, stringsAsFactors = F, 
                                  skip = electricSkipLines))
pgeElectricBill$START.DATE = as.Date(pgeElectricBill$START.DATE,"%Y-%m-%d")
pgeElectricBill$END.DATE = as.Date(pgeElectricBill$END.DATE,"%Y-%m-%d")
pgeElectricBill$COST = as.numeric(sub("\\$","", pgeElectricBill$COST))

pgeElectricBill$numDays =as.integer(pgeElectricBill$END.DATE - 
                                      pgeElectricBill$START.DATE)
pgeElectricBill$avgDailyUsage  = pgeElectricBill$USAGE/pgeElectricBill$numDays
pgeElectricBill$KwhCost    =  pgeElectricBill$COST/pgeElectricBill$USAGE
pgeElectricBill = pgeElectricBill %>% select(-NOTES) %>% mutate(year = year(START.DATE),
                                                                month = month(START.DATE))

g1 = ggplot(pgeElectricBill) + aes(x = START.DATE) + geom_line(aes(y = USAGE)) +
  scale_colour_manual(values=c("red", "green")) +
  xlab(NULL) + ylab("Monthly Usage (Kwh)")  + #+
  ggtitle("Monthly Electric usage") 

g2 = ggplot(pgeElectricBill) + aes(x = START.DATE) + geom_line(aes(y = COST)) +
  xlab(NULL) + ylab("Monthly Cost($)")  + #+
  scale_colour_manual(values=c("red", "green")) +
  ggtitle("Monthly Electric usage and $$ spend") 

subplot(g1,g2,nrows=2, titleY = TRUE)

lastSolarReading = last(solarDf$DATE)


l = nrow(pgeElectricBill)
s = seq(as.Date(pgeElectricBill$START.DATE[1]), as.Date(pgeElectricBill$END.DATE[l]), "days") %>% 
  tibble::enframe(name=NULL) 
colnames(s) = c("DATE")
s = s %>% mutate(year=year(DATE),month=month(DATE)) %>% filter(DATE < lastSolarReading)
s2 = s %>% full_join(pgeElectricBill, by=c("year","month")) %>% mutate(usage=avgDailyUsage) %>%
  select(DATE,usage,KwhCost) %>% filter(!is.na(usage))

#KwhCost not useful because of extreme range of values
sDf = getSolarDf()
# merge with solarDf, the ts seems to have missing dates
s3 = s2 %>% left_join(sDf, by="DATE") %>% mutate(genKwh = energyProducedWh/1000,
                                                 netUsage = usage) %>% select(DATE, netUsage, genKwh)
s3$genKwh[s3$DATE < solarInstallDate] = 0
#s2[is.na(s2)] <- 0
s3 = s3 %>% mutate(usage=netUsage+genKwh)




# get monthly values, remove partial months
s3 <- s3 %>% 
  mutate(month = month(DATE),
         year  = year(DATE),
         DATE = floor_date(DATE, "month")) %>%
  group_by(year, month, DATE) %>%
  add_count() %>%
  summarise(monthlyUsage = sum(usage), 
            numDays = n(),
            monthlyNetUsage = sum(netUsage),
            monthlyGen = sum(genKwh)) %>%  ungroup() %>%
  filter(numDays >= 25) %>% 
  select(-numDays)


s3$context = "No Solar"
s3$context[s3$DATE > solarInstallDate] = "Solar"
s3$context[s3$DATE > electricCarDeployDate] = "Solar & Electric Car"

g1 = ggplot(s3) + aes(x = DATE) + geom_line(aes(y = monthlyUsage, colour = context)) +
  xlab(NULL) + ylab("Monthly Usage (Kwh)")  + #+
  ggtitle("Monthly Electric usage & generation") 

g2 = ggplot(s3) + aes(x = DATE) + geom_line(aes(y = monthlyGen, colour = context)) +
  xlab(NULL) + ylab("Monthly Generation (Kwh)")  + #+
  ggtitle("Monthly Electric usage & generation") 

g3 = ggplot(s3) + aes(x = DATE) + geom_line(aes(y = monthlyNetUsage, colour = context)) +
  xlab(NULL) + ylab("NEM(Kwh)")  + #+
  ggtitle("Monthly Electric usage & generation") 


subplot(g1,g2,g3,nrows=3, titleY = TRUE)



