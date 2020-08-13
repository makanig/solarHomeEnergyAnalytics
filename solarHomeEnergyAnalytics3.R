
library(ggplot2)
library(tidyverse)
library(forecast)
library(plotly)
#library("tm")
library(data.table)
library(stringr)
library(lubridate)
library(tidyquant)
library(timetk)
library(zoo)
library(tsibble)
library(imputeTS)
library(sweep) 
library(geosphere)
library(ggcorrplot)
#library(fUnitRoots)
library(FinCal)
library(directlabels)

library(tidyr)
library(TSstudio)
nest <- nest_legacy
unnest <- unnest_legacy

#devtools::install_github("RamiKrispin/TSstudio")


# This package is on early development mode, 
# will use it for the linear regression shiny example
#devtools::install_github("RamiKrispin/forecastML")

library(TSstudio)
library(forecastML)


useRDSFiles = T # use saved RDSFiles

# Modify this with your own username and api_key
setPlotlyEnvs <- function() {
  Sys.setenv("plotly_username"="XXXX")
  Sys.setenv("plotly_api_key"="XXX")
}
# utility functions
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colSort <- function(data, ...) sapply(data, sort, ...)

#setPlotlyEnvs()

dataDir = "C:/Users/gauta/repos/solarHomeEnergyAnalytics/data2"
#dataDir = "data"
gasSkipLines = 5
electricSkipLines = 5

gasUsageDaily = "pge_gas_interval_data.csv"
solarProductionDaily = "Enphase_system_energy_20150708_to_20190808.csv"
solarProductionHourly = "system_power_20190802.csv"

pgeElectricBilling = "pge_electric_billing_data.csv"
pgeGasBilling = "pge_gas_billing_data.csv"

weatherData = "WeatherDailyData_2014-Current.csv"

nsrdbDataFiles = c("nsrdbData/123342_37.85_-122.22_2015.csv", 
                   "nsrdbData/123342_37.85_-122.22_2016.csv",
                   "nsrdbData/123342_37.85_-122.22_2017.csv")


  
gasTier1Pricing = 1.284 + 0.09  # $ per Therm, the 0.09 is a surcharge
gasTier1DailyAllocation = 0.59
gasTier2Pricing = 1.884 + 0.09  

myLatitude = 37.877

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

# read https://nsrdb.nrel.gov/current-version#psm
# aggregate hourly readings into daily
getNsrdbDf <- function() {
  
  # get non-zero DHI readings
  
  nsrDf = lapply(nsrdbDataFiles, function(x) {
    read.csv(file = x, skip = 2, stringsAsFactors = F) %>% as_tibble
  }) %>% rbindlist %>% mutate(  # Cloud.Type = as.factor(Cloud.Type),
    DATE=ISOdate(year = Year, month= Month, day = Day)) 
  # aggregate on a daily basis - consider adding: %>% filter(DHI > 0)
  nsrDf = nsrDf   %>% select(Year, Month, Day, DATE, DHI, DNI, GHI, Clearsky.GHI, Clearsky.DNI, Clearsky.DHI, 
                             Cloud.Type, Dew.Point)
  
  nsrDf = nsrDf %>% group_by(DATE) %>%
    add_count() %>% summarise(DHI = sum(DHI),
                              DNI = sum(DNI),
                              GHI = sum(GHI),
                              Clearsky.DHI = sum(Clearsky.DHI),
                              Clearsky.DNI = sum(Clearsky.DNI),
                              Clearsky.GHI = sum(Clearsky.GHI),
                              Clould.Type = max(Cloud.Type),
                              Dew.Point = sum(Dew.Point)) %>%  ungroup() %>% 
    mutate(DATE = as.Date(DATE))
  
}

solarInvestment  = 6513  # $$ cost of your solar upfront investment post tax credit
solarInstallDate = as.Date("2015/07/08")
solarGenDataBadValues = c(0, 14873)
maxDailySolarGen = 24000 # any readings over this are suspect and discarded
minDailySolarGen = 1 # constrain this to allow for log transforms
solarSuspectDateRange = seq(from=as.Date("2018/6/2"), to=as.Date("2019/1/16"), by='day')
limitSolarDate = as.Date("2018/6/2") # limit to this value because later date values are suspect
lastGoodSolarReading = as.Date("2019-08-02")
electricCarDeployDate = as.Date("2018-10-15")

# Specific to solar installation. Filter out these specific values and replace with moving average
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
  
  # setDT(solarHourlyDf)[wday == "Saturday" | wday == "Sunday",
  #                      type := "offPeak",
  #                      (wday == "Saturday" | wday == "Sunday") & (hour >= 17 & hour < 20 ),
  #                      type := "partPeak"]
  #
  
  solarHourlyDf %>% group_by(DATE, type) %>% summarise(energyProducedWhTotal = sum(energyProducedWh))
  # # A tibble: 20 x 3
  # # Groups:   DATE [8]
  # DATE       type     energyProducedWhTotal
  # <date>     <chr>                    <int>
  #   1 2019-07-27 offPeak                 173391
  # 2 2019-07-27 partPeak                 14771
  # 3 2019-07-28 offPeak                 167313
  # 4 2019-07-28 partPeak                 14934
  # 5 2019-07-29 offPeak                  21999
  # 6 2019-07-29 partPeak                 56466
  # 7 2019-07-29 peak                    113049
  # 8 2019-07-30 offPeak                   7588
  # 9 2019-07-30 partPeak                 52087
  # 10 2019-07-30 peak                    114230
  # 11 2019-07-31 offPeak                  23320
  # 12 2019-07-31 partPeak                 52890
  # 13 2019-07-31 peak                    108887
  # 14 2019-08-01 offPeak                   8281
  # 15 2019-08-01 partPeak                 46477
  # 16 2019-08-01 peak                    111958
  # 17 2019-08-02 offPeak                  14682
  # 18 2019-08-02 partPeak                 51299
  # 19 2019-08-02 peak                    107815
  # 20 2019-08-03 offPeak                      0
  
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
  print(paste("oFormula model:", costDf$model[1], " o:", o))
  print(paste("nrow(monthlyDf):", nrow(monthlyDf)))
  print(paste("nrow(costDf):", nrow(costDf)))
  
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
  
  print(paste(costDf$costProfile, " predictMonthlyGen: entry"))
  
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
  
  print(paste("CostProfile:", retDf$costProfile[1], " totalSavings:", sum(retDf$monthlySavings)))
  retDf
  
  # # 
  # # if (costDf$model[1] == "linear") {
  # #   variables <- c("DATE")
  # #   f <- as.formula(paste(outcome,paste(variables, collapse = " + "), 
  # #                         sep = " ~ "))
  # #   fit  = lm(f, data = costDf)
  # # } else { # use log
  # #   variables <- c("poly(decimal_date(DATE),2)")  # anything different from straight line
  # #   
  # #   # use a poly function
  # #   f <- as.formula(paste(outcome, paste(variables, collapse = " + "), 
  # #                         sep = " ~ "))
  # # } 
  # # 
  # # fit = lm(f, data= costDf)
  # # 
  # summary(fit)
  # KwhCostDf = predict(fit, newdata=genMonthlyDf, type="response") %>% tibble::enframe() # %>%
  # KwhCostDf$DATE = genMonthlyDf$DATE
  # KwhCostDf$costProfile = costDf$costProfile[[1]]
  # KwhCostDf$monthlyGenKwh = genMonthlyDf$monthlyGenKwh
  # 
  # KwhCostDf = KwhCostDf %>% mutate(KwhCost = value,
  #                                  monthlySavings = KwhCost*monthlyGenKwh) %>% select(-name, -value)
}


setwd(dataDir)

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


# ##################### solar generation model ########################
solarDf = getSolarDf()

solarDfTsibble = solarDf %>% select(DATE, energyProducedWh) %>% as_tsibble()
ts_plot(solarDfTsibble, 
        title = "Daily Solar Generation",
        Ytitle = "Wh Daily Production",
        # Xtitle = "Date",
        slider = TRUE)

# include daily weather
solarDf = solarDf %>% inner_join(weatherDfDaily) %>% rowwise() %>% 
  mutate(DayLength = daylength(myLatitude, DATE))


solarTs = ts(solarDf$energyProducedWh, start=c(solarDf$year[[1]], solarDf$yDay[[1]]), frequency = 365)
ts_seasonal(solarTs)

# examine the plot below for stationarity and order 
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

# # Try #1
# decompose_df <- glm(energyProducedWhLog ~ DATE_Numeric*dRank + xc+xs+PRCP +DATE_Numeric*xc + DATE_Numeric*xs +
#                       TMAX + TMIN, data=solarDf)
# 
# summary(decompose_df)


# Try #2  <=== choose this one
decompose_df <- glm(energyProducedWhLog ~  xc+PRCP +DATE_Numeric*xc +
                      TMAX + TMIN, data=solarDf)

summary(decompose_df)

# Call:
#   glm(formula = energyProducedWhLog ~ xc + PRCP + DATE_Numeric * 
#         xc + TMAX + TMIN, data = solarDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.57045  -0.19717   0.09251   0.27796   1.12271  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.010e+00  5.320e-01   7.538 8.28e-14 ***
#   xc              -3.330e+00  6.546e-01  -5.087 4.10e-07 ***
#   PRCP            -7.399e-01  6.296e-02 -11.752  < 2e-16 ***
#   DATE_Numeric    -2.911e-04  2.776e-05 -10.487  < 2e-16 ***
#   TMAX             9.930e-03  1.916e-03   5.182 2.50e-07 ***
#   TMIN            -8.066e-03  2.586e-03  -3.119  0.00185 ** 
#   xc:DATE_Numeric  8.333e-05  3.759e-05   2.217  0.02677 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1948883)
# 
# Null deviance: 3122.33  on 1486  degrees of freedom
# Residual deviance:  288.43  on 1480  degrees of freedom
# AIC: 1797.2
# 
# Number of Fisher Scoring iterations: 2


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
# [1] "RMSE: 0.440421251694386  Pearson residual variance 0.194888308776893  GOF: 0.907621987482646"


# Messes up R install when you try to install these packages for stationary (tseries ?)
# don't run stationary test
# kpssTest = urkpssTest(solarTs, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = F)

#detemine stationarity of data
# urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
# tsstationary<-diff(tsData, differences=1)

# series now becomes stationary
tsStationary = diff(solarTsLog,differences=1)

# Forecast with  ARIMA, use the training set for the fit
solarDfTrainingTsLog = ts(data=trainingDf$energyProducedWhLog, start=c(trainingDf$year[[1]], trainingDf$yDay[[1]]),
                          frequency=365) 
# 
# Series: solarDfTrainingTsLog 
# ARIMA(2,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ar1     ar2      ma1
# 0.3535  0.1000  -1.0000
# s.e.  0.0513  0.0522   0.0091
# 
# sigma^2 estimated as 0.411:  log likelihood=-371.73
# AIC=751.47   AICc=751.57   BIC=767.2
# 
# Training set error measures:
#   ME      RMSE       MAE      MPE     MAPE      MASE        ACF1
# Training set -0.01495805 0.4548676 0.2319034 18.38288 50.35202 0.4301588 0.008524465


# From auto.arima (takes a long time) and looking at the ACF and PACF:
# summary(bestFit)
# Series: solarDfTs 
# ARIMA(0,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ma1
# -0.5314
# s.e.   0.0775
# 
# sigma^2 estimated as 8442129:  log likelihood=-3553.78
# AIC=7111.57   AICc=7111.6   BIC=7119.44
# 
# Training set error measures:
#   ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
# Training set -19.25653 2068.284 946.7204 -6.039934 18.02144 0.4558433 0.1060786
# > 
# Arima(solarDfTs, order = c(0,1,1),seasonal = c(0,1,0))
# Series: solarDfTs 
# ARIMA(0,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ma1
# -0.5314
# s.e.   0.0775
# 
# sigma^2 estimated as 8442129:  log likelihood=-3553.78
# AIC=7111.57   AICc=7111.6   BIC=7119.44


if (useRDSFiles) {
  ar_211_010 = readRDS("arModel.rds")
} else {
  ar_211_010 = Arima(solarDfTrainingTsLog, order = c(2,1,1),seasonal = c(0,1,0))
  saveRDS(ar_211_010, "arModel.rds")
}

summary(ar_211_010)
# Series: solarDfTrainingTsLog 
# ARIMA(2,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ar1     ar2      ma1
# 0.3535  0.1000  -1.0000
# s.e.  0.0513  0.0522   0.0091
# 
# sigma^2 estimated as 0.411:  log likelihood=-371.73
# AIC=751.47   AICc=751.57   BIC=767.2
# 
# Training set error measures:
#   ME      RMSE       MAE      MPE     MAPE      MASE        ACF1
# Training set -0.01495805 0.4548676 0.2319034 18.38288 50.35202 0.4301588 0.008524465


# Series: solarDfTrainingTs 
# ARIMA(2,1,1)(0,1,0)[365] 
# 
# Coefficients:
#   ar1     ar2      ma1
# 0.2218  0.3819  -0.9519
# s.e.  0.1033  0.0972   0.0709
# 
# sigma^2 estimated as 3794177:  log likelihood=-1477.43
# AIC=2962.86   AICc=2963.11   BIC=2975.25
# 
# Training set error measures:
#   ME     RMSE     MAE       MPE     MAPE      MASE       ACF1
# Training set -72.98841 1073.579 391.062 -1.860337 7.596822 0.2276769 0.01096718

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




# Call:
#   glm(formula = energyProducedWh ~ DATE_Numeric + xc + DATE_Numeric * 
#         xc + xs + DATE_Numeric * dRank, family = gaussian(link = "log"), 
#       data = trainingDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -12842.6   -1112.0     123.4    1217.5    4485.9  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.106e+01  8.431e+00   3.684 0.000247 ***
#   DATE_Numeric       -1.247e-03  4.939e-04  -2.525 0.011764 *  
#   xc                 -1.013e+01  7.563e+00  -1.339 0.181032    
# xs                 -4.943e-02  1.399e-02  -3.532 0.000438 ***
#   dRank              -3.357e+01  1.732e+01  -1.938 0.052974 .  
# DATE_Numeric:xc     4.887e-04  4.438e-04   1.101 0.271192    
# DATE_Numeric:dRank  1.860e-03  1.015e-03   1.832 0.067315 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 4996068)
# 
# Null deviance: 3.2916e+10  on 742  degrees of freedom
# Residual deviance: 3.6771e+09  on 736  degrees of freedom
# AIC: 13578
# 
# Number of Fisher Scoring iterations: 5


# > summary(weatherIndependentGlmModel)
# Call:
#   glm(formula = energyProducedWhLog ~ DATE_Numeric + dRank + DATE_Numeric * 
#         dRank + xc + xs, data = trainingDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.77127  -0.15298   0.09528   0.33273   0.93369  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        14.1348210  3.3483472   4.221 2.73e-05 ***
#   DATE_Numeric       -0.0008795  0.0001949  -4.512 7.49e-06 ***
#   dRank              -7.6224957  5.2967503  -1.439    0.151    
# xc                 -1.9120966  0.2202975  -8.680  < 2e-16 ***
#   xs                 -0.0423796  0.0279387  -1.517    0.130    
# DATE_Numeric:dRank  0.0004593  0.0003046   1.508    0.132    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 0.2491656)
# 
# Null deviance: 1648.71  on 742  degrees of freedom
# Residual deviance:  183.64  on 737  degrees of freedom
# AIC: 1084
# 
# Number of Fisher Scoring iterations: 2


weatherIndependentGlmForecast <- stats::predict(weatherIndependentGlmModel, type = "response", newdata=testDf) %>% 
  tibble::enframe()
weatherIndependentErr <- stats::predict(weatherIndependentGlmModel, newdata=testDf, se = TRUE)

# 
# plotGlmFit(solarDf[round((nReadings+1)/2):nReadings,], solarDf, weatherIndependentGlmModel,
#            title = "Weather Independent GLM")



# # Now GLM with weather variables

# tweaked based on summary(glm) output



weatherBasedGlmModel = glm(energyProducedWhLog ~  xc+PRCP +DATE_Numeric*xc +
                             TMAX + TMIN, 
                           # energyProducedWhLog ~  DATE_Numeric + dRank + PRCP  +  xc + xc*dRank + DATE_Numeric*xc +
                           #   TMAX + TMIN + DATE_Numeric*xs + DATE_Numeric*dRank + xs*dRank, 
                           #family="gaussian"( link="log" ), 
                           data = trainingDf)
summary(weatherBasedGlmModel) 
# 
# Call:
#   glm(formula = energyProducedWh ~ DATE_Numeric + dRank + PRCP + 
#         xc + xc * dRank + DATE_Numeric * xc + TMAX + TMIN + DATE_Numeric * 
#         dRank, family = gaussian(link = "log"), data = trainingDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -12172.1    -514.3     259.7    1070.0    4116.1  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         4.458e+01  7.295e+00   6.111 1.60e-09 ***
#   DATE_Numeric       -2.086e-03  4.245e-04  -4.914 1.10e-06 ***
#   dRank              -6.097e+01  1.486e+01  -4.103 4.54e-05 ***
#   PRCP               -1.505e+00  2.162e-01  -6.960 7.55e-12 ***
#   xc                 -2.387e+01  6.488e+00  -3.679 0.000252 ***
#   TMAX                7.279e-03  9.761e-04   7.457 2.50e-13 ***
#   TMIN               -1.455e-02  2.027e-03  -7.178 1.74e-12 ***
#   dRank:xc            1.014e+00  1.124e-01   9.026  < 2e-16 ***
#   DATE_Numeric:xc     1.309e-03  3.805e-04   3.442 0.000611 ***
#   DATE_Numeric:dRank  3.606e-03  8.696e-04   4.146 3.77e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 3448352)
# 
# Null deviance: 3.2916e+10  on 742  degrees of freedom
# Residual deviance: 2.5276e+09  on 733  degrees of freedom
# AIC: 13305
# 
# Number of Fisher Scoring iterations: 8


# Call:
#   glm(formula = energyProducedWh ~ DATE_Numeric + dRank + PRCP + 
#         PRCP * dRank + xc + DATE_Numeric * xc + TMAX + TMIN + DATE_Numeric * 
#         dRank, family = gaussian(link = "log"), data = trainingDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -11762.4    -986.6      72.3    1166.2    4830.8  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         4.183e+01  7.735e+00   5.409 8.60e-08 ***
#   DATE_Numeric       -1.885e-03  4.504e-04  -4.184 3.21e-05 ***
#   dRank              -5.824e+01  1.577e+01  -3.693 0.000238 ***
#   PRCP               -7.821e-01  3.856e-01  -2.028 0.042914 *  
#   xc                 -2.277e+01  6.875e+00  -3.311 0.000974 ***
#   TMAX                9.227e-03  1.023e-03   9.021  < 2e-16 ***
#   TMIN               -9.838e-03  2.146e-03  -4.583 5.38e-06 ***
#   dRank:PRCP         -1.687e+00  9.279e-01  -1.818 0.069513 .  
# DATE_Numeric:xc     1.234e-03  4.031e-04   3.061 0.002286 ** 
#   DATE_Numeric:dRank  3.309e-03  9.238e-04   3.582 0.000364 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 3944453)
# 
# Null deviance: 3.2916e+10  on 742  degrees of freedom
# Residual deviance: 2.8913e+09  on 733  degrees of freedom
# AIC: 13405
# 
# Number of Fisher Scoring iterations: 8



# > summary(weatherBasedGlmModel)
# 
# Call:
#   glm(formula = energyProducedWh ~ dRank + PRCP + TMAX + DATE_Numeric * 
#         dRank, family = gaussian(link = "log"), data = trainingDf)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -13208.1   -1688.1    -474.7    1571.9    6005.0  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        13.9260061  4.7174556   2.952   0.0033 ** 
#   dRank              -0.0672540  6.2139415  -0.011   0.9914    
# PRCP               -2.6293599  0.5934313  -4.431 1.14e-05 ***
#   TMAX                0.0081504  0.0015859   5.139 3.90e-07 ***
#   DATE_Numeric       -0.0003848  0.0002787  -1.381   0.1680    
# dRank:DATE_Numeric  0.0001255  0.0003678   0.341   0.7331    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 6400436)
# 
# Null deviance: 2.3171e+10  on 529  degrees of freedom
# Residual deviance: 3.3538e+09  on 524  degrees of freedom
# AIC: 9818.1
# 
# Number of Fisher Scoring iterations: 11


weatherBasedGlmForecast <- stats::predict(weatherBasedGlmModel, type = "response", newdata=testDf) %>% 
  tibble::enframe()
weatherBasedErr <- stats::predict(weatherBasedGlmModel, newdata=testDf, se = TRUE)

# ETS, won't run. bats takes too long
# solarEtsFit <- ets(solarDfTrainingTs)
# sw_tidy(solarEtsFit)
# 
# sw_glance(solarEtsFit)
# 
# solarEtsFcast = forecast(solarEtsFit, h = testLen) %>% sw_sweep(fitted = FALSE, timetk_idx = F) 
# 
allTestVals = tibble(DATE = testDf$DATE,
                     ArimaVals= backTransformLogValues(aF$mean),
                     WeatherIndependent =backTransformLogValues(weatherIndependentGlmForecast$value),
                     WeatherBasedSeasonal =backTransformLogValues(weatherBasedGlmForecast$value ))

dfAllTestVals = solarDf %>% dplyr::left_join(allTestVals, by = "DATE") 
# 
# solarDf$ArimaVal = NA
# solarDf$weatherIndependentGlm = NA
# solarDf$weatherBasedGlm = NA
# 
# solarDf$ArimaVal[halfWay+1:nReadings] = 

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

## modelMSPE                    modelMSPEVals
# <chr>                                <dbl>
#   1 WeatherBasedSeasonalGLM_MSPE      4567720.
# 2 WeatherIndependentGLM_MSPE        4772622.
# 3 ArimaMSPE                         6356574.

########## Financials#########


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

forecastDf  = tibble(DATE=index(ar_fullForecast$mean),
                     ArimaPredictedDailyWh= as.numeric(backTransformLogValues(ar_fullForecast$mean)))
actualDf = tibble(DATE = index(solarTs),
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

fullGenDf = fullGenDf  %>% mutate(DATE=format(date_decimal(DATE),"%Y-%m-%d"),
                                  month = month(DATE),
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

# 
# # Model 3 scenarios, two increasing electricity prices, another flat 
# electricityCostDf1 = as_tibble(list(DATE=c(as.Date("2015-07-01"),as.Date("2019-07-01")), 
#                                    costPerKwh = c(0.26, 0.29),
#                                    costProfile = "CostProfile1",
#                                    model = "linear"))
# electricityCostDf2 = as_tibble(list(DATE=c(as.Date("2015-07-01"),as.Date("2019-07-01")), 
#                                     costPerKwh = c(0.27, 0.30),
#                                     costProfile = "CostProfile2",
#                                     model = "linear"))
# electricityCostDf3 = as_tibble(list(DATE=c(as.Date("2015-07-01"),as.Date("2019-07-01"), as.Date("2025-07-01")),
#                                     costPerKwh = c(0.26, 0.27, 0.43),
#                                     costProfile = "CostProfile3",
#                                     model = "log")) 
# electricityCostDf4 = as_tibble(list(DATE=c(as.Date("2015-07-01"),
#                                   as.Date("2019-07-01"), as.Date("2025-07-01")),
#                                     costPerKwh = c(0.28, 0.30, 0.38),
#                                     costProfile = "CostProfile4",
#                                     model = "log")) # stays constant after 2020
# electricityCostDf5 = as_tibble(list(DATE=c(as.Date("2015-07-01"),
#                               as.Date("2016-07-01"), as.Date("2017-07-01")),
#                               costPerKwh = c(0.28, 0.30, 0.34),
#                                     costProfile = "CostProfile5",
#                                model = "log")) # stays constant after 2020

# outcome <- "costPerKwh"
# variables <- c("DATE")
# 
# f <- as.formula(paste(outcome,paste(variables, collapse = " + "), sep = " ~ "))
# #f
# 
# costLm  = lm(f, data = costDf)

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
subplot(g1,g2, nrows=2, titleY = TRUE)

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

sumPlot



#=================== Usage ===========================



# g %>% ggplotly()
# 
# ggplot(combinedData, aes(x = DATE)) +
#   geom_line(aes(y = energyProduced, colour = "energyProduced")) + 
#   geom_line(aes(y = dailyUsage, colour = "dailyUsage")) +
#   scale_colour_manual(values=c("red", "green")) +
#   ggtitle("Solar generation & Energy Usage - without zero-generation fixes")
# 
# # 
# # 
# # 
# # 
# # solarLmModel = solarLmModel1
# # solarLmModel = solarLmModel2
# # solarLmModel = solarLmModel3
# # solarLmModel = solarLmModel4
# 
# # solarLmModel = glm(energyProducedWh ~ GHI + DNI + DHI + log(dRank) + log(DATE_Numeric) + 
# #                      log(TMAX) + log(TMIN), family="Gamma"( link="identity" ), 
# #                    data = solarWeatherAndRadiationDaily)
# # 
# # 
# # solarLmModel = glm(energyProducedWh ~ GHI, family="Gamma"( link="log" ), 
# #                    data = solarWeatherAndRadiationDaily)
# # 
# # glm( y ~ x1 + log( x2 ) + x3,
# #      family=poisson( link="log" ) )
# 
# # Utility function for fitting GLM
# 
# 
# 
# # Compute a matrix of correlation p-values
# p.mat <- cor_pmat(mtcars)
# p.mat
# 
# # Visualize the correlation matrix
# # --------------------------------
# # method = "square" or "circle"
# ggcorrplot(corr, type="lower")
# ggcorrplot(corr, method = "circle")

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
# ggcorrplot(corr, hc.order = TRUE, outline.color = "white")


################ Gas model#################################
# 
# gasBilling30Ts = tk_ts(gasBilling30Days, select = c(-DATE, -year), 
#                        start = c(year(gasDfMonthly$DATE[1]),
#                                  month(gasDfMonthly$DATE[1])),
#                        frequency = 12)

# # Note - this is net electric usage
# electricDf = tbl_df(read.csv(electricUsageHourly, skip = electricSkipLines, stringsAsFactors = F))
# 
# 
# electricDf$DATE = as.Date(electricDf$DATE,"%Y-%m-%d")
# electricDf = addMissingTimeValues(electricDf)  # there's none in this dataset
# # Use moving average to replace missing NA values 
# electricDf = imputeTS::na_ma(electricDf, k = 10)
# 
# 
# electricDfDaily = electricDf %>% group_by(DATE) %>% 
#                      summarise(dailyNetUsage=sum(USAGE))
# electricDfDaily
# 
# 
# (ggplot(electricDfDaily, aes(x = DATE, y = dailyNetUsage)) + geom_point() + 
#   ggtitle("Net Electric Usage")) %>% ggplotly()
# 
# 
# 
# # inner join with solar data and create a dailyUsage column
# combinedData = solarDf %>% inner_join(electricDfDaily) %>% 
#   mutate(dailyUsage = dailyNetUsage + energyProduced) %>% select(-energyProducedWh)
# 
# ggplot(combinedData, aes(x = DATE)) +
#   geom_line(aes(y = energyProduced, colour = "energyProduced")) + 
#   geom_line(aes(y = dailyUsage, colour = "dailyUsage")) +
#   scale_colour_manual(values=c("red", "green")) +
#   ggtitle("Solar generation & Energy Usage - without zero-generation fixes")
# 
# energyProducedBadData = combinedData %>% 
#   filter(DATE %in% solarDfSuspect$DATE)
# 
# energyProducedGoodData = combinedData %>% anti_join(energyProducedBadData, 
#                                                     by = "DATE") 
#  
# 
# ggplot(energyProducedGoodData, aes(x = DATE)) +
#   geom_point(aes(y = energyProduced, colour = "energyProduced")) + 
#   geom_point(aes(y = dailyUsage, colour = "dailyUsage")) +
#   scale_colour_manual(values=c("red", "green")) +
#   ggtitle("Missing zero Solar generation values & Energy Usage")# this is very likely a reporting problem with Enphase, just grab the previous years
# # energyProduced data
# 
# energyProducedFixFillDates = energyProducedBadData$DATE - 365
# 
# energyProducedFixes = solarDf %>% filter(DATE %in% energyProducedFixFillDates)
# energyProducedBadData$energyProduced = energyProducedFixes$energyProduced
# energyProducedBadData$dailyUsage = energyProducedBadData$dailyNetUsage + 
#   energyProducedBadData$energyProduced
# 
# # badData is fixed now
# 
# 
# 
# # try merge
# combinedData = tbl_df(rbind(energyProducedGoodData, energyProducedBadData))
# 
# 
# (ggplot(combinedData, aes(x = DATE)) +
#          geom_point(aes(y = energyProduced, colour = "energyProduced")) + 
#          geom_point(aes(y = dailyUsage, colour = "dailyUsage")) +
#          scale_colour_manual(values=c("red", "green")) +
#          ggtitle("Solar generation & Energy Usage")) %>% ggplotly()

# afterTesla = combinedData %>% filter(DATE > as.Date("2018-10-15"))
# beforeTesla =  combinedData %>% filter(DATE < as.Date("2018-10-15"))
# 
# summary(beforeTesla)
# summary(afterTesla)

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


(ggplot(pgeElectricBill, aes(x = START.DATE)) +
    geom_point(aes(y = avgDailyUsage, colour = "avgDailyUsage")) +
    scale_colour_manual(values=c("red", "green")) +
    ggtitle("Net electric generation & Energy Usage")) %>% ggplotly()

(ggplot(pgeElectricBill, aes(x = START.DATE)) +
    geom_point(aes(y = avgDailyUsage, colour = "avgDailyUsage")) +
    scale_colour_manual(values=c("red", "green")) +
    ggtitle("Net electric generation & Energy Usage")) %>% ggplotly()



# Read in the Gas data and plot against weather
gasDf = tbl_df(read.csv(gasUsageDaily, skip = gasSkipLines, stringsAsFactors = F)) %>%
  select(DATE, USAGE)
gasDf$DATE = as.Date(gasDf$DATE,"%Y-%m-%d")
gasDf = addMissingTimeValues(gasDf,"DATE","USAGE")

# Use moving average to replace missing NA values 
gasDf = imputeTS::na_ma(gasDf, k = 3)

# get monthly values, remove partial months
gasDfMonthly <- gasDf %>%
  mutate(month = month(DATE, label = TRUE),
         year  = year(DATE),
         DATE = floor_date(DATE, "month")) %>%
  group_by(year, month, DATE) %>%
  add_count() %>%
  summarise(monthlyUsage = sum(USAGE), numDays = n()) %>%  ungroup() %>%
  filter(days_in_month(DATE) == numDays) %>% 
  select(-numDays)

# 
# gasDfDailyTs = tibble(ts(gasDf$USAGE, start = gasDf$DATE[1],
#                  end = gasDf$DATE[length(gasDf$DATE)],
#                  frequency = 1))

# adding pricing estimate


gasDfMonthlyTs = tk_ts(gasDfMonthly, select = c(-month,-DATE, -year), 
                       start = c(year(gasDfMonthly$DATE[1]),
                                 month(gasDfMonthly$DATE[1])),
                       frequency = 12)



(ggseasonplot(gasDfMonthlyTs, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("Therms") +
    ggtitle("Seasonal plot: Gas usage")) %>% ggplotly()


gasDfMonthlyTsFit <- ets(gasDfMonthlyTs)
sw_tidy(gasDfMonthlyTsFit)

sw_glance(gasDfMonthlyTsFit)

gasDfFcast = forecast(gasDfMonthlyTsFit, h = 41) %>% sw_sweep(fitted = FALSE, timetk_idx = TRUE) 

gasDfFcastWithPricing = gasDfFcast %>% mutate(numDays = days_in_month(index),
                                              year = year(index)) %>%
  rowwise() %>% mutate(monthlyCharge=calculateMonthlyCharge(monthlyUsage, as.integer(numDays)),
                       monthlyLo80 = calculateMonthlyCharge(lo.80, as.integer(numDays)),
                       monthlyHi80 = calculateMonthlyCharge(hi.80, as.integer(numDays))) %>% ungroup()


gasDfYearlySummary = gasDfFcastWithPricing %>% group_by(year) %>%
  summarise(yearlyUsage=sum(monthlyUsage),
            yearlyCost=sum(monthlyCharge))

#gasDfFcastWithPricing2 = gasDfFcastWithPricing %>% left_join(gasDfYearlySummary)

p1 = plot_ly(gasDfFcastWithPricing) %>%
  add_lines(x = ~index, y = ~monthlyUsage,
            color = I("black"),
            hoverinfo = "text",
            name = "Monthly usage",
            text = ~paste0(round(monthlyUsage), 'Therms ', index, ' $', round(monthlyCharge))) %>%
  add_ribbons(x = ~index, ymin = ~lo.80, ymax = ~hi.80,
              color = "#D5DBFF", name = "80% confidence") %>%
  layout(
    title = "Forecast of gas usage and charges",
    xaxis = list(
      zeroline = F
    ),
    yaxis = list(
      hoverformat = '.2f',
      title = "Monthly usage (Therms)"
    )
  )


p2 = plot_ly(gasDfFcastWithPricing) %>%
  add_lines(x = ~index, y = ~monthlyCharge,
            color = I("green"),
            hoverinfo = "text",
            name = "$$ Monthly charges",
            text = ~paste0(round(monthlyUsage), 'Therms ', index, ' $', round(monthlyCharge))) %>%
  add_ribbons(x = ~index, ymin = ~monthlyLo80, ymax = ~monthlyHi80,
              color = "#92d991", name="80% confidence") %>%
  layout(
    title = "Forecast of gas usage and charges",
    xaxis = list(
      zeroline = F
    ),
    yaxis = list(
      hoverformat = '.2f',
      title = "$$ Monthly charge"
    )
  )

p3 <- plot_ly(gasDfYearlySummary) %>%
  add_bars(x = ~year, y = ~yearlyUsage, type = 'identity', name = 'Yearly Usage',
           hoverinfo = "text",
           color = "#D5DBFF",
           text = ~paste0(round(yearlyUsage), 'Therms ')) %>%
  add_trace(x = ~year,y = ~yearlyCost, name = 'Cost', type='bar',
            hoverinfo = "text",
            color = I("green"),
            text = ~paste0('$', round(yearlyCost))) %>%
  layout(yaxis = list(title = 'Yearly usage and costs'), barmode = 'group')


p <- subplot(p1, p2, p3, nrows=3, titleY = TRUE)
p

gasUsageAndWeather = gasDf %>% inner_join(weatherDf)
gasDfMonthlyAndWeather = gasUsageAndWeather %>% group_by(month=floor_date(DATE, "month")) %>%
  mutate(monthlyUsage=sum(USAGE))
gasDfMonthlyAndWeather = replace(gasDfMonthlyAndWeather, TRUE, lapply(gasDfMonthlyAndWeather, NA2mean))


ggplot(gasDf)+geom_point(aes(x=DATE,y=USAGE)) + geom_smooth(aes(x=DATE,y=USAGE),method='loess')

# # the chart above showed the readings are not exact. Aggregate the months readings
# gasDfMonthly = gasDf %>% group_by(month=floor_date(DATE, "month")) %>%
#   summarize(monthlyUsage=sum(USAGE))



# Plot against weather

plot_ly(gasDfMonthlyAndWeather) %>%
  add_trace(x = ~DATE, y = ~monthlyUsage,  type = 'bar', name = 'Monthly Gas Usage',
            marker = list(color = '#a85618'),
            hoverinfo = "text",
            text = ~paste(monthlyUsage, ' Therms', DATE)) %>%
  # add_trace(x = ~DATE, y = ~TEMP, type = 'scatter', mode = 'markers', name = 'Temperature', yaxis = 'y2',
  #           marker = list(color = '#45171D'),
  #           hoverinfo = "none",
  #           text = ~paste(TMIN, '?F', DATE)) %>%
  add_lines(x = ~DATE, y = ~fitted(loess(TMIN ~ as.numeric(DATE), span=0.01)),
            line = list(color = 'rgba(7, 164, 181, 1)'),
            name = "Daily minimum temperature",
            hoverinfo = "text",
            text = ~paste(TMIN, '?F', DATE)) %>%
  layout(title = "Monthly gas usage vs minimum daily temperature",
         xaxis = list(title = ""),
         yaxis = list(side = "left",  title = 'Therms', 
                      showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(overlaying = "y", side = "right", 
                       title = "Temperature in Fahrenheit",
                       showgrid = FALSE, zeroline = FALSE))





  
