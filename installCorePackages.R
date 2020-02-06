# Meant to be done on a fresh R install

install.packages("tidyverse")
install.packages("forecast")
install.packages("plotly")
install.packages("tidyquant")
install.packages("timetk")
install.packages("imputeTS")
install.packages("zoo")
install.packages('data.table')

install.packages("lubridate")
install.packages("geosphere")
install.packages("ggcorrplot")
install.packages("FinCal")

# install if necessary
load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
