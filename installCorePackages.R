# Meant to be done on a fresh R install

install.packages('ggplot2')
install.packages("tidyverse")
install.packages("forecast")
install.packages("plotly")
install.packages("tidyquant")
install.packages("timetk")
install.packages("imputeTS")
# install.packages("ggplotly") => not available in R 3.5.1
install.packages("tm")
install.packages("zoo")
install.packages('data.table')
install.packages('stringr')
install.packages("lubridate")
install.packages("nycflights13")
install.packages("sweep")
install.packages("geosphere")
install.packages("ggcorrplot")
install.packages("rcompanion")
install.packages("FinCal")
install.packages("directlabels")

#install.packages("ggfortify")
#install.packages("fUnitRoots") => messes up R installation
# install.packages("tseries") ==> messes up R installation

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 