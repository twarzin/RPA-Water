# File to transform climate data into format usable by demand model
# Travis Warziniack

rm(list = ls())  # clears memory

# Set working directory to file location
# for Pam: 
#setwd("E:/WaterDemand/WaterDemandProject/DataWaterDemand")
# for Travis desktop:
setwd("D:/5_RPA/Demand model")
# for Travis laptop
# setwd("D:/WEAP Input Creation")

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)  # Has the pipe operator %>%.
library(data.table)

# read in one of the county-level monthly precip files

precip <-read.csv("1_ClimateData/CountyPrecip/pr_CNRM_CM5rcp45_month.csv", check.names=F)

# turn date variable into POSIX (date/time) variable and extract year and month 
precip$date <- as.POSIXct(precip$Date, format = "%Y-%m-%d")
precip$year <- as.numeric(format(precip$date, format="%Y"))
precip$month <- as.numeric(format(precip$date, format="%m"))

precip = subset(precip, select = -c(Date,date) )

summer.precip <- subset(precip, month >= 4 & month <= 9) %>%
  group_by(year) %>%
  summarise_all(sum)

# drop month column
summer.precip = subset(summer.precip, select = -c(month) )

# Make a new table where columns = FIPS, rows = year -----------------------------------------------
# Transpose the new.summer data so that FIPS are in rows and years are in columns. --------------
#    Values in the first row will be the years and field names are assigned as 'V[n]'.
#    Transpose columns and rows in summer.precip, save as summer.t.
summer.t <- t(summer.precip)
#    turn matrix into a dataframe
summer.t <- as.data.frame(summer.t)
#    Renames columns using values in the 1st row of data ([1, ] references the 1st row).
colnames(summer.t) = summer.t[1, ]
#    Now that you've renamed the columns, eliminate the first row with the year values.
#    Removes the 1st-row year labels (selects all rows except the first and all columns).
summer.t = summer.t[-1, ]
#    Saves the transposed data back to summer.precip. Both are now the same.
summer.precip <- summer.t

