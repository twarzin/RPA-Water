# Program for estimating trends in water use from USGS water use data
# Travis Warziniack, USFS Rocky Mountain Research Station

rm(list=ls())

library(readxl)
library(dplyr)

setwd("D:/")

# read in raw USGS water use data
us1985 <- read_excel("USGS raw water use/us85co.xls")
us1990 <- read_excel("USGS raw water use/us90co.xls")
us1995 <- read_excel("USGS raw water use/usco1995.xls")
us2000 <- read_excel("USGS raw water use/usco2000.xls")
us2005 <- read_excel("USGS raw water use/usco2005.xls")
us2010 <- read_excel("/USGS raw water use/usco2010.xlsx", sheet = "CountyData")
us2015 <- read_excel("/USGS raw water use/usco2015v2.0.xlsx", sheet = "usco2015v2.0", 
                     range=cell_rows(2:3225))

# creating FIPS fields from state and county codes. Some files have state codes
# and county/area codes. Need to combine fields
us1985$FIPS <- paste0(us1985$scode, us1985$area)
us1990$FIPS <- paste0(us1990$scode, us1990$area)
us1995$FIPS <- paste0(us1995$StateCode, us1995$CountyCode)

# define variable to designate year
us1985$YEAR = 1985
us1990$YEAR = 1990
us1995$YEAR = 1995
us2000$YEAR = 2000
us2005$YEAR = 2005
us2010$YEAR = 2010
us2015$YEAR = 2015

# need to keep relevant fields, subtract differences between years, then calculate 
# growth rates

water1 <- merge(us2010, us2015, by="FIPS")

# domestic deliveries from public supply

# checking which years of domestic deliveries from public supply:
# yes: 2015, 2010, 1995, 1990
# no: 2000

domestic5 <- merge(us2010, us2015, by='FIPS') 
domestic5 <- select(domestic5, "FIPS", "DO-PSPCp.x", "DO-PSPCp.y")
# select only counties with positive public deliveries
domestic5 <- subset(domestic5, domestic5$`DO-PSPCp.x` > 0)
domestic5$diff <- (domestic5$`DO-PSPCp.y`- domestic5$`DO-PSPCp.x`)/domestic5$`DO-PSPCp.x`
domestic5$time <- 5

domestic10 <- merge(us2005, us2010, by='FIPS') 
domestic10 <- select(domestic10, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic10 <- subset(domestic10, domestic10$`DO-PSDel.x` > 0)
domestic10$diff <- (domestic10$`DO-PSDel.y`- domestic10$`DO-PSDel.x`)/domestic10$`DO-PSDel.x`
domestic10$time <- 10

domestic20 <- merge(us1995, us2015, by='FIPS') 
domestic20 <- select(domestic20, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic20 <- subset(domestic20, domestic20$`DO-PSDel.x` > 0)
domestic20$diff <- (domestic20$`DO-PSDel.y`- domestic20$`DO-PSDel.x`)/domestic20$`DO-PSDel.x`
domestic20$time <- 20

rename(us1990, "DO-PSDel" = "do-psdel")
domestic25 <- merge(us1990, us1995, by='FIPS') 
domestic25 <- select(domestic25, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic25 <- subset(domestic25, domestic25$`DO-PSDel.x` > 0)
domestic25$diff <- (domestic25$`DO-PSDel.y`- domestic25$`DO-PSDel.x`)/domestic25$`DO-PSDel.x`
domestic25$time <- 25



mean(domestic5$diff)
mean(domestic10$diff)
mean(domestic20$diff)

domestic <- rbind(domestic5, domestic10, domestic20)
d.rate <- lm(diff ~ time, data=domestic)
summary(d.rate)

# Python code:
# wpuDPt = [baseline 2015 per-cap wd]*[exp[domestic wd growth]*[# of years past 2015]]
# dfWd["wpuDPt"] = dfWd["wpuDP0"] * np.exp(dfWd["DP.growth"]*(2015-dfWd["year_x"]))
