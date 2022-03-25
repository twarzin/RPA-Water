# Program for estimating trends in water use from USGS water use data
# Travis Warziniack, USFS Rocky Mountain Research Station

rm(list=ls())

library(readxl)
library(dplyr)

setwd("D:/")

us1985 <- read_excel("USGS raw water use/us85co.xls")
us1990 <- read_excel("USGS raw water use/us90co.xls")
us1995 <- read_excel("USGS raw water use/usco1995.xls")
us2000 <- read_excel("USGS raw water use/usco2000.xls")
us2005 <- read_excel("USGS raw water use/usco2005.xls")
us2010 <- read_excel("/USGS raw water use/usco2010.xlsx", sheet = "CountyData")
us2015 <- read_excel("/USGS raw water use/usco2015v2.0.xlsx", sheet = "usco2015v2.0", 
                     range=cell_rows(2:3225))

# creating FIPS fields from state and county codes
us1985$FIPS <- paste0(us1985$scode, us1985$area)
us1990$FIPS <- paste0(us1990$scode, us1990$area)
us1995$FIPS <- paste0(us1995$StateCode, us1995$CountyCode)


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

domestic1 <- merge(us2010, us2015, by='FIPS') 
domestic1 <- select(domestic1, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic1 <- subset(domestic1, domestic1$`DO-PSDel.x` > 0)
domestic1$diff <- (domestic1$`DO-PSDel.y`- domestic1$`DO-PSDel.x`)/domestic1$`DO-PSDel.x`
domestic1$time <- 5

domestic2 <- merge(us2005, us2010, by='FIPS') 
domestic2 <- select(domestic2, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic2 <- subset(domestic2, domestic2$`DO-PSDel.x` > 0)
domestic2$diff <- (domestic2$`DO-PSDel.y`- domestic2$`DO-PSDel.x`)/domestic2$`DO-PSDel.x`
domestic2$time <- 10

rename(us1990, "DO-PSDel" = "do-psdel")
domestic3 <- merge(us1990, us1995, by='FIPS') 
domestic3 <- select(domestic3, "FIPS", "DO-PSDel.x", "DO-PSDel.y")
# select only counties with positive public deliveries
domestic3 <- subset(domestic3, domestic3$`DO-PSDel.x` > 0)
domestic3$diff <- (domestic3$`DO-PSDel.y`- domestic3$`DO-PSDel.x`)/domestic3$`DO-PSDel.x`
domestic3$time <- 15



mean(domestic1$diff)
mean(domestic2$diff)

domestic <- rbind(domestic1, domestic2)
d.rate <- lm(diff ~ time, data=domestic)
summary(d.rate)
