# Program for estimating trends in water use from USGS water use data
# Travis Warziniack, USFS Rocky Mountain Research Station

rm(list=ls())

library(readxl)
library(dplyr)

setwd("H:/")

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

# ---- domestic deliveries from public supply --- 

# checking which years of domestic deliveries from public supply:
# yes: 2015, 2010, 1995, 1990
# no: 2000

domestic1 <- merge(us2010, us2015, by='FIPS') 
domestic1 <- select(domestic1, "FIPS", "DO-PSPCp.x", "DO-PSPCp.y")
# select only counties with positive public deliveries
domestic1 <- subset(domestic1, domestic1$`DO-PSPCp.x` > 0)
domestic1$diff <- (domestic1$`DO-PSPCp.y`/ domestic1$`DO-PSPCp.x`)
domestic1$time <- 5
mean(domestic1$diff)

domestic2 <- merge(us2005, us2010, by='FIPS') 
domestic2$'DO-PSPCp.x' <- domestic2$`DO-PSDel.x` / domestic2$`PS-TOPop.x`
domestic2$'DO-PSPCp.y' <- domestic2$`DO-PSDel.y` / domestic2$`PS-TOPop.y`
domestic2 <- select(domestic2, "FIPS", "DO-PSPCp.x", "DO-PSPCp.y")
# select only counties with positive public deliveries
domestic2 <- subset(domestic2, domestic2$`DO-PSPCp.x` > 0)
domestic2 <- subset(domestic2, domestic2$`DO-PSPCp.y` > 0)
domestic2$diff <- (domestic2$`DO-PSPCp.y`/ domestic2$`DO-PSPCp.x`)
domestic2$time <- 10
mean(domestic2$diff)

rename(us1990, "DO-PSDel.x" = "do-psdel")
rename(us1995, "DO-PSDel.y" = "PS-DelDO")
domestic3 <- merge(us1990, us1995, by='FIPS') 
domestic3$'DO-PSPCp.x' <- domestic3$`do-psdel` / domestic3$`ps-popto`
domestic3$'DO-PSPCp.y' <- domestic3$`DO-PSDel` / domestic3$`DO-PSPop`
domestic3 <- select(domestic3, "FIPS", "DO-PSPCp.x", "DO-PSPCp.y")
# select only counties with positive public deliveries
domestic3 <- subset(domestic3, domestic3$`DO-PSPCp.x` > 0)
domestic3 <- subset(domestic3, domestic3$`DO-PSPCp.y` > 0)
domestic3$diff <- (domestic3$`DO-PSPCp.y`/domestic3$`DO-PSPCp.x`)
domestic3$time <- 15
domestic3<-subset(domestic3, (!is.na(domestic3[,4])))

mean(domestic1$diff)
mean(domestic2$diff)
mean(domestic3$diff)

annualr.1 <- mean(domestic1$diff)**(1/5) - 1
annualr.2 <- mean(domestic2$diff)**(1/5) - 1
annualr.3 <- mean(domestic3$diff)**(1/5) - 1

domestic <- rbind(domestic1, domestic2, domestic3)
d.rate <- lm(diff ~ log(time), data=domestic)
summary(d.rate)
