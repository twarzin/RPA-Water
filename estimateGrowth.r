# Program for estimating trends in water use from USGS water use data
# Travis Warziniack, USFS Rocky Mountain Research Station

rm(list=ls())

library(readxl)
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


water <- merge(us1985, us1990, by="FIPS")

