library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(tidyverse)
library(stringr)

### FYI - I used several files that were pulled from diff locations in Box. I'd pull those files and make a folder
### solely for these historical monthly projections

##### Water data #####
# This file is located in Box: 2020 RPA Assessment > Base Data
setwd("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand")
waterdemand <- read_excel("./TotalWaterDemands2015.xlsx")
waterdemand.cons <- waterdemand


# reformat state and county to obtain full FIPS
waterdemand.cons$state_cd <-formatC(waterdemand.cons$state_cd, width = 2, format = "d", flag = "0") # Adds a place holder for the state FIPS (2 digits)
waterdemand.cons$county_cd <-formatC(waterdemand.cons$county_cd, width = 3, format = "d", flag = "0") # Adds a place holder for the county FIPS (3 digits)
waterdemand.cons$FIPS <- paste(waterdemand.cons$state_cd, waterdemand.cons$county_cd, sep="") # This combines the state and county values


# Read in county watershed conversion data and reformat so we can merge with water data
# In Box this is located in 2020 RPA Assessment > Water Demand - Complete R code and input files > WEAP Input Creation
cty.huc <- read.csv("CountyWatershedPct.csv")
cty.huc <- select(cty.huc,FIPS, HUC_10)
cty.huc$FIPS2 <- floor(log10(cty.huc$FIPS)) + 1
cty.huc$FIPS <- ifelse(cty.huc$FIPS2==4,paste0("0",cty.huc$FIPS),cty.huc$FIPS)
cty.huc$HUC4 <- as.character(cty.huc$HUC_10)
cty.huc$HUC4 <- substr(cty.huc$HUC4,1,3)
cty.huc <- select(cty.huc,FIPS,HUC4)
duplicated(cty.huc)
cty.huc <- cty.huc[!duplicated(cty.huc$FIPS), ]


# Merge HUC and FIPS
fips.huc <- merge(waterdemand.cons,cty.huc)


#------------------------------------------------------------------------------------------.


# Add inputs output from water data cleanup R script that includes consumption ratios for all sectors except thermo
# In Box this is located in 2020 RPA Assessment > Water Demand - Complete R code and input files > WEAP Input Creation
# FYI - Consumptive proportions are the same for every SSP scenario
inputs<- read.csv("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/inputs_ssp1.csv")
inputs.cu <- select(inputs,fips,DP.cu.ratio,IC.cu.ratio,AQ.cu.ratio,LS.cu.ratio,IR.cu.ratio)

# FIPS column is missing leading 0 so this adds it so we can merge it with water data
inputs.cu$FIPS2 <- floor(log10(inputs.cu$fips)) + 1
inputs.cu$fips <- ifelse(inputs.cu$FIPS2==4,paste0("0",inputs.cu$fips),inputs.cu$fips)

# Add thermo consumption ratio which is a separate document retrieved from Tom's spreadsheet
# In Box, this info is located in Tom's excel file in 2020 RPA Assessment - "Use-huc4 nov20.xlsm"
# I merely copy + pasted the thermo CU proportions section and made a new excel sheet called thermo_cu
thermo.cu <- read_excel("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/thermo_cu.xlsx")

# Merge water data and consumption ratios from inputs file
cons.water <- merge(fips.huc,inputs.cu, by.x = "FIPS", by.y = "fips")
cons.water <- cons.water[order(cons.water$FIPS, cons.water$year),]

# Merge thermo data with water data
cons.water <- merge(cons.water,thermo.cu)
cons.water <- cons.water[order(cons.water$FIPS, cons.water$year),]

# Calculate consumptive use (mgal/day) for each sector
cons.water$dp.cu <- cons.water$`Domestic, total`*cons.water$DP.cu.ratio
cons.water$ic.cu <- cons.water$`C&I`*cons.water$IC.cu.ratio
cons.water$aq.cu <- cons.water$`Aquaculture, total`*cons.water$AQ.cu.ratio
cons.water$ls.cu <- cons.water$`Livestock, total`*cons.water$LS.cu.ratio
cons.water$ir.cu <- cons.water$`Irrigation, total`*cons.water$IR.cu.ratio
cons.water$th.cu <- cons.water$`Thermo, total`* cons.water$TH.cu.ratio
cons.water$tot.cu <- cons.water$dp.cu+cons.water$ic.cu+cons.water$aq.cu+
  cons.water$ls.cu+cons.water$ir.cu+cons.water$th.cu


#------------------------------------------------------------------------------------------.
# Calculate annual and monthly consumptive use

# Read in monthly data that will merge monthly proportions with annual data
# In Box this is located in 2020 RPA Assessment > Water Demand - Complete R code and input files > WEAP Input Creation
month <- read.csv("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/monthly.csv")


######## Domestic and public sector ########
dp.df <- select(cons.water,FIPS,HUC4,year,dp.cu)
DF <- dp.df %>%
  gather(key, value, dp.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF

month.dp <- select(month,1:13)

df.huc.month <- merge(df,month.dp, by="HUC4")
monthly <- df.huc.month

monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]


monthly <- select(monthly, 1,2,3,47:418)
dp.historic.monthly <- monthly
#write.csv(dp.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/DPhistoricmonthly.csv")

#------------------------------------------------------------------------------------------.
####### Industrial / Commercial sector ########
ic.df <- select(cons.water,FIPS,HUC4,year,ic.cu)
DF <- ic.df %>%
  gather(key, value, ic.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF

month.ic <- select(month,1,14:25)

df.huc.month <- merge(df,month.ic, by="HUC4")
monthly <- df.huc.month

monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]


monthly <- select(monthly, 1,2,3,47:418)
ic.historic.monthly <- monthly
#write.csv(ic.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/IChistoricmonthly.csv")



#------------------------------------------------------------------------------------------.
######### Irrigation sector #########
ir.df <- select(cons.water,FIPS,HUC4,year,ir.cu)
DF <- ir.df %>%
  gather(key, value, ir.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF

month.ir <- select(month,1,38:49)

df.huc.month <- merge(df,month.ir, by="HUC4")
monthly <- df.huc.month


monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]

monthly <- select(monthly, 1,2,3,47:418)
ir.historic.monthly <- monthly
#write.csv(ir.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/IRhistoricmonthly.csv")



#------------------------------------------------------------------------------------------.
######## Livestock sector #######
ls.df <- select(cons.water,FIPS,HUC4,year,ls.cu)
DF <- ls.df %>%
  gather(key, value, ls.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF

month.ls <- select(month,1,50:61)

df.huc.month <- merge(df,month.ls, by="HUC4")
monthly <- df.huc.month


monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]

monthly <- select(monthly, 1,2,3,47:418)
ls.historic.monthly <- monthly
#write.csv(ls.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/LShistoricmonthly.csv")



#------------------------------------------------------------------------------------------.
######## Thermo sector ########
th.df <- select(cons.water,FIPS,HUC4,year,th.cu)
DF <- th.df %>%
  gather(key, value, th.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF

month.th <- select(month,1,26:37)

df.huc.month <- merge(df,month.th, by="HUC4")
monthly <- df.huc.month


monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]

monthly <- select(monthly, 1,2,3,47:418)
th.historic.monthly <- monthly

#write.csv(th.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/THhistoricmonthly.csv")


#------------------------------------------------------------------------------------------.
####### Aquaculture sector #######
# Note: monthly proportions are not available for AQ, so instead used the same proportions as LS
aq.df <- select(cons.water,FIPS,HUC4,year,aq.cu)
DF <- aq.df %>%
  gather(key, value, aq.cu) %>%
  spread(year, value)
DF<-plyr::rename(DF,c("key"="sector","1985"="Y1985","1990"="Y1990","1995"="Y1995","2000"="Y2000","2005"="Y2005","2010"="Y2010","2015"="Y2015"))


DF$Y1986 <- DF$Y1985 + 1*(DF$Y1990 - DF$Y1985)/5
DF$Y1987 <- DF$Y1985 + 2*(DF$Y1990 - DF$Y1985)/5
DF$Y1988 <- DF$Y1985 + 3*(DF$Y1990 - DF$Y1985)/5
DF$Y1989 <- DF$Y1985 + 4*(DF$Y1990 - DF$Y1985)/5

DF$Y1991 <- DF$Y1990 + 1*(DF$Y1995 - DF$Y1990)/5
DF$Y1992 <- DF$Y1990 + 2*(DF$Y1995 - DF$Y1990)/5
DF$Y1993 <- DF$Y1990 + 3*(DF$Y1995 - DF$Y1990)/5
DF$Y1994 <- DF$Y1990 + 4*(DF$Y1995 - DF$Y1990)/5

DF$Y1996 <- DF$Y1995 + 1*(DF$Y2000 - DF$Y1995)/5
DF$Y1997 <- DF$Y1995 + 2*(DF$Y2000 - DF$Y1995)/5
DF$Y1998 <- DF$Y1995 + 3*(DF$Y2000 - DF$Y1995)/5
DF$Y1999 <- DF$Y1995 + 4*(DF$Y2000 - DF$Y1995)/5

DF$Y2001 <- DF$Y2000 + 1*(DF$Y2005 - DF$Y2000)/5
DF$Y2002 <- DF$Y2000 + 2*(DF$Y2005 - DF$Y2000)/5
DF$Y2003 <- DF$Y2000 + 3*(DF$Y2005 - DF$Y2000)/5
DF$Y2004 <- DF$Y2000 + 4*(DF$Y2005 - DF$Y2000)/5

DF$Y2006 <- DF$Y2005 + 1*(DF$Y2010 - DF$Y2005)/5
DF$Y2007 <- DF$Y2005 + 2*(DF$Y2010 - DF$Y2005)/5
DF$Y2008 <- DF$Y2005 + 3*(DF$Y2010 - DF$Y2005)/5
DF$Y2009 <- DF$Y2005 + 4*(DF$Y2010 - DF$Y2005)/5

DF$Y2011 <- DF$Y2010 + 1*(DF$Y2015 - DF$Y2010)/5
DF$Y2012 <- DF$Y2010 + 2*(DF$Y2015 - DF$Y2010)/5
DF$Y2013 <- DF$Y2010 + 3*(DF$Y2015 - DF$Y2010)/5
DF$Y2014 <- DF$Y2010 + 4*(DF$Y2015 - DF$Y2010)/5


col_order <- c("FIPS","HUC4","sector", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994",
               "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
               "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015")
DF <- DF[,col_order]
df <- DF


month.aq <- select(month,1,50:61)

df.huc.month <- merge(df,month.aq, by="HUC4")
monthly <- df.huc.month


monthly$Jan.85 <- monthly$Y1985 * monthly[,36]
monthly$Feb.85 <- monthly$Y1985 * monthly[,37]
monthly$Mar.85 <- monthly$Y1985 * monthly[,38]
monthly$Apr.85 <- monthly$Y1985 * monthly[,39]
monthly$May.85 <- monthly$Y1985 * monthly[,40]
monthly$Jun.85 <- monthly$Y1985 * monthly[,41]
monthly$Jul.85 <- monthly$Y1985 * monthly[,42]
monthly$Aug.85 <- monthly$Y1985 * monthly[,43]
monthly$Sep.85 <- monthly$Y1985 * monthly[,44]
monthly$Oct.85 <- monthly$Y1985 * monthly[,45]
monthly$Nov.85 <- monthly$Y1985 * monthly[,46]
monthly$Dec.85 <- monthly$Y1985 * monthly[,47]

monthly$Jan.86 <- monthly$Y1986 * monthly[,36]
monthly$Feb.86 <- monthly$Y1986 * monthly[,37]
monthly$Mar.86 <- monthly$Y1986 * monthly[,38]
monthly$Apr.86 <- monthly$Y1986 * monthly[,39]
monthly$May.86 <- monthly$Y1986 * monthly[,40]
monthly$Jun.86 <- monthly$Y1986 * monthly[,41]
monthly$Jul.86 <- monthly$Y1986 * monthly[,42]
monthly$Aug.86 <- monthly$Y1986 * monthly[,43]
monthly$Sep.86 <- monthly$Y1986 * monthly[,44]
monthly$Oct.86 <- monthly$Y1986 * monthly[,45]
monthly$Nov.86 <- monthly$Y1986 * monthly[,46]
monthly$Dec.86 <- monthly$Y1986 * monthly[,47]

monthly$Jan.87 <- monthly$Y1987 * monthly[,36]
monthly$Feb.87 <- monthly$Y1987 * monthly[,37]
monthly$Mar.87 <- monthly$Y1987 * monthly[,38]
monthly$Apr.87 <- monthly$Y1987 * monthly[,39]
monthly$May.87 <- monthly$Y1987 * monthly[,40]
monthly$Jun.87 <- monthly$Y1987 * monthly[,41]
monthly$Jul.87 <- monthly$Y1987 * monthly[,42]
monthly$Aug.87 <- monthly$Y1987 * monthly[,43]
monthly$Sep.87 <- monthly$Y1987 * monthly[,44]
monthly$Oct.87 <- monthly$Y1987 * monthly[,45]
monthly$Nov.87 <- monthly$Y1987 * monthly[,46]
monthly$Dec.87 <- monthly$Y1987 * monthly[,47]

monthly$Jan.88 <- monthly$Y1988 * monthly[,36]
monthly$Feb.88 <- monthly$Y1988 * monthly[,37]
monthly$Mar.88 <- monthly$Y1988 * monthly[,38]
monthly$Apr.88 <- monthly$Y1988 * monthly[,39]
monthly$May.88 <- monthly$Y1988 * monthly[,40]
monthly$Jun.88 <- monthly$Y1988 * monthly[,41]
monthly$Jul.88 <- monthly$Y1988 * monthly[,42]
monthly$Aug.88 <- monthly$Y1988 * monthly[,43]
monthly$Sep.88 <- monthly$Y1988 * monthly[,44]
monthly$Oct.88 <- monthly$Y1988 * monthly[,45]
monthly$Nov.88 <- monthly$Y1988 * monthly[,46]
monthly$Dec.88 <- monthly$Y1988 * monthly[,47]

monthly$Jan.89 <- monthly$Y1989 * monthly[,36]
monthly$Feb.89 <- monthly$Y1989 * monthly[,37]
monthly$Mar.89 <- monthly$Y1989 * monthly[,38]
monthly$Apr.89 <- monthly$Y1989 * monthly[,39]
monthly$May.89 <- monthly$Y1989 * monthly[,40]
monthly$Jun.89 <- monthly$Y1989 * monthly[,41]
monthly$Jul.89 <- monthly$Y1989 * monthly[,42]
monthly$Aug.89 <- monthly$Y1989 * monthly[,43]
monthly$Sep.89 <- monthly$Y1989 * monthly[,44]
monthly$Oct.89 <- monthly$Y1989 * monthly[,45]
monthly$Nov.89 <- monthly$Y1989 * monthly[,46]
monthly$Dec.89 <- monthly$Y1989 * monthly[,47]

monthly$Jan.90 <- monthly$Y1990 * monthly[,36]
monthly$Feb.90 <- monthly$Y1990 * monthly[,37]
monthly$Mar.90 <- monthly$Y1990 * monthly[,38]
monthly$Apr.90 <- monthly$Y1990 * monthly[,39]
monthly$May.90 <- monthly$Y1990 * monthly[,40]
monthly$Jun.90 <- monthly$Y1990 * monthly[,41]
monthly$Jul.90 <- monthly$Y1990 * monthly[,42]
monthly$Aug.90 <- monthly$Y1990 * monthly[,43]
monthly$Sep.90 <- monthly$Y1990 * monthly[,44]
monthly$Oct.90 <- monthly$Y1990 * monthly[,45]
monthly$Nov.90 <- monthly$Y1990 * monthly[,46]
monthly$Dec.90 <- monthly$Y1990 * monthly[,47]

monthly$Jan.91 <- monthly$Y1991 * monthly[,36]
monthly$Feb.91 <- monthly$Y1991 * monthly[,37]
monthly$Mar.91 <- monthly$Y1991 * monthly[,38]
monthly$Apr.91 <- monthly$Y1991 * monthly[,39]
monthly$May.91 <- monthly$Y1991 * monthly[,40]
monthly$Jun.91 <- monthly$Y1991 * monthly[,41]
monthly$Jul.91 <- monthly$Y1991 * monthly[,42]
monthly$Aug.91 <- monthly$Y1991 * monthly[,43]
monthly$Sep.91 <- monthly$Y1991 * monthly[,44]
monthly$Oct.91 <- monthly$Y1991 * monthly[,45]
monthly$Nov.91 <- monthly$Y1991 * monthly[,46]
monthly$Dec.91 <- monthly$Y1991 * monthly[,47]

monthly$Jan.92 <- monthly$Y1992 * monthly[,36]
monthly$Feb.92 <- monthly$Y1992 * monthly[,37]
monthly$Mar.92 <- monthly$Y1992 * monthly[,38]
monthly$Apr.92 <- monthly$Y1992 * monthly[,39]
monthly$May.92 <- monthly$Y1992 * monthly[,40]
monthly$Jun.92 <- monthly$Y1992 * monthly[,41]
monthly$Jul.92 <- monthly$Y1992 * monthly[,42]
monthly$Aug.92 <- monthly$Y1992 * monthly[,43]
monthly$Sep.92 <- monthly$Y1992 * monthly[,44]
monthly$Oct.92 <- monthly$Y1992 * monthly[,45]
monthly$Nov.92 <- monthly$Y1992 * monthly[,46]
monthly$Dec.92 <- monthly$Y1992 * monthly[,47]

monthly$Jan.93 <- monthly$Y1993 * monthly[,36]
monthly$Feb.93 <- monthly$Y1993 * monthly[,37]
monthly$Mar.93 <- monthly$Y1993 * monthly[,38]
monthly$Apr.93 <- monthly$Y1993 * monthly[,39]
monthly$May.93 <- monthly$Y1993 * monthly[,40]
monthly$Jun.93 <- monthly$Y1993 * monthly[,41]
monthly$Jul.93 <- monthly$Y1993 * monthly[,42]
monthly$Aug.93 <- monthly$Y1993 * monthly[,43]
monthly$Sep.93 <- monthly$Y1993 * monthly[,44]
monthly$Oct.93 <- monthly$Y1993 * monthly[,45]
monthly$Nov.93 <- monthly$Y1993 * monthly[,46]
monthly$Dec.93 <- monthly$Y1993 * monthly[,47]

monthly$Jan.94 <- monthly$Y1994 * monthly[,36]
monthly$Feb.94 <- monthly$Y1994 * monthly[,37]
monthly$Mar.94 <- monthly$Y1994 * monthly[,38]
monthly$Apr.94 <- monthly$Y1994 * monthly[,39]
monthly$May.94 <- monthly$Y1994 * monthly[,40]
monthly$Jun.94 <- monthly$Y1994 * monthly[,41]
monthly$Jul.94 <- monthly$Y1994 * monthly[,42]
monthly$Aug.94 <- monthly$Y1994 * monthly[,43]
monthly$Sep.94 <- monthly$Y1994 * monthly[,44]
monthly$Oct.94 <- monthly$Y1994 * monthly[,45]
monthly$Nov.94 <- monthly$Y1994 * monthly[,46]
monthly$Dec.94 <- monthly$Y1994 * monthly[,47]

monthly$Jan.95 <- monthly$Y1995 * monthly[,36]
monthly$Feb.95 <- monthly$Y1995 * monthly[,37]
monthly$Mar.95 <- monthly$Y1995 * monthly[,38]
monthly$Apr.95 <- monthly$Y1995 * monthly[,39]
monthly$May.95 <- monthly$Y1995 * monthly[,40]
monthly$Jun.95 <- monthly$Y1995 * monthly[,41]
monthly$Jul.95 <- monthly$Y1995 * monthly[,42]
monthly$Aug.95 <- monthly$Y1995 * monthly[,43]
monthly$Sep.95 <- monthly$Y1995 * monthly[,44]
monthly$Oct.95 <- monthly$Y1995 * monthly[,45]
monthly$Nov.95 <- monthly$Y1995 * monthly[,46]
monthly$Dec.95 <- monthly$Y1995 * monthly[,47]

monthly$Jan.96 <- monthly$Y1996 * monthly[,36]
monthly$Feb.96 <- monthly$Y1996 * monthly[,37]
monthly$Mar.96 <- monthly$Y1996 * monthly[,38]
monthly$Apr.96 <- monthly$Y1996 * monthly[,39]
monthly$May.96 <- monthly$Y1996 * monthly[,40]
monthly$Jun.96 <- monthly$Y1996 * monthly[,41]
monthly$Jul.96 <- monthly$Y1996 * monthly[,42]
monthly$Aug.96 <- monthly$Y1996 * monthly[,43]
monthly$Sep.96 <- monthly$Y1996 * monthly[,44]
monthly$Oct.96 <- monthly$Y1996 * monthly[,45]
monthly$Nov.96 <- monthly$Y1996 * monthly[,46]
monthly$Dec.96 <- monthly$Y1996 * monthly[,47]

monthly$Jan.97 <- monthly$Y1997 * monthly[,36]
monthly$Feb.97 <- monthly$Y1997 * monthly[,37]
monthly$Mar.97 <- monthly$Y1997 * monthly[,38]
monthly$Apr.97 <- monthly$Y1997 * monthly[,39]
monthly$May.97 <- monthly$Y1997 * monthly[,40]
monthly$Jun.97 <- monthly$Y1997 * monthly[,41]
monthly$Jul.97 <- monthly$Y1997 * monthly[,42]
monthly$Aug.97 <- monthly$Y1997 * monthly[,43]
monthly$Sep.97 <- monthly$Y1997 * monthly[,44]
monthly$Oct.97 <- monthly$Y1997 * monthly[,45]
monthly$Nov.97 <- monthly$Y1997 * monthly[,46]
monthly$Dec.97 <- monthly$Y1997 * monthly[,47]

monthly$Jan.98 <- monthly$Y1998 * monthly[,36]
monthly$Feb.98 <- monthly$Y1998 * monthly[,37]
monthly$Mar.98 <- monthly$Y1998 * monthly[,38]
monthly$Apr.98 <- monthly$Y1998 * monthly[,39]
monthly$May.98 <- monthly$Y1998 * monthly[,40]
monthly$Jun.98 <- monthly$Y1998 * monthly[,41]
monthly$Jul.98 <- monthly$Y1998 * monthly[,42]
monthly$Aug.98 <- monthly$Y1998 * monthly[,43]
monthly$Sep.98 <- monthly$Y1998 * monthly[,44]
monthly$Oct.98 <- monthly$Y1998 * monthly[,45]
monthly$Nov.98 <- monthly$Y1998 * monthly[,46]
monthly$Dec.98 <- monthly$Y1998 * monthly[,47]

monthly$Jan.99 <- monthly$Y1999 * monthly[,36]
monthly$Feb.99 <- monthly$Y1999 * monthly[,37]
monthly$Mar.99 <- monthly$Y1999 * monthly[,38]
monthly$Apr.99 <- monthly$Y1999 * monthly[,39]
monthly$May.99 <- monthly$Y1999 * monthly[,40]
monthly$Jun.99 <- monthly$Y1999 * monthly[,41]
monthly$Jul.99 <- monthly$Y1999 * monthly[,42]
monthly$Aug.99 <- monthly$Y1999 * monthly[,43]
monthly$Sep.99 <- monthly$Y1999 * monthly[,44]
monthly$Oct.99 <- monthly$Y1999 * monthly[,45]
monthly$Nov.99 <- monthly$Y1999 * monthly[,46]
monthly$Dec.99 <- monthly$Y1999 * monthly[,47]

monthly$Jan.00 <- monthly$Y2000 * monthly[,36]
monthly$Feb.00 <- monthly$Y2000 * monthly[,37]
monthly$Mar.00 <- monthly$Y2000 * monthly[,38]
monthly$Apr.00 <- monthly$Y2000 * monthly[,39]
monthly$May.00 <- monthly$Y2000 * monthly[,40]
monthly$Jun.00 <- monthly$Y2000 * monthly[,41]
monthly$Jul.00 <- monthly$Y2000 * monthly[,42]
monthly$Aug.00 <- monthly$Y2000 * monthly[,43]
monthly$Sep.00 <- monthly$Y2000 * monthly[,44]
monthly$Oct.00 <- monthly$Y2000 * monthly[,45]
monthly$Nov.00 <- monthly$Y2000 * monthly[,46]
monthly$Dec.00 <- monthly$Y2000 * monthly[,47]

monthly$Jan.01 <- monthly$Y2001 * monthly[,36]
monthly$Feb.01 <- monthly$Y2001 * monthly[,37]
monthly$Mar.01 <- monthly$Y2001 * monthly[,38]
monthly$Apr.01 <- monthly$Y2001 * monthly[,39]
monthly$May.01 <- monthly$Y2001 * monthly[,40]
monthly$Jun.01 <- monthly$Y2001 * monthly[,41]
monthly$Jul.01 <- monthly$Y2001 * monthly[,42]
monthly$Aug.01 <- monthly$Y2001 * monthly[,43]
monthly$Sep.01 <- monthly$Y2001 * monthly[,44]
monthly$Oct.01 <- monthly$Y2001 * monthly[,45]
monthly$Nov.01 <- monthly$Y2001 * monthly[,46]
monthly$Dec.01 <- monthly$Y2001 * monthly[,47]

monthly$Jan.02 <- monthly$Y2002 * monthly[,36]
monthly$Feb.02 <- monthly$Y2002 * monthly[,37]
monthly$Mar.02 <- monthly$Y2002 * monthly[,38]
monthly$Apr.02 <- monthly$Y2002 * monthly[,39]
monthly$May.02 <- monthly$Y2002 * monthly[,40]
monthly$Jun.02 <- monthly$Y2002 * monthly[,41]
monthly$Jul.02 <- monthly$Y2002 * monthly[,42]
monthly$Aug.02 <- monthly$Y2002 * monthly[,43]
monthly$Sep.02 <- monthly$Y2002 * monthly[,44]
monthly$Oct.02 <- monthly$Y2002 * monthly[,45]
monthly$Nov.02 <- monthly$Y2002 * monthly[,46]
monthly$Dec.02 <- monthly$Y2002 * monthly[,47]

monthly$Jan.03 <- monthly$Y2003 * monthly[,36]
monthly$Feb.03 <- monthly$Y2003 * monthly[,37]
monthly$Mar.03 <- monthly$Y2003 * monthly[,38]
monthly$Apr.03 <- monthly$Y2003 * monthly[,39]
monthly$May.03 <- monthly$Y2003 * monthly[,40]
monthly$Jun.03 <- monthly$Y2003 * monthly[,41]
monthly$Jul.03 <- monthly$Y2003 * monthly[,42]
monthly$Aug.03 <- monthly$Y2003 * monthly[,43]
monthly$Sep.03 <- monthly$Y2003 * monthly[,44]
monthly$Oct.03 <- monthly$Y2003 * monthly[,45]
monthly$Nov.03 <- monthly$Y2003 * monthly[,46]
monthly$Dec.03 <- monthly$Y2003 * monthly[,47]


monthly$Jan.04 <- monthly$Y2004 * monthly[,36]
monthly$Feb.04 <- monthly$Y2004 * monthly[,37]
monthly$Mar.04 <- monthly$Y2004 * monthly[,38]
monthly$Apr.04 <- monthly$Y2004 * monthly[,39]
monthly$May.04 <- monthly$Y2004 * monthly[,40]
monthly$Jun.04 <- monthly$Y2004 * monthly[,41]
monthly$Jul.04 <- monthly$Y2004 * monthly[,42]
monthly$Aug.04 <- monthly$Y2004 * monthly[,43]
monthly$Sep.04 <- monthly$Y2004 * monthly[,44]
monthly$Oct.04 <- monthly$Y2004 * monthly[,45]
monthly$Nov.04 <- monthly$Y2004 * monthly[,46]
monthly$Dec.04 <- monthly$Y2004 * monthly[,47]


monthly$Jan.05 <- monthly$Y2005 * monthly[,36]
monthly$Feb.05 <- monthly$Y2005 * monthly[,37]
monthly$Mar.05 <- monthly$Y2005 * monthly[,38]
monthly$Apr.05 <- monthly$Y2005 * monthly[,39]
monthly$May.05 <- monthly$Y2005 * monthly[,40]
monthly$Jun.05 <- monthly$Y2005 * monthly[,41]
monthly$Jul.05 <- monthly$Y2005 * monthly[,42]
monthly$Aug.05 <- monthly$Y2005 * monthly[,43]
monthly$Sep.05 <- monthly$Y2005 * monthly[,44]
monthly$Oct.05 <- monthly$Y2005 * monthly[,45]
monthly$Nov.05 <- monthly$Y2005 * monthly[,46]
monthly$Dec.05 <- monthly$Y2005 * monthly[,47]


monthly$Jan.06 <- monthly$Y2006 * monthly[,36]
monthly$Feb.06 <- monthly$Y2006 * monthly[,37]
monthly$Mar.06 <- monthly$Y2006 * monthly[,38]
monthly$Apr.06 <- monthly$Y2006 * monthly[,39]
monthly$May.06 <- monthly$Y2006 * monthly[,40]
monthly$Jun.06 <- monthly$Y2006 * monthly[,41]
monthly$Jul.06 <- monthly$Y2006 * monthly[,42]
monthly$Aug.06 <- monthly$Y2006 * monthly[,43]
monthly$Sep.06 <- monthly$Y2006 * monthly[,44]
monthly$Oct.06 <- monthly$Y2006 * monthly[,45]
monthly$Nov.06 <- monthly$Y2006 * monthly[,46]
monthly$Dec.06 <- monthly$Y2006 * monthly[,47]


monthly$Jan.07 <- monthly$Y2007 * monthly[,36]
monthly$Feb.07 <- monthly$Y2007 * monthly[,37]
monthly$Mar.07 <- monthly$Y2007 * monthly[,38]
monthly$Apr.07 <- monthly$Y2007 * monthly[,39]
monthly$May.07 <- monthly$Y2007 * monthly[,40]
monthly$Jun.07 <- monthly$Y2007 * monthly[,41]
monthly$Jul.07 <- monthly$Y2007 * monthly[,42]
monthly$Aug.07 <- monthly$Y2007 * monthly[,43]
monthly$Sep.07 <- monthly$Y2007 * monthly[,44]
monthly$Oct.07 <- monthly$Y2007 * monthly[,45]
monthly$Nov.07 <- monthly$Y2007 * monthly[,46]
monthly$Dec.07 <- monthly$Y2007 * monthly[,47]


monthly$Jan.08 <- monthly$Y2008 * monthly[,36]
monthly$Feb.08 <- monthly$Y2008 * monthly[,37]
monthly$Mar.08 <- monthly$Y2008 * monthly[,38]
monthly$Apr.08 <- monthly$Y2008 * monthly[,39]
monthly$May.08 <- monthly$Y2008 * monthly[,40]
monthly$Jun.08 <- monthly$Y2008 * monthly[,41]
monthly$Jul.08 <- monthly$Y2008 * monthly[,42]
monthly$Aug.08 <- monthly$Y2008 * monthly[,43]
monthly$Sep.08 <- monthly$Y2008 * monthly[,44]
monthly$Oct.08 <- monthly$Y2008 * monthly[,45]
monthly$Nov.08 <- monthly$Y2008 * monthly[,46]
monthly$Dec.08 <- monthly$Y2008 * monthly[,47]


monthly$Jan.09 <- monthly$Y2009 * monthly[,36]
monthly$Feb.09 <- monthly$Y2009 * monthly[,37]
monthly$Mar.09 <- monthly$Y2009 * monthly[,38]
monthly$Apr.09 <- monthly$Y2009 * monthly[,39]
monthly$May.09 <- monthly$Y2009 * monthly[,40]
monthly$Jun.09 <- monthly$Y2009 * monthly[,41]
monthly$Jul.09 <- monthly$Y2009 * monthly[,42]
monthly$Aug.09 <- monthly$Y2009 * monthly[,43]
monthly$Sep.09 <- monthly$Y2009 * monthly[,44]
monthly$Oct.09 <- monthly$Y2009 * monthly[,45]
monthly$Nov.09 <- monthly$Y2009 * monthly[,46]
monthly$Dec.09 <- monthly$Y2009 * monthly[,47]

monthly$Jan.10 <- monthly$Y2010 * monthly[,36]
monthly$Feb.10 <- monthly$Y2010 * monthly[,37]
monthly$Mar.10 <- monthly$Y2010 * monthly[,38]
monthly$Apr.10 <- monthly$Y2010 * monthly[,39]
monthly$May.10 <- monthly$Y2010 * monthly[,40]
monthly$Jun.10 <- monthly$Y2010 * monthly[,41]
monthly$Jul.10 <- monthly$Y2010 * monthly[,42]
monthly$Aug.10 <- monthly$Y2010 * monthly[,43]
monthly$Sep.10 <- monthly$Y2010 * monthly[,44]
monthly$Oct.10 <- monthly$Y2010 * monthly[,45]
monthly$Nov.10 <- monthly$Y2010 * monthly[,46]
monthly$Dec.10 <- monthly$Y2010 * monthly[,47]

monthly$Jan.11 <- monthly$Y2011 * monthly[,36]
monthly$Feb.11 <- monthly$Y2011 * monthly[,37]
monthly$Mar.11 <- monthly$Y2011 * monthly[,38]
monthly$Apr.11 <- monthly$Y2011 * monthly[,39]
monthly$May.11 <- monthly$Y2011 * monthly[,40]
monthly$Jun.11 <- monthly$Y2011 * monthly[,41]
monthly$Jul.11 <- monthly$Y2011 * monthly[,42]
monthly$Aug.11 <- monthly$Y2011 * monthly[,43]
monthly$Sep.11 <- monthly$Y2011 * monthly[,44]
monthly$Oct.11 <- monthly$Y2011 * monthly[,45]
monthly$Nov.11 <- monthly$Y2011 * monthly[,46]
monthly$Dec.11 <- monthly$Y2011 * monthly[,47]

monthly$Jan.12 <- monthly$Y2012 * monthly[,36]
monthly$Feb.12 <- monthly$Y2012 * monthly[,37]
monthly$Mar.12 <- monthly$Y2012 * monthly[,38]
monthly$Apr.12 <- monthly$Y2012 * monthly[,39]
monthly$May.12 <- monthly$Y2012 * monthly[,40]
monthly$Jun.12 <- monthly$Y2012 * monthly[,41]
monthly$Jul.12 <- monthly$Y2012 * monthly[,42]
monthly$Aug.12 <- monthly$Y2012 * monthly[,43]
monthly$Sep.12 <- monthly$Y2012 * monthly[,44]
monthly$Oct.12 <- monthly$Y2012 * monthly[,45]
monthly$Nov.12 <- monthly$Y2012 * monthly[,46]
monthly$Dec.12 <- monthly$Y2012 * monthly[,47]

monthly$Jan.13 <- monthly$Y2013 * monthly[,36]
monthly$Feb.13 <- monthly$Y2013 * monthly[,37]
monthly$Mar.13 <- monthly$Y2013 * monthly[,38]
monthly$Apr.13 <- monthly$Y2013 * monthly[,39]
monthly$May.13 <- monthly$Y2013 * monthly[,40]
monthly$Jun.13 <- monthly$Y2013 * monthly[,41]
monthly$Jul.13 <- monthly$Y2013 * monthly[,42]
monthly$Aug.13 <- monthly$Y2013 * monthly[,43]
monthly$Sep.13 <- monthly$Y2013 * monthly[,44]
monthly$Oct.13 <- monthly$Y2013 * monthly[,45]
monthly$Nov.13 <- monthly$Y2013 * monthly[,46]
monthly$Dec.13 <- monthly$Y2013 * monthly[,47]

monthly$Jan.14 <- monthly$Y2014 * monthly[,36]
monthly$Feb.14 <- monthly$Y2014 * monthly[,37]
monthly$Mar.14 <- monthly$Y2014 * monthly[,38]
monthly$Apr.14 <- monthly$Y2014 * monthly[,39]
monthly$May.14 <- monthly$Y2014 * monthly[,40]
monthly$Jun.14 <- monthly$Y2014 * monthly[,41]
monthly$Jul.14 <- monthly$Y2014 * monthly[,42]
monthly$Aug.14 <- monthly$Y2014 * monthly[,43]
monthly$Sep.14 <- monthly$Y2014 * monthly[,44]
monthly$Oct.14 <- monthly$Y2014 * monthly[,45]
monthly$Nov.14 <- monthly$Y2014 * monthly[,46]
monthly$Dec.14 <- monthly$Y2014 * monthly[,47]

monthly$Jan.15 <- monthly$Y2015 * monthly[,36]
monthly$Feb.15 <- monthly$Y2015 * monthly[,37]
monthly$Mar.15 <- monthly$Y2015 * monthly[,38]
monthly$Apr.15 <- monthly$Y2015 * monthly[,39]
monthly$May.15 <- monthly$Y2015 * monthly[,40]
monthly$Jun.15 <- monthly$Y2015 * monthly[,41]
monthly$Jul.15 <- monthly$Y2015 * monthly[,42]
monthly$Aug.15 <- monthly$Y2015 * monthly[,43]
monthly$Sep.15 <- monthly$Y2015 * monthly[,44]
monthly$Oct.15 <- monthly$Y2015 * monthly[,45]
monthly$Nov.15 <- monthly$Y2015 * monthly[,46]
monthly$Dec.15 <- monthly$Y2015 * monthly[,47]

monthly <- select(monthly, 1,2,3,47:418)
aq.historic.monthly <- monthly
#write.csv(aq.historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/AQhistoricmonthly.csv")



historic.monthly <- rbind(dp.historic.monthly,ic.historic.monthly,ir.historic.monthly,ls.historic.monthly,th.historic.monthly,aq.historic.monthly)
#write.csv(historic.monthly, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/HistoricMonthly.csv")



#------------------------------------------------------------------.


dp.historic.monthly$TotalDP <- rowSums( dp.historic.monthly[,4:375] )
dp.historic <- select(dp.historic.monthly,HUC4,FIPS,TotalDP)

ic.historic.monthly$TotalIC <- rowSums( ic.historic.monthly[,4:375] )
ic.historic <- select(ic.historic.monthly,HUC4,FIPS,TotalIC)

ir.historic.monthly$TotalIR <- rowSums( ir.historic.monthly[,4:375] )
ir.historic <- select(ir.historic.monthly,HUC4,FIPS,TotalIR)

th.historic.monthly$TotalTH <- rowSums( th.historic.monthly[,4:375] )
th.historic <- select(th.historic.monthly,HUC4,FIPS,TotalTH)

ls.historic.monthly$TotalLS <- rowSums( ls.historic.monthly[,4:375] )
ls.historic <- select(ls.historic.monthly,HUC4,FIPS,TotalLS)

aq.historic.monthly$TotalAQ <- rowSums( aq.historic.monthly[,4:375] )
aq.historic <- select(aq.historic.monthly,HUC4,FIPS,TotalAQ)


historic.totals <- merge(dp.historic,ic.historic)
historic.totals <- merge(historic.totals, ir.historic)
historic.totals <- merge(historic.totals, th.historic)
historic.totals <- merge(historic.totals, ls.historic)
historic.totals <- merge(historic.totals, aq.historic)

historic.totals$Total <- rowSums(historic.totals[,3:8])
write.csv(historic.totals, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/TestingHistoricalMonthly/HistoricTotals.csv")



