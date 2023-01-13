# Testing
# hi test 2

# Program file for RPA Water Demand Model
# Travis Warziniack
# US Forest Service
# travis.w.warziniack@usda.gov

# to do:
# - Does industrial need to be total income or per capita income?
# - should ag climate be based on pet instead of precip?

rm(list = ls())  # clears memory

# Set working directory to file location
# for Pam: 
setwd("C:/Users/twwarziniack/Documents/5_RPA/Demand model")
#E:/WaterDemand/WaterDemandProject/DataWaterDemand")
#setwd("D:/Demand model")

# Set working directory to same location of the R file location
base.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(base.dir)

library(tidyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(dplyr)  # Has the pipe operator %>%.
library(data.table)
library(openxlsx)

# Notes for faster functions
'
1) "data.table::fread" is faster than "read.csv" but I use "%>% as.data.frame" at the end
fread doesnot change the column names even if they are not allowed, 
so I will limit its use here not to distribe the original code

2) "data.table::fwrite" is faster than "write.csv" and by default donot include the row names

3) "dplyr::inner_join" is quivalent but faster than "merge" , other options are full_join and left_join

4) "dplyr::arrange" is faster than attaching and using the order function

5) I replaced the for loop with a different login that run in 3 min now instsead of 2 hr

'

# Read Data ------------------------
# load baseline data

# Population and income projections are from Wear and Prestemon
# Population by FIPS: fields are ID (x), fips, year, pop, ssp, inc; 860,440 records
# Population is in thousands
# Income is in __________
pop.inc <- data.table::fread("1_BaseData/popinc_proj.csv") %>% as.data.frame()
# projections of irrigated acreage for ag
acre.data <- data.table::fread('1_BaseData/acredata-use.csv', header=TRUE) %>% as.data.frame()

# acre.data does not vary by ssp, so collapse to shorten merges
acre.data <- subset(acre.data, ssp == "ssp1")
acre.data <- acre.data %>%
  select(fips, year, acres)
# combine projection data
proj.data <- dplyr::inner_join(pop.inc, acre.data, by=c("fips", "year"))

proj.data <- proj.data %>%
  select(fips, year, pop, ssp, inc, acres)

# Water withdrawals in 2015 from USGS (3,223 records):
# Data fields:
# DO.WDelv  Domestic, total use (withdrawals + deliveries), in Mgal/d
# IN.WFrTo  Industrial, self-supplied total withdrawals, fresh, in Mgal/d
# IR.WFrTo  Irrigation, total withdrawals, fresh, in Mgal/d
# IR.CUsFr  Irrigation, total consumptive use, fresh, in Mgal/d
# IR.IrTot  Irrigation, acres irrigated, total, in thousand acres
# LI.WFrTo  Livestock, total withdrawals, fresh, in Mgal/d
# AQ.WFrTo  Aquaculture, total withdrawals, fresh, in Mgal/d
# MI.WFrTo  Mining, total withdrawals, fresh, in Mgal/d
# PT.WFrTo  Thermoelectric, total withdrawals, fresh, in Mgal/d
# PT.PSDel  Thermoelectric, deliveries from Public Supply, in Mgal/d
# PT.CUsFr  Thermoelectric, total consumptive use, fresh, in Mgal/d
# PT.Power  Thermoelectric, power generated, in gigawatt-hours

wd.2015 <- read.csv("1_BaseData/USGS2015.csv")
wd.2015 <- wd.2015 %>%
  select(FIPS,
         'DO.WDelv',
         'IN.WFrTo',
         'IR.WFrTo',
         'IR.CUsFr',
         'IR.IrTot',
         'LI.WFrTo',
         'AQ.WFrTo',
         'MI.WFrTo',
         'PT.WFrTo',
         'PT.PSDel',
         'PT.CUsFr',
         'PT.Power')

# FIPS codes are off between USGS data and population projections. Will need to adjust
# population data later (outside of R)

# Create baseline withdrawals for surface water fresh for each sector
# - first calculate total withdrawals for public supply, then calculate percent
#   of those withdrawals from surface water. Some values read in as character
#   so need to convert to numeric
wd.2015[] <- lapply(wd.2015, as.numeric)
#   convert NAs to zeros
wd.2015[is.na(wd.2015)] <- 0

# Creating variables for total sector withdrawals + deliveries
wd.2015$dom   <- wd.2015$DO.WDelv
wd.2015$ind   <- wd.2015$IN.WFrTo + wd.2015$MI.WFrTo
wd.2015$therm <- wd.2015$PT.WFrTo + wd.2015$PT.PSDel
wd.2015$ag    <- wd.2015$IR.WFrTo
wd.2015$la    <- wd.2015$LI.WFrTo + wd.2015$AQ.WFrTo

# Check total withdrawals for baseline data
wd.2015$total <- wd.2015$dom + wd.2015$ind + wd.2015$therm + 
  wd.2015$ag + wd.2015$la

all.wd.2015 <- sum(wd.2015$total)

print(paste0('2015 total withdrawal is ',all.wd.2015,' MGD'))

# baseline demand driver data from USGS
wd.2015$acres <- wd.2015$IR.IrTot
wd.2015$power <- wd.2015$PT.Power

# rename fips to lowercase to match other data files
wd.2015 <- rename(wd.2015, fips = FIPS)

# Growth and decay rates:
# Growth and decay rates for withdrawals per unit are taken from Tom Brown's work.
# This file also has a variable denoting whether the county in in the eastern or
# western United States
growth <- read.csv("1_BaseData/WDGrowthCU.csv")

# First calculate withdrawals for each sector without climate impacts. Climate
# impacts are added in a separate section at the bottom of this code

#---------------------------------------------------------------------------------.
# -   SECTOR PROJECTIONS TO 2070 - no climate #########################
# This calculates projections out to 2070 using function from
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

pop.inc.2015 <- subset(pop.inc, year == 2015)

# join population projections with base year withdrawals data
demand.init1 <- merge(pop.inc.2015, wd.2015, by = "fips")

# select variables needed for calcuations:
# Note, Pop is from withdrawal data. pop used here is from population projections
keeps <- c("fips","year","ssp","inc","pop","dom","ag","ind","therm","la",
           "acres", "power")
demand.init <- demand.init1[,names(demand.init1) %in% keeps]

# calculate initial withdrawals per unit
demand.init$wpu.dom   <- demand.init$dom / demand.init$pop
demand.init$wpu.ind   <- demand.init$ind / demand.init$inc
demand.init$wpu.ag    <- demand.init$ag / demand.init$acres
demand.init$wpu.therm <- demand.init$therm / demand.init$power
# need to add thermo-electric, and aquaculture

# create dataframe for projections
demand.proj <- subset(proj.data, year != 2015)

demand.proj$dom <- NA
demand.proj$ind <- NA
demand.proj$therm <- NA
demand.proj$ag <- NA
demand.proj$la <- NA
demand.proj$power <- NA
demand.proj$wpu.dom <- NA
demand.proj$wpu.ind <- NA
demand.proj$wpu.ag <- NA
demand.proj$wpu.therm <- NA

demand <- rbind(demand.init, demand.proj)

demand <- dplyr::inner_join(demand, growth, by="fips")

# # order data so I can run a loop on lagged values
demand <- demand %>% dplyr::arrange(fips,ssp,year)

# # be sure to sort first!!
demand2 <- demand 
for(i in 1:55) {
  demand2 <- demand2 %>%
    group_by(fips,ssp) %>%
    dplyr::mutate(
      wpu.dom = ifelse(is.na(wpu.dom), dplyr::lag(wpu.dom) * (1+DP.growth*(1+DP.decay)^(year-2015)),wpu.dom),
      wpu.ind = ifelse(is.na(wpu.ind), dplyr::lag(wpu.ind) * (1+IC.growth*(1+IC.decay)^(year-2015)),wpu.ind),
      wpu.ag  = ifelse(is.na(wpu.ag),  dplyr::lag(wpu.ag)  * (1+IR.growth*(1+IR.decay)^(year-2015)),wpu.ag))
  }

demand2 <- demand2 %>%
  ungroup()

# export the above results so we don't have to run them every time
data.table::fwrite(demand2, file="demand_temp_Ahmed.csv")

rm(demand,demand2)

# assuming the above loop has run, read in demand-temp
demand <- fread(file="demand_temp_Ahmed.csv") %>% as.data.frame()

# calculate annual withdrawals for each sector
demand <- demand %>%
  dplyr::mutate(dom.t = pop * wpu.dom,
                ind.t = inc * wpu.ind,
                ag.t  = acres * wpu.ag)

names(demand)

demand.noCC <- demand

keeps <- c("fips","state","county","year","ssp","inc","pop","acres","wpu.dom","wpu.ind","wpu.ag","dom.t","ag.t","ind.t")
demand.noCC <- demand.noCC[,names(demand.noCC) %in% keeps]

# check total demands
# thermal (therm), and life stock and aquaculture (la) are missing in withdrawal projections
# So the total withdrawal are misleading
# will come back to this

demand.total <- demand %>% 
  dplyr::select(year,ssp,dom.t,ind.t,ag.t,therm,la) %>%
  dplyr::group_by(year,ssp) %>%
  dplyr::summarise_all(sum,na.rm = T) %>% 
  dplyr::ungroup() %>%
  as.data.frame()

demand.total <- demand.total %>%
  mutate(total.ag.dom.ind = dom.t + ind.t + ag.t,
         total = dom.t + ind.t + ag.t + therm + la)


'
Last Line I checked
Ahmed
"Sep,30,2022
'

# --  PROJECTIONS WITH CLIMATE -------------

# Climate impacts the withdrawals needed per unit. Equations for climate effects
# can be found in:
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

# Domestic water use: Climate change only affects outdoor water use for domestic uses
# during the growing season (April - September). Pam Froemke has created 
# files for summer precip and et used here. That code is in the same GitHub 
# repository

# read in summer precip data
precip.data <- read.xlsx(
  xlsxFile="1_ClimateData/SummerPrecip.xlsx",
  sheet = 2,
  startRow = 1,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = FALSE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  sep.names = ".",
  namedRegion = NULL,
  na.strings = "NA",
  fillMergedCells = FALSE
)

colnames(precip.data)[colnames(precip.data) == "FIPS"] <- "fips"
colnames(precip.data)[colnames(precip.data) == "Year"] <- "year"

# subset demand to test code
precip.data <- subset(precip.data, FIPS < 1005)
# if demand not already subsetted, 
demand <- subset(demand, fips < 1005)

demand <- merge(demand, precip.data, by=c('fips','year'))
demand$delta.sprecip <- (100 + demand$PctChangePrecip) / 100

# # for testing, we'll create fake changes in growing season pet:
# demand$delta.spet <- rnorm(1, mean=1, sd=1)
demand$delta.spet <- 0
demand$ChangeSummerET <- demand$delta.spet

# --- domestic water use with climate change: 

# the following coefficients are taken from Tom Brown's work for the 2010 RPA Assessment
# the coefficients give the change in gallons per capita per day for a 1cm change in precip 
# and ET
cc.dp1 <- -1.415    # coefficient on change in summertime precip
cc.dp2 <- 0.778     # coefficient on change in pet

# convert precip data in mm height to cm height 
demand$ChangeSummerPrecip.cm <- demand$ChangeSummerPrecip * 0.1

# precip.data$ChangeSummerPrecip.meters <- precip.data$ChangeSummerPrecip / 1000
# # read in county area to turn height into volume
# countyArea <- read.csv(file="1_BaseData/CountyAreas.csv")
# colnames(countyArea)[colnames(countyArea) == "FIPS"] <- "fips"
# precip.data <- merge(precip.data, countyArea, by="fips")
# colnames(precip.data)[colnames(precip.data) == "Shape_Area..m2."] <- "area"
# precip.data$ChangeSummerP.volume <- precip.data$ChangeSummerPrecip.meters * precip.data$area 
# # divide by number of days in growing season April - Sept
# precip.data$ChangeSummerPrecip <- precip.data$ChangeSummerP.volume / (6*30)
# # convert m3/day to gallons / day
# precip.data$ChangeSummerPrecipGD <- precip.data$ChangeSummerPrecip * 264
# precip.data$ChangeSummerPrecipMGD <- precip.data$ChangeSummerPrecipGD / 1000000

### change in ag

### verify that the following is additive
demand$wpu.dp.cc <- demand$wpu.dom + (cc.dp1*demand$ChangeSummerPrecip.cm + cc.dp2*demand$ChangeSummerET) / 1000
# the original code did not have the last term and divided by 1000
# domestic demand with climate change:
demand$dom.cc <- demand$wpu.dp.cc * demand$pop

# agricultural water use with climate change:
# Multiply wpu by percentage change in summer precip to get 
# demands with climate impacts
demand$wpu.ag.cc <- demand$wpu.ag * (1/demand$delta.sprecip)
demand$ag.cc <- demand$wpu.ag.cc * demand$acres

# comparing domestic demand with and without climate change
keeps <- c("fips","year","ssp","GCM","RCP","dom.t","dom.cc","ag.t","ag.cc")
dcheck <- demand
dcheck <- dcheck[,names(dcheck) %in% keeps]
dcheck$compare.dom <- dcheck$dom.cc / dcheck$dom.t
dcheck$compare.ag <- dcheck$ag.cc / dcheck$ag.t

write.csv(demand, file="demand-final.csv")

# --- OUTPUT FORMAT ----

# Output for reports, etc. 
# extracting domestic estimates
dom.wpu <- demand

dom.wpu1 <- dom.wpu %>% 
  group_by(fips,year,ssp) %>%
  summarise(wpu = mean(wpu.dom*1000), pop = mean(pop*1000), domestic = mean(dom.t*1000000))

dp.wpu2 <- dom.wpu %>% 
  group_by(fips,year) %>%
  summarise(wpu = mean(wpu.dom*1000), pop = mean(pop*1000), domestic = mean(dom.t*1000000))

# testing consistency with Brenna Kent
df <- subset(demand, fips == 51107)
df <- df %>% 
  group_by(fips,year,ssp) %>%
  summarise(wpu = mean(wpu.dom*1000), pop = mean(pop*1000))


