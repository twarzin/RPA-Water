# Program file for RPA Water Demand Model
# Travis Warziniack
# US Forest Service
# travis.w.warziniack@usda.gov

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

# ------------------------ 
# load baseline data

# Population and income projections are from Wear and Prestemon
# Population by FIPS: fields are ID (x), fips, year, pop, ssp, inc; 860,440 records
# Population is in thousands
# Income is in __________
pop.inc <- read.csv("1_BaseData/popinc_proj.csv")

# Water withdrawals in 2015 from USGS (3,108 records):
# fields are x, fips, state, county, year, and 22 data fields
# Withdrawals are in MGD
wd.2015 <- read.csv("1_BaseData/usgswithdrawals.csv")
wd.2015 <- subset(wd.2015, year == 2015)
wd.2015 <- wd.2015 %>%
  select(FIPSRecoded, Pop, PSgw, PSsw, PSdom, PScom, PSind, PStherm, 
         DomSelfSW, ComSelfSW, IndSelfSW, ThermSelfSW, MinSelfSW, StockSelfSW, 
         AquaSelfSW, IrrigSelfSW, IrrigAcres, IrrigCU)

# FIPS codes are off between USGS data and population projections, hence the 
# FIPSRecoded field that has the updated FIPS codes. Rename variable to match 
# other data:

wd.2015 <- rename(wd.2015, fips = FIPSRecoded)

# Create baseline withdrawals for surface water fresh for each sector
# - first calculate total withdrawals for public supply, then calculate percent
#   of those withdrawals from surface water. Some values read in as character 
#   so need to convert to numeric
wd.2015[] <- lapply(wd.2015, as.numeric)
#   convert NAs to zeros
wd.2015[is.na(wd.2015)] <- 0

wd.2015$PStot <- wd.2015$PSgw + wd.2015$PSsw
wd.2015$swShare <- PSsw / PStot
# Sector formauls is (domestic for exmaple):
# domestic = public deliveries * (percent from surface) + self-supplied surface withdrawals
wd.2015$dom <- wd.2015$PSdom*wd.2015$swShare + wd.2015$DomSelfSW
wd.2015$com <- wd.2015$PScom*wd.2015$swShare + wd.2015$ComSelfSW
wd.2015$ind <- wd.2015$PSind*wd.2015$swShare + wd.2015$IndSelfSW + wd.2015$MinSelfSW
wd.2015$therm <- wd.2015$PStherm*wd.2015$swShare + wd.2015$ThermSelfSW
# no public supply deliveries to ag
wd.2015$ag <- wd.2015$IrrigSelfSW

# RPA combines commercial and industrial
wd.2015$ind <- wd.2015$ind + wd.2015$com

# Growth and decay rates:
# Growth and decay rates for withdrawals per unit are taken from Tom Brown's work.
# This file also has a variable denoting whether the county in in the eastern or 
# western United States
ew <- read.csv("1_BaseData/WDGrowthCU.csv")

# First calculate withdrawals for each sector without climate impacts. Climate 
# impacts are added in a separate section at the bottom of this code

#---------------------------------------------------------------------------------.
################# SECTOR PROJECTIONS TO 2070 - no climate #########################
# This calculates dp projections out to 2070 using function from 
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

demand.init <- subset(pop.inc, year == 2015)
# join population projections with base year withdrawals data
demand.init <- merge(demand.init, wd.2015, by = "fips")

# select variables needed for calcuations:
# Note, Pop is from withdrawal data. pop used here is from population projections
keeps <- c("fips","year","ssp","inc","pop","dom","ag","ind","therm",
           "IrrigAcres")
demand.init <- demand.init[,names(demand.init) %in% keeps]

# calculate initial withdrawals per unit
demand.init$wpu.dom <- demand.init$dom / demand.init$pop
demand.init$wpu.ind <- demand.init$ind / demand.init$inc
demand.init$wpu.ag <- demand.init$ag / demand.init$IrrigAcres

# need to add thermo-electric, and aquaculture

# create dataframe for projections
demand.proj <- subset(pop.inc, year != 2015)
keeps <- c("fips","year","pop","ssp","inc")
demand.proj <- demand.proj[,names(demand.proj) %in% keeps]

demand.proj$IrrigAcres <- NA
demand.proj$dom <- NA
demand.proj$ind <- NA
demand.proj$therm <- NA
demand.proj$ag <- NA
demand.proj$wpu.dom <- NA
demand.proj$wpu.ind <- NA
demand.proj$wpu.ag <- NA

demand <- rbind(demand.init, demand.proj)

demand <- merge(demand, ew, by="fips")

# order data so I can run a loop on lagged values
attach(demand)
demand <- demand[order(fips,ssp,year),]
detach(demand)

# be sure to sort first!!
# this loop takes a VERY long time to run, about 30 minutes on Travis' desktop
nobs <- dim(demand)[1]
for(i in 1:nobs) {
  if (demand$year[i] != 2015) {
    demand$wpu.dom[i] <- demand$wpu.dom[(i-1)] * (1+demand$DP.growth[i]*(1+demand$DP.decay[i])^(demand$year[i]-2015))
    demand$wpu.ind[i] <- demand$wpu.ind[(i-1)] * (1+demand$IC.growth[i]*(1+demand$IC.decay[i])^(demand$year[i]-2015))
    demand$wpu.ag[i]  <- demand$wpu.ag[(i-1)]  * (1+demand$IR.growth[i]*(1+demand$IR.decay[i])^(demand$year[i]-2015))
          }
}

# NEED TO READ IN Ag acres projections
demand$dom.t <- demand$pop * demand$wpu.dom
demand$ind.t <- demand$inc * demand$wpu.ind
demand$ag.t  <- demand$IrrigAcres * demand$wpu.ag



# -- PROJECTIONS WITH CLIMATE -------------

# Climate impacts the withdrawals needed per unit. Equations for climate effects
# can be found in:
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

# Domestic water use: Climate change only affects outdoor water use for domestic uses
# during the growing season (April - September). The first step is to divide  
# water use between outdoor and indoor use base on XXXXX.



# for testing, we'll create fake changes in growing season precipitation:
demand$delta.sprecip <- rnorm(1, mean=1, sd=1)
demand$delta.spet <- rnorm(1, mean=1, sd=1)


cc.dp1 <- -1.415    # coefficient on change in summertime precip
cc.dp2 <- 0.778     # coefficient on change in pet


# need to move this into loop to get values each year
demand$wpu.dp.cc <- (cc.dp1*demand$delta.sprecip + cc.dp2*demand$delta.spet) / 1000
# the original code did not have the last term and divided by 1000



# --- OUTPUT FORMAT ----

# Output for reports, etc. 
# extracting domestic estimates
dom.wpu <- demand
#subset(demand, ssp=="ssp1")

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


