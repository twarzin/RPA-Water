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

# # ------------------------ 
# # load baseline data
# 
# # Population and income projections are from Wear and Prestemon
# # Population by FIPS: fields are ID (x), fips, year, pop, ssp, inc; 860,440 records
# # Population is in thousands
# # Income is in __________
# pop.inc <- read.csv("1_BaseData/popinc_proj.csv")
# # projections of irrigated acreage for ag
# acre.data <- read.csv('1_BaseData/acredata-use.csv', header=TRUE)
# 
# # acre.data does not vary by ssp, so collapse to shorten merges
# acre.data <- subset(acre.data, ssp == "ssp1")
# acre.data <- acre.data %>%
#   select(fips, year, acres)
# # combine projection data
# proj.data <- merge(pop.inc, acre.data, by=c("fips", "year"))
# 
# proj.data <- proj.data %>%
#   select(fips, year, pop, ssp, inc, acres)
# 
# # Water withdrawals in 2015 from USGS (3,223 records):
# # Data fields:
# # DO.WDelv  Domestic, total use (withdrawals + deliveries), in Mgal/d
# # IN.WFrTo  Industrial, self-supplied total withdrawals, fresh, in Mgal/d
# # IR.WFrTo  Irrigation, total withdrawals, fresh, in Mgal/d
# # IR.CUsFr  Irrigation, total consumptive use, fresh, in Mgal/d
# # IR.IrTot  Irrigation, acres irrigated, total, in thousand acres
# # LI.WFrTo  Livestock, total withdrawals, fresh, in Mgal/d
# # AQ.WFrTo  Aquaculture, total withdrawals, fresh, in Mgal/d
# # MI.WFrTo  Mining, total withdrawals, fresh, in Mgal/d
# # PT.WFrTo  Thermoelectric, total withdrawals, fresh, in Mgal/d
# # PT.PSDel  Thermoelectric, deliveries from Public Supply, in Mgal/d
# # PT.CUsFr  Thermoelectric, total consumptive use, fresh, in Mgal/d
# # PT.Power  Thermoelectric, power generated, in gigawatt-hours
# 
# wd.2015 <- read.csv("1_BaseData/USGS2015.csv")
# wd.2015 <- wd.2015 %>%
#   select(FIPS, 
#          'DO.WDelv',
#          'IN.WFrTo',
#          'IR.WFrTo',
#          'IR.CUsFr',
#          'IR.IrTot',
#          'LI.WFrTo',
#          'AQ.WFrTo',
#          'MI.WFrTo',
#          'PT.WFrTo',
#          'PT.PSDel',
#          'PT.CUsFr',
#          'PT.Power')
# 
# # FIPS codes are off between USGS data and population projections. Will need to adjust 
# # population data later (outside of R)
# 
# # Create baseline withdrawals for surface water fresh for each sector
# # - first calculate total withdrawals for public supply, then calculate percent
# #   of those withdrawals from surface water. Some values read in as character 
# #   so need to convert to numeric
# wd.2015[] <- lapply(wd.2015, as.numeric)
# #   convert NAs to zeros
# wd.2015[is.na(wd.2015)] <- 0
# 
# # Creating variables for total industry withdrawals + deliveries
# wd.2015$dom <- wd.2015$DO.WDelv
# wd.2015$ind <- wd.2015$IN.WFrTo + wd.2015$MI.WFrTo 
# wd.2015$therm <- wd.2015$PT.WFrTo + wd.2015$PT.PSDel
# wd.2015$ag <- wd.2015$IR.WFrTo
# wd.2015$la <- wd.2015$LI.WFrTo + wd.2015$AQ.WFrTo
# 
# # baseline demand driver data from USGS 
# wd.2015$acres <- wd.2015$IR.IrTot
# wd.2015$power <- wd.2015$PT.Power
# 
# # rename fips to lowercase to match other data files
# wd.2015 <- rename(wd.2015, fips = FIPS)
# 
# # Growth and decay rates:
# # Growth and decay rates for withdrawals per unit are taken from Tom Brown's work.
# # This file also has a variable denoting whether the county in in the eastern or 
# # western United States
# growth <- read.csv("1_BaseData/WDGrowthCU.csv")
# 
# # First calculate withdrawals for each sector without climate impacts. Climate 
# # impacts are added in a separate section at the bottom of this code
# 
# #---------------------------------------------------------------------------------.
# ################# SECTOR PROJECTIONS TO 2070 - no climate #########################
# # This calculates projections out to 2070 using function from 
# # Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT
# 
# pop.inc.2015 <- subset(pop.inc, year == 2015)
# 
# # join population projections with base year withdrawals data
# demand.init1 <- merge(pop.inc.2015, wd.2015, by = "fips")
# 
# # select variables needed for calcuations:
# # Note, Pop is from withdrawal data. pop used here is from population projections
# keeps <- c("fips","year","ssp","inc","pop","dom","ag","ind","therm","la",
#            "acres", "power")
# demand.init <- demand.init1[,names(demand.init1) %in% keeps]
# 
# # calculate initial withdrawals per unit
# demand.init$wpu.dom <- demand.init$dom / demand.init$pop
# demand.init$wpu.ind <- demand.init$ind / demand.init$inc
# demand.init$wpu.ag <- demand.init$ag / demand.init$acres
# demand.init$wpu.therm <- demand.init$therm / demand.init$power
# # need to add thermo-electric, and aquaculture
# 
# # create dataframe for projections
# demand.proj <- subset(proj.data, year != 2015)
# 
# demand.proj$dom <- NA
# demand.proj$ind <- NA
# demand.proj$therm <- NA
# demand.proj$ag <- NA
# demand.proj$la <- NA
# demand.proj$acres <- NA
# demand.proj$power <- NA
# demand.proj$wpu.dom <- NA
# demand.proj$wpu.ind <- NA
# demand.proj$wpu.ag <- NA
# demand.proj$wpu.therm <- NA
# 
# demand <- rbind(demand.init, demand.proj)
# 
# demand <- merge(demand, growth, by="fips")
# 
# # order data so I can run a loop on lagged values
# attach(demand)
# demand <- demand[order(fips,ssp,year),]
# detach(demand)
# 
# # be sure to sort first!!
# # this loop takes hours on Travis' desktop
# 
# # after it is run once, save results then read in data from code below loop
# 
# # to run new data, un-comment the following
# # # ----------------------------------------
# # nobs <- dim(demand)[1]
# # for(i in 1:nobs) {
# #   if (demand$year[i] != 2015) {
# #     demand$wpu.dom[i] <- demand$wpu.dom[(i-1)] * (1+demand$DP.growth[i]*(1+demand$DP.decay[i])^(demand$year[i]-2015))
# #     demand$wpu.ind[i] <- demand$wpu.ind[(i-1)] * (1+demand$IC.growth[i]*(1+demand$IC.decay[i])^(demand$year[i]-2015))
# #     demand$wpu.ag[i]  <- demand$wpu.ag[(i-1)]  * (1+demand$IR.growth[i]*(1+demand$IR.decay[i])^(demand$year[i]-2015))
# #           }
# # }
# # 
# # # export the above results so we don't have to run them every time
# # write.csv(demand, file="demand-temp.csv")
#---------------------------------------

# assuming the above loop has run, read in demand-temp
demand <- read.csv(file="demand-temp.csv")

# calculate annual withdrawals for each sector
demand$dom.t <- demand$pop * demand$wpu.dom
demand$ind.t <- demand$inc * demand$wpu.ind
demand$ag.t  <- demand$acres * demand$wpu.ag


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

# --- domestic water use with climate change: 

cc.dp1 <- -1.415    # coefficient on change in summertime precip
cc.dp2 <- 0.778     # coefficient on change in pet

demand$wpu.dp.cc <- (cc.dp1*demand$delta.sprecip + cc.dp2*demand$delta.spet) / 1000
# the original code did not have the last term and divided by 1000
# domestic demand with climate change:
demand$dom.cc <- demand$wpu.dp.cc * demand$pop

# agricultural water use with climate change:

# from old code:
# 
# ir.test$wpuc.2015 <- ir.test$ir.wpu.2015 / ir.test$precip15
# ir.test$wpuc.2016 <- ir.test$ir.wpu.2016 / ir.test$precip16
# ir.test$wpuc.2017 <- ir.test$ir.wpu.2017 / ir.test$precip17
# ir.test$wpuc.2018 <- ir.test$ir.wpu.2018 / ir.test$precip18
# ir.test$wpuc.2019 <- ir.test$ir.wpu.2019 / ir.test$precip19



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


