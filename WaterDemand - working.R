# Program file for RPA Water Demand Model
# Travis Warziniack
# US Forest Service
# travis.w.warziniack@usda.gov

# to do:
# - Does industrial need to be total income or per capita income?
# - should ag climate be based on pet instead of precip?

rm(list = ls())  # clears memory

# Set working directory to file location
# for Leslie: 
setwd("/Users/leslie/Dropbox/RPA-Water")  
#E:/WaterDemand/WaterDemandProject/DataWaterDemand")
#setwd("D:/Demand model")
# for Travis:
setwd("C:/Users/twwarziniack/Documents/5_RPA/Demand model")

# Set working directory to same location of the R file location
# base.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(base.dir)

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
#####
  ##  1. Estimate baseline water use: 
  ##  Water Use   = withdrawal units * wpu = Unit * ϕ
  ##          ϕ   = (ϕnocc+ϕcc)(1-∂)
  ##                Mgal/day per unit with climate change

  ##  2. Estimate water use rate projected to 2070
  ##      ϕnocc   = ϕnocc,Y-5(1+growth(1+decay)^(Y-LDY))^5
  ##                Mgal/day per unit, without climate change            
  ##                This is projections to 2070

  ##  3. Estimate change in water use rate under CC scenarios, projected to 2070
  ##       ∆ϕcc   = ∆P'*nP+ ∆ET*nETp
  ##                Mgal/day per unit difference with climate change
  



                    ## 1. Estimate Baseline Water Use
#-------------------------------------------------------------------------------
# Read Data ------------------------
# load baseline data

# Population and income projections are from Wear and Prestemon
# Population by FIPS: fields are ID (x), fips, year, pop, ssp, inc; 860,440 records
# Population and income are projected through 2070
# Population is in thousands
# Income is in thousands ($)

pop.inc <- data.table::fread("1_BaseData/popinc_proj.csv") %>% as.data.frame()
# projections of irrigated acreage for ag
acre.data <- data.table::fread('1_BaseData/acredata-use.csv', header=TRUE) %>% as.data.frame()

# acre.data does not vary by SSP, so collapse to shorten merges
acre.data <- subset(acre.data, ssp == "ssp1")
acre.data <- acre.data %>%
  select(fips, year, acres)

# combine projection data
proj.data <- dplyr::inner_join(pop.inc, acre.data, by=c("fips", "year"))
    
## This is population, income, and acreage projected through 2070:
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

## This is baseline (i.e., 2015) water demand, by sector:
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
# population data later (outside of R) - (2023-02-06: not sure if this is still a problem?)

# Create baseline withdrawals for surface water fresh for each sector
# - first calculate total withdrawals for public supply, then calculate percent
#   of those withdrawals from surface water. Some values read in as character
#   so need to convert to numeric
wd.2015[] <- lapply(wd.2015, as.numeric)
#   convert NAs to zeros
wd.2015[is.na(wd.2015)] <- 0

# Creating variables for total sector withdrawals + deliveries (Mgal/day)
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




        ## 2. Estimate Sector Projections to 2070 - no climate change
#-------------------------------------------------------------------------------
##  Water Use   =   Units * ϕnocc
##  Domestic: 
##      ϕnocc   =   ϕnocc,Y-5*(1+growth(1+decay)^(Y-LDY))^5
## Agriculture:
##      ϕnocc   =   W_t / A_t
##        A_t   =   A_wrrt * (A_cnty,2015 / A_wrr,1995) + A^RFS

# This calculates projections out to 2070 using function from
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

# Growth and decay rates:
# Growth and decay rates for withdrawals per unit are taken from Tom Brown's work.
# This file also has a variable denoting whether the county in in the eastern or
# western United States
growth <- read.csv("1_BaseData/WDGrowthCU.csv")

# First calculate withdrawals for each sector without climate impacts. Climate
# impacts are added in a separate section at the bottom of this code

pop.inc.2015 <- subset(pop.inc, year == 2015)

# join population projections with base year withdrawals data
demand.init1 <- merge(pop.inc.2015, wd.2015, by = "fips")

# select variables needed for calculations:
# Note, Pop is from withdrawal data. pop used here is from population projections
keeps <- c("fips","year","ssp","inc","pop","dom","ag","ind","therm","la",
           "acres", "power")
demand.init <- demand.init1[,names(demand.init1) %in% keeps]

# calculate initial withdrawals per unit
  # wpu.dom = (Mgal per day / person)
  # wpu.inc = (Mgal per day / dollar)
  # wpu.ag  = (Mgal per day / acre)
  
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

# This is water demand, by sector, by SSR, projected to 2017
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
data.table::fwrite(demand2, file="DPwithdrawal_noCC.csv")

rm(demand,demand2)

# assuming the above loop has run, read in demand-temp
demand <- fread(file="withdrawal_noCC.csv") %>% as.data.frame()

# calculate annual withdrawals (Mgal/day = wpu * units) for each sector

demand <- demand %>%
  dplyr::mutate(dom.t = pop * wpu.dom,
                ind.t = inc * wpu.ind,
                ag.t  = acres * wpu.ag)

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

data.table::fwrite(demand.noCC, file="withdrawal_noCC.csv")



        ## 3. Estimate Sector Projections to 2070 - WITH CLIMATE CHANGE
#-------------------------------------------------------------------------------
##  ∆ϕcc = ∆P'*nP+ ∆ET*nETp

# Climate impacts the withdrawals needed per unit. Equations for climate effects
# can be found in:
# Foti, Ramirez, Brown (2010) FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT

# Domestic water use: Climate change only affects outdoor water use for domestic uses
# during the growing season (April - September). Pam Froemke has created 
# files for summer precip and et used here. That code is in the same GitHub 
# repository

# read in summer precip data; Excel file columns include summer precip data for all models
## precip is measured in mm

precip.data <- read.xlsx(
  xlsxFile="1_ClimateData/CountyPrecip/SummerPrecip/SummerPrecip.xlsx",
  sheet = 1,
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

demand <- merge(demand.noCC, precip.data, by=c('fips','year'))

# --- 3a. Domestic water use with climate change: 

# the following coefficients are taken from Tom Brown's work for the 2010 RPA Assessment
# the coefficients give the change in gallons per capita per day for a 1cm change in precip 
# and ET
cc.dp1 <- -1.415    # coefficient on change in summertime precip
cc.dp2 <- 0.778     # coefficient on change in pet

demand$delta.spet     <- 0
demand$ChangeSummerET <- demand$delta.spet


# convert precip data in mm height to cm height (per equation 11) 
demand$spChange.cn45.cm   <- demand$spChange_cn_45 * 0.1
demand$spChange.cn85.cm   <- demand$spChange_cn_85 * 0.1

demand$spChange.had45.cm  <- demand$spChange_had_45 * 0.1
demand$spChange.had85.cm  <- demand$spChange_had_85 * 0.1

demand$spChange.cgcm45.cm <- demand$spChange_cgcm_45 * 0.1
demand$spChange.cgcm85.cm <- demand$spChange_cgcm_85 * 0.1

demand$spChange.cm5a45.cm <- demand$spChange_cm5a_45 * 0.1
demand$spChange.cm5a85.cm <- demand$spChange_cm5a_85 * 0.1

demand$spChange.esm45.cm  <- demand$spChange_esm_45 * 0.1
demand$spChange.esm85.cm  <- demand$spChange_esm_85 * 0.1


# Multiply change in precip by n^p, which is a constant equal to -1.415
# This is in equation 11: ∆Φ = (ΦnoCC + ∆Φcc), where ∆Φcc=∆P'*n^p + ∆ETp*nET

# for each climate change scenario, for each SSP


demand$deltaP.n.cn45.cm   <- (cc.dp1*demand$spChange.cn45.cm + cc.dp2*demand$ChangeSummerET) 
demand$deltaP.n.cn85.cm   <- (cc.dp1*demand$spChange.cn85.cm + cc.dp2*demand$ChangeSummerET)

demand$deltaP.n.esm45.cm  <- (cc.dp1*demand$spChange.esm45.cm + cc.dp2*demand$ChangeSummerET) 
demand$deltaP.n.esm85.cm  <- (cc.dp1*demand$spChange.esm85.cm + cc.dp2*demand$ChangeSummerET)

demand$deltaP.n.cgcm45.cm <- (cc.dp1*demand$spChange.cgcm45.cm + cc.dp2*demand$ChangeSummerET) 
demand$deltaP.n.cgcm85.cm <- (cc.dp1*demand$spChange.cgcm85.cm + cc.dp2*demand$ChangeSummerET)

demand$deltaP.n.cm5a45.cm <- (cc.dp1*demand$spChange.cm5a45.cm + cc.dp2*demand$ChangeSummerET) 
demand$deltaP.n.cm5a85.cm <- (cc.dp1*demand$spChange.cm5a85.cm + cc.dp2*demand$ChangeSummerET)

demand$deltaP.n.had45.cm  <- (cc.dp1*demand$spChange.had45.cm + cc.dp2*demand$ChangeSummerET) 
demand$deltaP.n.had85.cm  <- (cc.dp1*demand$spChange.had85.cm + cc.dp2*demand$ChangeSummerET)

# Steps to convert cm precip to volume at the county level
## First, convert cm to meters
demand$deltaP.n.cn45.m    <- demand$deltaP.n.cn45.cm/100
demand$deltaP.n.cn85.m    <- demand$deltaP.n.cn85.cm/100

demand$deltaP.n.esm45.m   <- demand$deltaP.n.esm45.cm/100
demand$deltaP.n.esm85.m   <- demand$deltaP.n.esm85.cm/100

demand$deltaP.n.cgcm45.m  <- demand$deltaP.n.cgcm45.cm/100
demand$deltaP.n.cgcm85.m  <- demand$deltaP.n.cgcm85.cm/100

demand$deltaP.n.cm5a45.m  <- demand$deltaP.n.cm5a45.cm/100
demand$deltaP.n.cm5a85.m  <- demand$deltaP.n.cm5a85.cm/100

demand$deltaP.n.had45.m   <- demand$deltaP.n.had45.cm/100
demand$deltaP.n.had85.m   <- demand$deltaP.n.had85.cm/100

# Next, multiply precip meters by county area (m2) to get volume of rainfall
demand$deltaP.n.cn45.m3   <- demand$deltaP.n.cn45.m*demand$aland
demand$deltaP.n.cn85.m3   <- demand$deltaP.n.cn85.m*demand$aland

demand$deltaP.n.esm45.m3  <- demand$deltaP.n.esm45.m*demand$aland
demand$deltaP.n.esm85.m3  <- demand$deltaP.n.esm85.m*demand$aland

demand$deltaP.n.cgcm45.m3 <- demand$deltaP.n.cgcm45.m*demand$aland
demand$deltaP.n.cgcm85.m3 <- demand$deltaP.n.cgcm85.m*demand$aland

demand$deltaP.n.cm5a45.m3 <- demand$deltaP.n.cm5a45.m*demand$aland
demand$deltaP.n.cm5a85.m3 <- demand$deltaP.n.cm5a85.m*demand$aland

demand$deltaP.n.had45.m3  <- demand$deltaP.n.had45.m*demand$aland
demand$deltaP.n.had85.m3  <- demand$deltaP.n.had85.m*demand$aland

# # divide by number of days in growing season April - Sept (6*30)
# And convert to gallons (*264); can do this in one step
# This is ∆P'*nP in Mgal/day
demand$Dom.deltaP.n.cn45.Mgal   <- (demand$deltaP.n.cn45.m3/(6*30)*264)/1000000
demand$Dom.deltaP.n.cn85.Mgal   <- (demand$deltaP.n.cn85.m3/(6*30)*264)/1000000

demand$Dom.deltaP.n.esm45.Mgal  <- (demand$deltaP.n.esm45.m3/(6*30)*264)/1000000
demand$Dom.deltaP.n.esm85.Mgal  <- (demand$deltaP.n.esm85.m3/(6*30)*264)/1000000

demand$Dom.deltaP.n.had45.Mgal  <- (demand$deltaP.n.had45.m3/(6*30)*264)/1000000
demand$Dom.deltaP.n.had85.Mgal  <- (demand$deltaP.n.had85.m3/(6*30)*264)/1000000

demand$Dom.deltaP.n.cgcm45.Mgal <- (demand$deltaP.n.cgcm45.m3/(6*30)*264)/1000000
demand$Dom.deltaP.n.cgcm85.Mgal <- (demand$deltaP.n.cgcm85.m3/(6*30)*264)/1000000

demand$Dom.deltaP.n.cm5a45.Mgal <- (demand$deltaP.n.cm5a45.m3/(6*30)*264)/1000000
demand$Dom.deltaP.n.cm5a85.Mgal <- (demand$deltaP.n.cm5a85.m3/(6*30)*264)/1000000

                                         
# In the main equation, withdrawal = U * ϕ, this is is ϕ where ϕ=ϕnocc+∆ϕcc:

demand$wpu.dp.cc.cn45   <- demand$wpu.dom + (demand$Dom.deltaP.n.cn45.Mgal + cc.dp2*demand$ChangeSummerET) 
demand$wpu.dp.cc.cn85   <- demand$wpu.dom + (demand$Dom.deltaP.n.cn85.Mgal + cc.dp2*demand$ChangeSummerET)

demand$wpu.dp.cc.esm45  <- demand$wpu.dom + (demand$Dom.deltaP.n.esm45.Mgal + cc.dp2*demand$ChangeSummerET) 
demand$wpu.dp.cc.esm85  <- demand$wpu.dom + (demand$Dom.deltaP.n.esm85.Mgal + cc.dp2*demand$ChangeSummerET)

demand$wpu.dp.cc.cgcm45 <- demand$wpu.dom + (demand$Dom.deltaP.n.cgcm45.Mgal + cc.dp2*demand$ChangeSummerET) 
demand$wpu.dp.cc.cgcm85 <- demand$wpu.dom + (demand$Dom.deltaP.n.cgcm85.Mgal + cc.dp2*demand$ChangeSummerET)

demand$wpu.dp.cc.cm5a45 <- demand$wpu.dom + (demand$Dom.deltaP.n.cm5a45.Mgal + cc.dp2*demand$ChangeSummerET) 
demand$wpu.dp.cc.cm5a85 <- demand$wpu.dom + (demand$Dom.deltaP.n.cm5a85.Mgal + cc.dp2*demand$ChangeSummerET)

demand$wpu.dp.cc.had45  <- demand$wpu.dom + (demand$Dom.deltaP.n.had45.Mgal + cc.dp2*demand$ChangeSummerET) 
demand$wpu.dp.cc.had85  <- demand$wpu.dom + (demand$Dom.deltaP.n.had85.Mgal + cc.dp2*demand$ChangeSummerET)


# Multiply population projections by WPU to get total domestic withdrawals
# for each climate change projection 
# This is W = U*⏀
# U = population; ⏀=WPUcc

demand$W.dom.cc.cn45    <- demand$pop*demand$wpu.dp.cc.cn45
demand$W.dom.cc.cn85    <- demand$pop*demand$wpu.dp.cc.cn85

demand$W.dom.cc.esm45   <- demand$pop*demand$wpu.dp.cc.esm45
demand$W.dom.cc.esm85   <- demand$pop*demand$wpu.dp.cc.esm85

demand$W.dom.cc.cgcm45  <- demand$pop*demand$wpu.dp.cc.cgcm45
demand$W.dom.cc.cgcm85  <- demand$pop*demand$wpu.dp.cc.cgcm85

demand$W.dom.cc.cm5a45  <- demand$pop*demand$wpu.dp.cc.cm5a45
demand$W.dom.cc.cm5a85  <- demand$pop*demand$wpu.dp.cc.cm5a85

demand$W.dom.cc.had45   <- demand$pop*demand$wpu.dp.cc.had45
demand$W.dom.cc.had85   <- demand$pop*demand$wpu.dp.cc.had85


data.table::fwrite(demand, file="withdrawal_CC.csv")

# --- 3b. Agricultural Water Use with Climate Change: 

##     W  = A * ϕ * ∂
##     ϕ  = Irrigation withdrawal in feet of depth
##     ∂  = 893 gallons per acre-foot

##     ϕ  = (ϕnocc + ∆ϕcc)
## ϕnocc  = Withdrawals_t/Acres_t

# In this section, we estimate this:
##  ∆ϕcc  = ϑ (-∆P' + ˚ETp)/(γ)
###    ϑ  = 0.0328, number of feet per cm
###   ∆P' = Change in effective precipitation (cm)
### ∆ETp  = Change in potential ET during growing season
###    γ  = Consumptive use portion, which we'll ignore for now

# ϑ (- ∆P' + ETp)
cc.ag1 <- 0.0328  # number of feet per cm
cc.ag2 <- 893  # gallon-days per acre-foot

# This is ∆wpu_cc in feet (-∆P' + ˚ETp)
demand$wpu.Delta.ag.cc.cn45.ft    <- cc.ag1*(-1*demand$deltaP.n.cn45.cm + demand$delta.spet)
demand$wpu.Delta.ag.cc.cn85.ft    <- cc.ag1*(-1*demand$deltaP.n.cn85.cm + demand$delta.spet)

demand$wpu.Delta.ag.cc.esm45.ft   <- cc.ag1*(-1*demand$deltaP.n.esm45.cm + demand$delta.spet)
demand$wpu.Delta.ag.cc.esm85.ft   <- cc.ag1*(-1*demand$deltaP.n.esm85.cm + demand$delta.spet)

demand$wpu.Delta.ag.cc.cgcm45.ft  <- cc.ag1*(-1*demand$deltaP.n.cgcm45.cm + demand$delta.spet)
demand$wpu.Delta.ag.cc.cgcm85.ft  <- cc.ag1*(-1*demand$deltaP.n.cgcm85.cm + demand$delta.spet)

demand$wpu.Delta.ag.cc.cm5a45.ft  <- cc.ag1*(-1*demand$deltaP.n.cm5a45.cm + demand$delta.spet)
demand$wpu.Delta.ag.cc.cm5a85.ft  <- cc.ag1*(-1*demand$deltaP.n.cm5a85.cm + demand$delta.spet)

demand$wpu.Delta.ag.cc.had45.ft   <- cc.ag1*(-1*demand$deltaP.n.had45.cm + demand$delta.spet)
demand$wpu.Delta.ag.cc.had85.ft   <- cc.ag1*(-1*demand$deltaP.n.had85.cm + demand$delta.spet)


#   Divide by irrigated acres to get AF
##  Irrigated acres, acres, are in 1000s, so have to multiply by 1000
##  This is ∆wpu_cc in Acre-Feet (-∆P' + ˚ETp)

################################### Question: divide by irrigated acres or by county acreage?
#----------
demand$wpu.Delta.ag.cc.cn45.AF   <-  demand$wpu.Delta.ag.cc.cn45.ft/(demand$acres*1000)
demand$wpu.Delta.ag.cc.cn85.AF   <-  demand$wpu.Delta.ag.cc.cn85.ft/(demand$acres*1000)

demand$wpu.Delta.ag.cc.esm45.AF  <-  demand$wpu.Delta.ag.cc.esm45.ft/(demand$acres*1000)
demand$wpu.Delta.ag.cc.esm85.AF  <-  demand$wpu.Delta.ag.cc.esm85.ft/(demand$acres*1000)

demand$wpu.Delta.ag.cc.cgcm45.AF <-  demand$wpu.Delta.ag.cc.cgcm45.ft/(demand$acres*1000)
demand$wpu.Delta.ag.cc.cgcm85.AF <-  demand$wpu.Delta.ag.cc.cgcm85.ft/(demand$acres*1000)

demand$wpu.Delta.ag.cc.cm5a45.AF <-  demand$wpu.Delta.ag.cc.cm5a45.ft/(demand$acres*1000)
demand$wpu.Delta.ag.cc.cm5a85.AF <-  demand$wpu.Delta.ag.cc.cm5a85.ft/(demand$acres*1000)

demand$wpu.Delta.ag.cc.had45.AF  <-  demand$wpu.Delta.ag.cc.had45.ft/(demand$acres*1000)
demand$wpu.Delta.ag.cc.had85.AF  <-  demand$wpu.Delta.ag.cc.had85.ft/(demand$acres*1000)



#################### Question: what unit is wpu_nocc? If it's already in Mgal/day, then will add it after
# converting AF units on ∆wpu_cc to Mgal/day

## This is  ∆ϕcc converted from AF to Mgal/day

demand$wpu.Delta.ag.cc.cn45.Mgal <- demand$wpu.Delta.ag.cc.cn45.AF*cc.ag2
demand$wpu.Delta.ag.cc.cn85.Mgal <- demand$wpu.Delta.ag.cc.cn85.AF*cc.ag2

demand$wpu.Delta.ag.cc.cgcm45.Mgal <- demand$wpu.Delta.ag.cc.cgcm45.AF*cc.ag2
demand$wpu.Delta.ag.cc.cgcm85.Mgal <- demand$wpu.Delta.ag.cc.cgcm85.AF*cc.ag2

demand$wpu.Delta.ag.cc.esm45.Mgal <- demand$wpu.Delta.ag.cc.esm45.AF*cc.ag2
demand$wpu.Delta.ag.cc.esm85.Mgal <- demand$wpu.Delta.ag.cc.esm85.AF*cc.ag2

demand$wpu.Delta.ag.cc.cm5a45.Mgal <- demand$wpu.Delta.ag.cc.cm5a45.AF*cc.ag2
demand$wpu.Delta.ag.cc.cm5a85.Mgal <- demand$wpu.Delta.ag.cc.cm5a85.AF*cc.ag2

demand$wpu.Delta.ag.cc.had45.Mgal <- demand$wpu.Delta.ag.cc.had45.AF*cc.ag2
demand$wpu.Delta.ag.cc.had85.Mgal <- demand$wpu.Delta.ag.cc.had85.AF*cc.ag2


## This is  ϕ  = (ϕnocc + ∆ϕcc):
demand$wpu.ag.cc.cn45   <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cn45.Mgal
demand$wpu.ag.cc.cn85   <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cn85.Mgal

demand$wpu.ag.cc.esm45  <- demand$wpu.ag + demand$wpu.Delta.ag.cc.esm45.Mgal
demand$wpu.ag.cc.esm85  <- demand$wpu.ag + demand$wpu.Delta.ag.cc.esm85.Mgal

demand$wpu.ag.cc.cgcm45 <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cgcm45.Mgal
demand$wpu.ag.cc.cgcm85 <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cgcm85.Mgal

demand$wpu.ag.cc.cm5a45 <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cm5a45.Mgal
demand$wpu.ag.cc.cm5a85 <- demand$wpu.ag + demand$wpu.Delta.ag.cc.cm5a85.Mgal

demand$wpu.ag.cc.had45  <- demand$wpu.ag + demand$wpu.Delta.ag.cc.had45.Mgal
demand$wpu.ag.cc.had85  <- demand$wpu.ag + demand$wpu.Delta.ag.cc.had85.Mgal


## Finally, W  = A * ϕ * ∂

demand$W.ag.cc.cn45   <-  demand$acres * demand$wpu.ag.cc.cn45 * cc.ag2
demand$W.ag.cc.cn85   <-  demand$acres * demand$wpu.ag.cc.cn85 * cc.ag2

demand$W.ag.cc.esm45  <-  demand$acres * demand$wpu.ag.cc.esm45 * cc.ag2
demand$W.ag.cc.esm85  <-  demand$acres * demand$wpu.ag.cc.esm85 * cc.ag2

demand$W.ag.cc.cgcm45 <-  demand$acres * demand$wpu.ag.cc.cgcm45 * cc.ag2
demand$W.ag.cc.cgcm85 <-  demand$acres * demand$wpu.ag.cc.cgcm85 * cc.ag2 

demand$W.ag.cc.cm5a45 <-  demand$acres * demand$wpu.ag.cc.cm5a45 * cc.ag2
demand$W.ag.cc.cm5a85 <-  demand$acres * demand$wpu.ag.cc.cm5a85 * cc.ag2

demand$W.ag.cc.had45  <-  demand$acres * demand$wpu.ag.cc.had45 * cc.ag2
demand$W.ag.cc.had85  <-  demand$acres * demand$wpu.ag.cc.had85 * cc.ag2
  
write.csv(demand, file="demand-final.csv")


# Multiply wpu by percentage change in summer precip to get 
# demands with climate impacts
# demand$wpu.ag.cc <- demand$wpu.ag * (1/demand$delta.sprecip)
# demand$ag.cc <- demand$wpu.ag.cc * demand$acres

# comparing domestic demand with and without climate change
# keeps <- c("fips","year","ssp","GCM","RCP","dom.t","dom.cc","ag.t","ag.cc")
# dcheck <- demand
# dcheck <- dcheck[,names(dcheck) %in% keeps]
# dcheck$compare.dom <- dcheck$dom.cc / dcheck$dom.t
# dcheck$compare.ag <- dcheck$ag.cc / dcheck$ag.t




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


