rm(list = ls())

# set working directory to file location
# for Travis:
setwd("~/5_RPA/2020 Assessment/Demand model/WEAP Input Creation")
# for Shaunie
#setwd("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation")

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)

################## READ IN DATA ###############

pop.inc <- read.csv("popinc_proj.csv")
wd.2015 <- read.csv("wd2015.csv")
# cu.ratios <- read.csv("consumptive use.csv")

#choosing variables that will be common throughout years. Handy for merging later
ew <- wd.2015 %>% select(fips, EastWest, DP.growth, DP.decay)

# water <- merge(wd.2015, cu.ratios, by="fips")
# wd.2015$PD <- wd.2015$Public + wd.2015$Domestic

# pop1 <- read.csv("pop_ssp1.csv")
# pop2 <- read.csv("pop_ssp2.csv")
# pop3 <- read.csv("pop_ssp3.csv")
# pop4 <- read.csv("pop_ssp4.csv")
# pop5 <- read.csv("pop_ssp5.csv")
# 
# water1 <- merge(water, pop1, by="fips")
# water2 <- merge(water, pop2, by="fips")
# water3 <- merge(water, pop3, by="fips")
# water4 <- merge(water, pop4, by="fips")
# water5 <- merge(water, pop5, by="fips")

############################## SET MODELS ##########################################
#IMPORTANT: need to set the carbon assumption and global climate model
#-----------------------------------------------------------------------------------.
#carbon=45 or carbon=85 depending on which carbon climate model
carbon <- 85

#set the global climate model. options: "cnrm_c5", "hadgem","ipsl_cm5a","mri_cgcm3","noresm", "base"
gcm <- "noresm"

# #this is messy, but brings in the corresponding climate data based on setting the model above
# if(carbon==45 & gcm=="cnrm_c5"){
# precip <-read.csv("ClimateData/p_cnrm_c5_45.csv", check.names=F)
# pet <-read.csv("ClimateData/pet_cnrm_c5_45.csv", check.names=F)
# }
# if(carbon==85 & gcm=="cnrm_c5"){
#   precip <-read.csv("ClimateData/p_cnrm_c5_85.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_cnrm_c5_85.csv", check.names=F)
# }
# if(carbon==45 & gcm=="hadgem"){
#   precip <-read.csv("ClimateData/p_hadgem_45.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_hadgem_45.csv", check.names=F)
# }
# if(carbon==85 & gcm=="hadgem"){
#   precip <-read.csv("ClimateData/p_hadgem_85.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_hadgem_85.csv", check.names=F)
# }
# if(carbon==45 & gcm=="ipsl_cm5a"){
#   precip <-read.csv("ClimateData/p_ipsl_cm5a_45.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_ipsl_cm5a_45.csv", check.names=F)
# }
# if(carbon==85 & gcm=="ipsl_cm5a"){
#   precip <-read.csv("ClimateData/p_ipsl_cm5a_85.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_ipsl_cm5a_85.csv", check.names=F)
# }
# if(carbon==45 & gcm=="mri_cgcm3"){
#   precip <-read.csv("ClimateData/p_mri_cgcm3_45.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_mri_cgcm3_45.csv", check.names=F)
# }
# if(carbon==85 & gcm=="mri_cgcm3"){
#   precip <-read.csv("ClimateData/p_mri_cgcm3_85.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_mri_cgcm3_85.csv", check.names=F)
# }
# if(carbon==45 & gcm=="noresm"){
#   precip <-read.csv("ClimateData/p_noresm_45.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_noresm_45.csv", check.names=F)
# }
# if(carbon==85 & gcm=="noresm"){
#   precip <-read.csv("ClimateData/p_noresm_85.csv", check.names=F)
#   pet <-read.csv("ClimateData/pet_noresm_85.csv", check.names=F)
# }
# #if(gcm=="base"){
# #  precip <-read.csv("ClimateData/baseline.csv", check.names=F)
# #  pet <-read.csv("ClimateData/baselinepet.csv", check.names=F)
# #}


### read in new precip data

if(carbon==45 & gcm=="cnrm_c5"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_CNRM_CM5rcp45_month.csv", check.names=F)
}
if(carbon==85 & gcm=="cnrm_c5"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_CNRM_CM5rcp85_month.csv", check.names=F)
}

if(carbon==45 & gcm=="hadgem"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_HadGEM2_ES365rcp45_month.csv", check.names=F)
}
if(carbon==85 & gcm=="hadgem"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_HadGEM2_ES365rcp85_month.csv", check.names=F)
}

if(carbon==45 & gcm=="ipsl_cm5a"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_IPSL_CM5A_MRrcp45_month.csv", check.names=F)
}
if(carbon==85 & gcm=="ipsl_cm5a"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_IPSL_CM5A_MRrcp85_month.csv", check.names=F)
}

if(carbon==45 & gcm=="mri_cgcm3"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_MRI_CGCM3rcp45_month.csv", check.names=F)
}
if(carbon==85 & gcm=="mri_cgcm3"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_MRI_CGCM3rcp85_month.csv", check.names=F)
}

if(carbon==45 & gcm=="noresm"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_NorESM1_Mrcp45_month.csv", check.names=F)
}
if(carbon==85 & gcm=="noresm"){
  new.precip <-read.csv("CountyPrecip/Monthly/pr_NorESM1_Mrcp85_month.csv", check.names=F)
}


# We should call summer precip growing season precip
new.precip <- new.precip[,-1]
summer.precip <- subset(new.precip, Month >= 4 & Month <= 9) %>% group_by(Year) %>% summarise_all(sum)
summer.precip <- summer.precip[,-2]

# transpose

summer.t <- t(summer.precip)
summer.t <- as.data.frame(summer.t)
colnames(summer.t) = summer.t[1, ]
summer.t = summer.t[-1, ] 
summer.precip <- summer.t

# there has to be a better way to do this, perhaps outside of R
# then import summer precip as a data file
sy15 <- summer.precip %>% select('2015') 
colnames(sy15) = 'summer.precip'
sy15$fips <- rownames(sy15)
sy15$year <- 2015

base <- sy15 %>% select(fips, summer.precip)  # base year precip to calc changes
colnames(base) = c("fips", "s.precp0")

sy16 <- summer.precip %>% select('2016') 
colnames(sy16) = 'summer.precip'
sy16$fips <- rownames(sy16)
sy16$year <- 2016

sp.all <- rbind(sy15, sy16)
sp.all <- merge(sp.all, base, by="fips")
sp.all$delta.sprecip <- sp.all$summer.precip - sp.all$s.precp0
  
##### Pam stops here ################################################################

#takes output from WaterUsedatacleanup.R, population and income projections and creates input for WEAP
#WaterUsedatacleanup.R takes USGS water data and converts it into a usable format. See that file for additional assumptions in our modeling.

precip_orig <- precip
pet_orig <- pet

# FIPs fixes should be done to the base data and not in this code
############################# FUNCTIONS TO FIX FIPS CODES ###########################
# FIPS fixes are guided by document "US county population and income projections by SSP(Final_7_25_19)"
# this file is located in box - 2020 RPA Assessment > Base Data

#this function fixes FIPS differences

fipsfix <- function (inc) 
{
inc[,c(1)] <- ifelse(inc[,c(1)] == 51003, 51901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51540, 51901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51015, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51790, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51820, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51031, 51911, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51680, 51911, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51035, 51913, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51640, 51913, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51053, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51570, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51730, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51059, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51600, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51610, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51069, 51921, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51840, 51921, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51081, 51923, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51595, 51923, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51089, 51929, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51690, 51929, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51095, 51931, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51830, 51931, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51121, 51933, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51750, 51933, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51939, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51590, 51939, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51670, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51153, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51683, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51685, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51161, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51775, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51163, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51530, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51165, 51947, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51660, 51947, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51175, 51949, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51620, 51949, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51177, 51951, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51630, 51951, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51191, 51953, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51520, 51953, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51195, 51955, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51720, 51955, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51199, 51958, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51735, 51958, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51560, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51005, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51580, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51780, 51083, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51515, 51909, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51019, 51909, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 12025, 12086, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 46102, 46113, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55078, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55115, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 08014, 08013, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 04012, 04027, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 35006, 35061, inc[,c(1)])
inc <- inc %>% group_by(fips) %>% summarise_all(funs(sum))
}

#fix fips codes for energy regions
fipsfixenergy <- function (inc) 
{
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51003, 51901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51540, 51901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51015, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51790, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51820, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51031, 51911, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51680, 51911, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51035, 51913, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51640, 51913, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51053, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51570, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51730, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51059, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51600, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51610, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51069, 51921, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51840, 51921, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51081, 51923, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51595, 51923, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51089, 51929, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51690, 51929, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51095, 51931, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51830, 51931, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51121, 51933, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51750, 51933, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51939, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51590, 51939, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51670, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51153, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51683, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51685, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51161, 51944, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51775, 51944, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51163, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51530, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51165, 51947, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51660, 51947, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51175, 51949, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51620, 51949, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51177, 51951, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51630, 51951, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51191, 51953, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51520, 51953, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51195, 51955, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51720, 51955, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51199, 51958, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51735, 51958, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51560, 51903, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51005, 51903, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51580, 51903, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51780, 51083, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51019, 51909, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51515, 51909, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 12025, 12086, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 46102, 46113, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 55078, 55901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 55115, 55901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 08014, 08013, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 04012, 04027, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 35006, 35061, inc[,c(1)])
  inc <- inc %>% group_by(fips) %>% summarise_all(funs(sum,mean))
}

#----------------------------------------------------------------------------------------------------.

##### FIX FIPS CODES FOR COUNTY WATERSHED PCT #####

cntypercent<-read.csv("CountyWatershedPct.csv")

cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51003, 51901, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51540, 51901, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51015, 51907, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51790, 51907, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51820, 51907, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51031, 51911, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51680, 51911, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51035, 51913, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51640, 51913, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51053, 51918, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51570, 51918, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51730, 51918, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51059, 51919, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51600, 51919, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51610, 51919, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51069, 51921, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51840, 51921, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51081, 51923, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51595, 51923, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51089, 51929, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51690, 51929, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51095, 51931, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51830, 51931, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51121, 51933, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51750, 51933, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51143, 51939, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51590, 51939, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51149, 51941, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51670, 51941, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51153, 51942, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51683, 51942, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51685, 51942, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51161, 51944, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51775, 51944, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51163, 51945, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51530, 51945, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51678, 51945, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51165, 51947, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51660, 51947, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51175, 51949, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51620, 51949, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51177, 51951, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51630, 51951, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51191, 51953, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51520, 51953, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51195, 51955, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51720, 51955, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51199, 51958, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51735, 51958, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51560, 51903, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51005, 51903, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51580, 51903, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51780, 51083, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51019, 51909, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51515, 51909, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51149, 51941, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51678, 51945, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 12025, 12086, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 46102, 46113, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 55078, 55901, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 55115, 55901, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 08014, 08013, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 04012, 04027, cntypercent[,c(1)])
cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 35006, 35061, cntypercent[,c(1)])
cntypercent[,c(5)] <- 0

cntypercent <- cntypercent %>% group_by(FIPS, HUC_10) %>% summarise(Shape_Area=sum(Shape_Area), CountyArea=sum(CountyArea), AreaHuc10=mean(AreaHuc10), CountyAreaPct=sum(CountyAreaPct), HU_10_NAME=sum(HU_10_NAME))
cntypercent <- cntypercent[,c(1,4,3,2,7,5,6)]
test.pct <- cntypercent %>% group_by(FIPS) %>% summarise(Shape_Area=sum(Shape_Area), CountyArea=mean(CountyArea), AreaHuc10=mean(AreaHuc10), CountyAreaPct=sum(CountyAreaPct), HU_10_NAME=sum(HU_10_NAME))

# cntypercent$CountyAreaPct <- cntypercent$Shape_Area/cntypercent$CountyArea 
#----------------------------------------------------------------------------------------------------.

######################## FUNCTION FOR LINEAR APPROXIMATIONS #######################
#This function is used to create the linear approximations for years in between the 5 year intervals.
linapprox <- function (DF)
{
  DF$Y2016 <- DF$Y2015 + 1*(DF$Y2020 - DF$Y2015)/5
  DF$Y2017 <- DF$Y2015 + 2*(DF$Y2020 - DF$Y2015)/5
  DF$Y2018 <- DF$Y2015 + 3*(DF$Y2020 - DF$Y2015)/5
  DF$Y2019 <- DF$Y2015 + 4*(DF$Y2020 - DF$Y2015)/5
  
  DF$Y2021 <- DF$Y2020 + 1*(DF$Y2025 - DF$Y2020)/5
  DF$Y2022 <- DF$Y2020 + 2*(DF$Y2025 - DF$Y2020)/5
  DF$Y2023 <- DF$Y2020 + 3*(DF$Y2025 - DF$Y2020)/5
  DF$Y2024 <- DF$Y2020 + 4*(DF$Y2025 - DF$Y2020)/5
  
  DF$Y2026 <- DF$Y2025 + 1*(DF$Y2030 - DF$Y2025)/5
  DF$Y2027 <- DF$Y2025 + 2*(DF$Y2030 - DF$Y2025)/5
  DF$Y2028 <- DF$Y2025 + 3*(DF$Y2030 - DF$Y2025)/5
  DF$Y2029 <- DF$Y2025 + 4*(DF$Y2030 - DF$Y2025)/5
  
  DF$Y2031 <- DF$Y2030 + 1*(DF$Y2035 - DF$Y2030)/5
  DF$Y2032 <- DF$Y2030 + 2*(DF$Y2035 - DF$Y2030)/5
  DF$Y2033 <- DF$Y2030 + 3*(DF$Y2035 - DF$Y2030)/5
  DF$Y2034 <- DF$Y2030 + 4*(DF$Y2035 - DF$Y2030)/5
  
  DF$Y2036 <- DF$Y2035 + 1*(DF$Y2040 - DF$Y2035)/5
  DF$Y2037 <- DF$Y2035 + 2*(DF$Y2040 - DF$Y2035)/5
  DF$Y2038 <- DF$Y2035 + 3*(DF$Y2040 - DF$Y2035)/5
  DF$Y2039 <- DF$Y2035 + 4*(DF$Y2040 - DF$Y2035)/5
  
  DF$Y2041 <- DF$Y2040 + 1*(DF$Y2045 - DF$Y2040)/5
  DF$Y2042 <- DF$Y2040 + 2*(DF$Y2045 - DF$Y2040)/5
  DF$Y2043 <- DF$Y2040 + 3*(DF$Y2045 - DF$Y2040)/5
  DF$Y2044 <- DF$Y2040 + 4*(DF$Y2045 - DF$Y2040)/5
  
  DF$Y2046 <- DF$Y2045 + 1*(DF$Y2050 - DF$Y2045)/5
  DF$Y2047 <- DF$Y2045 + 2*(DF$Y2050 - DF$Y2045)/5
  DF$Y2048 <- DF$Y2045 + 3*(DF$Y2050 - DF$Y2045)/5
  DF$Y2049 <- DF$Y2045 + 4*(DF$Y2050 - DF$Y2045)/5
  
  DF$Y2051 <- DF$Y2050 + 1*(DF$Y2055 - DF$Y2050)/5
  DF$Y2052 <- DF$Y2050 + 2*(DF$Y2055 - DF$Y2050)/5
  DF$Y2053 <- DF$Y2050 + 3*(DF$Y2055 - DF$Y2050)/5
  DF$Y2054 <- DF$Y2050 + 4*(DF$Y2055 - DF$Y2050)/5
  
  DF$Y2056 <- DF$Y2055 + 1*(DF$Y2060 - DF$Y2055)/5
  DF$Y2057 <- DF$Y2055 + 2*(DF$Y2060 - DF$Y2055)/5
  DF$Y2058 <- DF$Y2055 + 3*(DF$Y2060 - DF$Y2055)/5
  DF$Y2059 <- DF$Y2055 + 4*(DF$Y2060 - DF$Y2055)/5
  
  DF$Y2061 <- DF$Y2060 + 1*(DF$Y2065 - DF$Y2060)/5
  DF$Y2062 <- DF$Y2060 + 2*(DF$Y2065 - DF$Y2060)/5
  DF$Y2063 <- DF$Y2060 + 3*(DF$Y2065 - DF$Y2060)/5
  DF$Y2064 <- DF$Y2060 + 4*(DF$Y2065 - DF$Y2060)/5
  
  DF$Y2066 <- DF$Y2065 + 1*(DF$Y2070 - DF$Y2065)/5
  DF$Y2067 <- DF$Y2065 + 2*(DF$Y2070 - DF$Y2065)/5
  DF$Y2068 <- DF$Y2065 + 3*(DF$Y2070 - DF$Y2065)/5
  DF$Y2069 <- DF$Y2065 + 4*(DF$Y2070 - DF$Y2065)/5
  
  DF <- DF[,c(1,2,3,15:18,4,19:22,5,23:26,6,27:30,7,31:34,8,35:38,9,39:42,10,43:46,11,47:50,12,51:54,13,55:58,14)]
}

#----------------------------------------------------------------------------------------------------.
###### CLIMATE EFFECTS ######

# The following section brings in climate data that will be used to supplement projections to 2070. These include precipitation data, evapotranspiration data,
# and population data. Also plantwater data.
#-----------------------------------------.
#Definitions from function used in Tom's paper
#plantwater is "a factor for the change due to the direct effect of rising atmospheric CO2 levels on plant water use"
#precip is "change in effective precipitation, the portion of total growing season precipitation that is useable by the plant, from 2005 to the future year, in cm/y, computed at the basin scale"
#mup is "the change in DP gallons per capita per day withdrawn for a 1 cm increase in P'  from 2005 to a future year, a constant equal to ???1.415"
#et is "the change potential evapotranspiration during the growing season, from 2005 to the future year, in cm/y, computed at the basin scale"
#muet is "the change in DP gallons per capita per day withdrawn for a 1 cm increase in ETp from 2005 to a future year, a constant equal to 0.778"

#-----------------------------------------.
#Bring in CO2 estimates, outdoor water use proportion from Tom
#

inputs<- read.csv("inputs_ssp1.csv")
inputs$Region <- "World"
inputs <- inputs[,c(2,28)]
co2.45 <-read.csv("R45_bulk.csv")
co2.85 <-read.csv("R85_bulk.csv")

co2.45 <- co2.45[co2.45$Region=="World",]
co2.45 <- co2.45[co2.45$Variable=="Concentration - CO2",]
co2.45 <- co2.45[,-c(2:6,17)]
climate.45 <- merge(inputs, co2.45, by="Region")
climate.45 <- climate.45[,-1]

co2.85 <- co2.85[co2.85$Region=="World",]
co2.85 <- co2.85[co2.85$Variable=="Concentration - CO2",]
co2.85 <- co2.85[,-c(2:6,17)]
climate.85 <- merge(inputs, co2.85, by="Region")
climate.85 <- climate.85[,-1]

#linear approximations of annual rather than 10 year intervals
fixclimate <- function(df){
  for (i in 1:6){
    for (n in 1:10){
      k <- (i-1)*10 +11 +n
      j <- 1+i
      m <- j + 1
      df[,k] <- df[,j]+(n*(df[,m]-df[,j])/10)
    }
  }
  df <- df[,c(1,16:71)]
  colnames(df) <- c("fips","plantwater15","plantwater16","plantwater17","plantwater18","plantwater19","plantwater20","plantwater21","plantwater22","plantwater23","plantwater24","plantwater25","plantwater26","plantwater27","plantwater28","plantwater29","plantwater30","plantwater31","plantwater32","plantwater33","plantwater34","plantwater35","plantwater36","plantwater37","plantwater38","plantwater39","plantwater40","plantwater41","plantwater42","plantwater43","plantwater44","plantwater45","plantwater46","plantwater47","plantwater48","plantwater49","plantwater50","plantwater51","plantwater52","plantwater53","plantwater54","plantwater55","plantwater56","plantwater57","plantwater58","plantwater59","plantwater60","plantwater61","plantwater62","plantwater63","plantwater64","plantwater65","plantwater66","plantwater67","plantwater68","plantwater69","plantwater70")
  for (i in 3:57){
    df[i] <- (df[,i]-df[,2])/1900
  }
  df[,2] <- 0
  return(df)
}
climate.45 <- fixclimate(climate.45)
climate.85 <- fixclimate(climate.85)

#climate.45 and climate.85 refer to RCP 45/85. This replaces the "plantwater" placeholder in the climate data frame below
#Note that each climate scenario from Hadi is run for both of these RCP scenarios.


#----------------------------------------------------------------------------------------------------.


##### CALCULATING OUTDOOR WATER USE PROPORTION #####

outdoor <- cntypercent
outdoor$HUC_10 <- floor(outdoor$HUC_10/100000000)
outdoor <- outdoor %>% group_by(FIPS, HUC_10) %>% summarise(Shape_Area=sum(Shape_Area), CountyArea=mean(CountyArea), AreaHuc10=mean(AreaHuc10), CountyAreaPct=sum(CountyAreaPct), HU_10_NAME=sum(HU_10_NAME))
outdoor$CountyAreaPct <- outdoor$Shape_Area/outdoor$CountyArea
norm <- outdoor %>% group_by(FIPS) %>% summarise_all(sum)
norm <- norm[,c(1,3)]
outdoor <- merge(outdoor, norm, by="FIPS")
outdoor$Shape_Area.y <- outdoor$Shape_Area.y/outdoor$CountyArea
outdoor$CountyAreaPct <- outdoor$CountyAreaPct/outdoor$Shape_Area.y
outdoor <- outdoor[,c(1,2,6)]
outdoor$outprop <- ifelse(outdoor$HUC_10==1, 0.07,ifelse(outdoor$HUC_10==2, 0.03,ifelse(outdoor$HUC_10==3, .12,ifelse(outdoor$HUC_10==4, .1, ifelse(outdoor$HUC_10==5, .08,ifelse(outdoor$HUC_10==6, .08,ifelse(outdoor$HUC_10==7, .17,ifelse(outdoor$HUC_10==8, .14,ifelse(outdoor$HUC_10==9, .14,ifelse(outdoor$HUC_10==10, .33,ifelse(outdoor$HUC_10==11, .28,ifelse(outdoor$HUC_10==12, .21,ifelse(outdoor$HUC_10==13, .26,ifelse(outdoor$HUC_10==14, .44,ifelse(outdoor$HUC_10==15, .32,ifelse(outdoor$HUC_10==16, .48,ifelse(outdoor$HUC_10==17, .34,0.44 ) ) ) ) ) ) ) ) ) ) ) ) )) ) ) )

outdoor$outprop <- outdoor$outprop * outdoor$CountyAreaPct
outdoor <- outdoor[,c(1,4)] %>% group_by(FIPS) %>% summarise_all(sum)

check <- as.list(outdoor$FIPS)
check3 <- as.list(inputs$fips)
#climate data has but county does not
check1 <- check3[! check3 %in% check]
#county data has but climate does not
check2 <- check[!  check %in% check3]

outdoor <- outdoor[!outdoor$FIPS %in% check2,]
colnames(outdoor) <- c("fips","outprop")

#----------------------------------------------------------------------------------------------------.
##### EFFECTIVE PRECIPITATION #####

#functions for effective precipitation
#converts to effective precipitation based on the function in Tom's paper. Note that input data is mm/month and the output needs to be in cm/month, hence the dividing by 10.

effectiveprecip <- function(df){
  for (i in 2:length(colnames(df))){
    for (j in 1:length(rownames(df))){
      df[j,i] <- df[j,i] /10
      df[j,i] <- ifelse(df[j,i] >= 25, 12.5+0.1*df[j,i] , (df[j,i]*(12.5-0.2*df[j,i]))/12.5   )
    }
  }
  return(df)
}

county <- function(climate,cnty){
  forlater <- cnty
  #first check that both datasets have the same HUC8s 
  check <- as.list(colnames(cnty))
  check3 <- as.list(colnames(climate))
  #climate data has but county does not
  check1 <- check3[! check3 %in% check]
  #county data has but climate does not
  check2 <- check[!  check %in% as.list(colnames(climate))]
  # make sure dataframes are ordered correctly
  cnty <- cnty[,order(as.numeric(as.character(colnames(cnty))))]
  climate <- climate[,order(as.numeric(as.character(colnames(climate))))]
  #removing bad HUC8s
  fixed <- climate[,!(names(climate)%in% check1)]
  cnty <- cnty[,!(names(cnty)%in% check2)]
  
  
  #make dataframes into matrices
  fixed <- t(as.matrix(fixed))
  cnty <- as.matrix(cnty)
  
  fixed <- cnty %*% fixed
  fixed <- as.data.frame(t(fixed))
  fixed[,(length(colnames(fixed))+1)] <- climate[,c(length(colnames(climate)))]
  colnames(fixed) <- c(forlater$FIPS,"year")
  
  fixed <- fixed[,c(length(colnames(fixed)),1:(length(colnames(fixed))-1))]
  
  return(fixed)
}

#------------------------------.
##### CONVERT HUC TO COUNTY #####

#converting HUC to county and then checking against fips codes in data

converthuc <- cntypercent
converthuc$HUC_10 <-floor(signif(converthuc$HUC_10,8)/100)
converthuc <- converthuc[,c(1,4,7)] %>% group_by(FIPS, HUC_10) %>% summarise_all(sum)
converthuc <- spread(converthuc, "HUC_10", "CountyAreaPct")
converthuc[is.na(converthuc)] <- 0
converthuc$norm <- rowSums(converthuc[,c(2:2110)], na.rm=T)

converthuc[,c(2:2111)] <- sapply(converthuc[,c(2:2111)], function(x, y) x/(y), y=converthuc[,2111])

check <- as.list(converthuc$FIPS)
check3 <- as.list(inputs$fips)
check1 <- check3[! check3 %in% check]
check2 <- check[!  check %in% check3]

converthuc <- converthuc[!converthuc$FIPS %in% check2,]

converthuc <- converthuc[,c(1:2110)]
converthuc <- converthuc[,order(as.numeric(as.character(colnames(converthuc))))]

#----------------------------------------------------------------------------------------------------.

##### CONVERT PRECIPITATION TO EFFECTIVE PRECIPITATION #####

converteffective <- function(df,converthuc){
  
  # the growing season is between April and September  
  df <- subset(df, month >=4 & month <= 9) %>% group_by(year) %>% summarise_all(mean)
  df <- df[,-2]
  
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  df <- effectiveprecip(df)
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}

##### CONVERT PRECIPITATION TO PERCENT CHANGE IN EFFECTIVE PRECIPITATION #####

convertpercent <- function(df,converthuc){
  
  # the growing season is between April and September  
  df <- subset(df, month >=4 & month <= 9) %>% group_by(year) %>% summarise_all(mean)
  df <- df[,-2]
  
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  df <- effectiveprecip(df)
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] / df[,2]
  }
  df[,2] <- 1
  return(df)
}

#----------------------------------------------------------------------------------------------------.
##### CONVERT PET TO CORRECT UNITS #####

convertpet <- function(df,converthuc){
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  for (i in 2:length(colnames(df))){
    for (j in 1:length(rownames(df))){
      df[j,i] <- df[j,i]/10
    }
  }
  
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}


calc.percent <- function(df){
  for(i in 2:length(colnames(df))){
  df[,i] <- df[,i] / df[,1]
  }
  df[,1] <- 1
  return(df)
  }

precip.percent <- calc.percent(summer.precip)

precip.percent$fips <- rownames(precip.percent)
colnames(precip.percent) <- c("precip15","precip16","precip17","precip18","precip19","precip20","precip21","precip22","precip23","precip24","precip25","precip26","precip27","precip28","precip29","precip30","precip31","precip32","precip33","precip34","precip35","precip36","precip37","precip38","precip39","precip40","precip41","precip42","precip43","precip44","precip45","precip46","precip47","precip48","precip49","precip50","precip51","precip52","precip53","precip54","precip55","precip56","precip57","precip58","precip59","precip60","precip61","precip62","precip63","precip64","precip65","precip66","precip67","precip68","precip69","precip70", "fips")
precip.percent <- precip.percent %>%
  select(fips, everything())
precip.percent$fips <- as.numeric(precip.percent$fips)
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51003, 51901, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51540, 51901, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51015, 51907, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51790, 51907, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51820, 51907, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51031, 51911, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51680, 51911, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51035, 51913, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51640, 51913, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51053, 51918, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51570, 51918, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51730, 51918, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51059, 51919, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51600, 51919, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51610, 51919, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51069, 51921, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51840, 51921, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51081, 51923, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51595, 51923, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51089, 51929, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51690, 51929, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51095, 51931, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51830, 51931, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51121, 51933, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51750, 51933, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51143, 51939, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51590, 51939, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51149, 51941, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51670, 51941, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51153, 51942, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51683, 51942, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51685, 51942, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51161, 51944, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51775, 51944, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51163, 51945, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51530, 51945, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51678, 51945, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51165, 51947, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51660, 51947, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51175, 51949, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51620, 51949, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51177, 51951, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51630, 51951, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51191, 51953, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51520, 51953, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51195, 51955, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51720, 51955, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51199, 51958, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51735, 51958, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51560, 51903, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51005, 51903, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51580, 51903, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51780, 51083, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51515, 51909, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51019, 51909, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 51149, 51941, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 12025, 12086, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 46102, 46113, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 55078, 55901, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 55115, 55901, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 08014, 08013, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 04012, 04027, precip.percent[,c(1)])
precip.percent[,c(1)] <- ifelse(precip.percent[,c(1)] == 35006, 35061, precip.percent[,c(1)])
precip.percent <- precip.percent %>% group_by(fips) %>% summarise_all(funs(sum))

precip <- converteffective(precip, converthuc)
colnames(precip) <- c("fips","precip15","precip16","precip17","precip18","precip19","precip20","precip21","precip22","precip23","precip24","precip25","precip26","precip27","precip28","precip29","precip30","precip31","precip32","precip33","precip34","precip35","precip36","precip37","precip38","precip39","precip40","precip41","precip42","precip43","precip44","precip45","precip46","precip47","precip48","precip49","precip50","precip51","precip52","precip53","precip54","precip55","precip56","precip57","precip58","precip59","precip60","precip61","precip62","precip63","precip64","precip65","precip66","precip67","precip68","precip69","precip70")

pet <- convertpet(pet,converthuc)
colnames(pet) <- c("fips","et15","et16","et17","et18","et19","et20","et21","et22","et23","et24","et25","et26","et27","et28","et29","et30","et31","et32","et33","et34","et35","et36","et37","et38","et39","et40","et41","et42","et43","et44","et45","et46","et47","et48","et49","et50","et51","et52","et53","et54","et55","et56","et57","et58","et59","et60","et61","et62","et63","et64","et65","et66","et67","et68","et69","et70")

#----------------------------------------------------------------------------------------------------.
####### MERGE CLIMATE EFFECTS AND CARBON DATA #######
#incorporate carbon effects into climate dataframe

climate <- merge(precip, pet, by="fips")
if (carbon==45){
  climate <- merge(climate,climate.45, by="fips")
}
if (carbon==85) {
  climate <- merge(climate,climate.85, by="fips")
}

if (gcm=="base"){
  climate[,c(114:169)] <- 0
}


# #----------------------------------------------------------------------------------------------------.
# ##### CLIMATE DP FUNCTION #####
# #NOTE: since withdrawal per unit for DP is per 1000 people, we divide by 1000 in our calculation
# #this function differs slightly from the paper but produces the same results. We multiply by population and
# #consumptive use proportion later.
# climatedp <- function(climate1,outdoor1) 
# {
#   for (n in 2:57){
#     j <- n + 56
#     climate1[,n] <- (climate1[,n]*(-1.415) +climate1[,j]*(0.778))/1000
#   }
#   
#   climate1 <- climate1[,c(1:57,114:169)]
#   colnames(climate1) <- c("fips","climate15","climate16","climate17","climate18","climate19","climate20","climate21","climate22","climate23","climate24","climate25","climate26","climate27","climate28","climate29","climate30","climate31","climate32","climate33","climate34","climate35","climate36","climate37","climate38","climate39","climate40","climate41","climate42","climate43","climate44","climate45","climate46","climate47","climate48","climate49","climate50","climate51","climate52","climate53","climate54","climate55","climate56","climate57","climate58","climate59","climate60","climate61","climate62","climate63","climate64","climate65","climate66","climate67","climate68","climate69","climate70",
#                           "plantwater15","plantwater16","plantwater17","plantwater18","plantwater19","plantwater20","plantwater21","plantwater22","plantwater23","plantwater24","plantwater25","plantwater26","plantwater27","plantwater28","plantwater29","plantwater30","plantwater31","plantwater32","plantwater33","plantwater34","plantwater35","plantwater36","plantwater37","plantwater38","plantwater39","plantwater40","plantwater41","plantwater42","plantwater43","plantwater44","plantwater45","plantwater46","plantwater47","plantwater48","plantwater49","plantwater50","plantwater51","plantwater52","plantwater53","plantwater54","plantwater55","plantwater56","plantwater57","plantwater58","plantwater59","plantwater60","plantwater61","plantwater62","plantwater63","plantwater64","plantwater65","plantwater66","plantwater67","plantwater68","plantwater69","plantwater70")
#   climate1 <-merge(climate1, outdoor1, by="fips")
#   for (i in 58:113) {
#     climate1[,i] <- climate1[,i] * climate1[,114]
#   }
#   return(climate1[,-114])
# }
# climate.dp <- climatedp(climate, outdoor)
# 

#---------------------------------------------------------------------------------.
################# DOMESTIC/PUBLIC PROJECTIONS TO 2070 #########################
#This function calculates dp projections out to 2070 using function from Tom's paper (Foti, Ramirez, Brown, 2010 FS RPA Assessment TECHNICAL DOCUMENT TO SUPPORT WATER ASSESSMENT)

demand.base <- subset(pop.inc, year == 2015)
demand.base <- merge(demand.base, wd.2015, by = "fips")
demand.base$wpu.dp <- demand.base$dp / demand.base$pop
demand.base$wpu.ind <- demand.base$indust / demand.base$inc

keeps <- c("fips","year","pop","ssp","inc","wpu.dp","state","indust","dp","thermo", "thermo.gWh","mining","livestock",
           "irrigation","IR.acres")

demand.base <- demand.base[,names(demand.base) %in% keeps]
#why did year fall off?
demand.base$year <- 2015


demand.proj <- subset(pop.inc, year != 2015)

demand.proj$wpu.dp <- NA
demand.proj$state <- NA
demand.proj$indust <- NA
demand.proj$dp <- NA
demand.proj$thermo <- NA
demand.proj$thermo.gWh <- NA
demand.proj$mining <- NA
demand.proj$livestock <- NA
demand.proj$irrigation <- NA
demand.proj$IR.acres <- NA


drops <- c("X")
demand.proj <- demand.proj[,!names(demand.proj) %in% drops]

ew <- wd.2015 %>% select(fips, EastWest, DP.growth, DP.decay)

#select all base year values
base <- demand.base %>% select(fips, wpu.dp) 
demand <- rbind(demand.base, demand.proj)

demand <- merge(demand, ew, by="fips")
demand <- merge(demand, base, by="fips")

attach(demand)
demand <- demand[order(fips,ssp,year),]
detach(demand)

demand$wpu.dp0 <- demand$wpu.dp.y

demand$wpu.dp <- demand$wpu.dp0 * (1+demand$DP.growth*(1+demand$DP.decay))^(demand$year-2015)
demand$dp.t <- demand$pop * demand$wpu.dp
# At this point first year dp is slight off for some SSPs. I think the 
# baseline wpu.dp might be off for some SSPs. You can see this by comparing dp
# and dp.t for year=2015

# dp demand with climate
cc.dp1 <- -1.415    # coefficient on change in summertime precip
cc.dp2 <- 0.778     # coefficient on change in pet
outdoor <-0         # placeholder. need to look more into this

demand$wpu.dp.cc <- (cc.dp1*summer.precip[1,1] + cc.dp2*pet[2,2] - demand$wpu.dp0 * outdoor) / 1000
# the original code did not have the last term and divided by 1000

demand$dp.cc <- demand$pop * demand$wpu.dp.cc


# 
# 
# 
# water1$DP.perUnit <- water1$dp / water1$pr_pop_2015
# water2$DP.perUnit <- water2$dp / water2$pr_pop_2015
# water3$DP.perUnit <- water3$dp / water3$pr_pop_2015
# water4$DP.perUnit <- water4$dp / water4$pr_pop_2015
# water5$DP.perUnit <- water5$dp / water5$pr_pop_2015
# 
# 
# dp.demand<-dp.demand %>% mutate(dp.wpu.2020 = DP.perUnit*(1+DP.growth*(1+DP.decay)^(2020-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2025 = dp.wpu.2020*(1+DP.growth*(1+DP.decay)^(2025-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2030 = dp.wpu.2025*(1+DP.growth*(1+DP.decay)^(2030-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2035 = dp.wpu.2030*(1+DP.growth*(1+DP.decay)^(2035-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2040 = dp.wpu.2035*(1+DP.growth*(1+DP.decay)^(2040-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2045 = dp.wpu.2040*(1+DP.growth*(1+DP.decay)^(2045-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2050 = dp.wpu.2045*(1+DP.growth*(1+DP.decay)^(2050-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2055 = dp.wpu.2050*(1+DP.growth*(1+DP.decay)^(2055-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2060 = dp.wpu.2055*(1+DP.growth*(1+DP.decay)^(2060-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2065 = dp.wpu.2060*(1+DP.growth*(1+DP.decay)^(2065-2015))^5 )
# dp.demand<-dp.demand %>% mutate(dp.wpu.2070 = dp.wpu.2065*(1+DP.growth*(1+DP.decay)^(2070-2015))^5 )
# dp.demand$sector <- "dp"
# 
# 
# 
# 
# 
# 
# dp.project <- function(water1, pop1,climate) 
#  {
# 
# ##### This is isolating columns for FIPS, population (unit?), growth & decay rates, and dp per unit (per 1000 people I believe?) #####
#   dp.demand <- water1 
#   
# #    dp.demand <- water1[,c(2,5,10,11,8)] 
#   
# # This equation is located in the water use projection method paper (equation 5)
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2020 = DP.perUnit*(1+DP.growth*(1+DP.decay)^(2020-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2025 = dp.wpu.2020*(1+DP.growth*(1+DP.decay)^(2025-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2030 = dp.wpu.2025*(1+DP.growth*(1+DP.decay)^(2030-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2035 = dp.wpu.2030*(1+DP.growth*(1+DP.decay)^(2035-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2040 = dp.wpu.2035*(1+DP.growth*(1+DP.decay)^(2040-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2045 = dp.wpu.2040*(1+DP.growth*(1+DP.decay)^(2045-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2050 = dp.wpu.2045*(1+DP.growth*(1+DP.decay)^(2050-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2055 = dp.wpu.2050*(1+DP.growth*(1+DP.decay)^(2055-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2060 = dp.wpu.2055*(1+DP.growth*(1+DP.decay)^(2060-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2065 = dp.wpu.2060*(1+DP.growth*(1+DP.decay)^(2065-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2070 = dp.wpu.2065*(1+DP.growth*(1+DP.decay)^(2070-2015))^5 )
#   dp.demand$sector <- "dp"
# 
#   # not sure the following are needed, though we do need to change column names
#   keeps <- c("fips","sector","DP.perUnit","dp.wpu.2020","dp.wpu.2025","dp.wpu.2030","dp.wpu.2035","dp.wpu.2040","dp.wpu.2045","dp.wpu.2050","dp.wpu.2055","dp.wpu.2060","dp.wpu.2065","dp.wpu.2070")
# #  dp.demand2 <- dp.demand[,c(1,17,5:16)] # line below specifies what these columns are
#   dp.demand2 <- dp.demand[keeps]
# 
#   colnames(dp.demand2) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
#   dp.demand3 <- linapprox(dp.demand2)
# 
# # fixing population fips codes and then merging with Domestic water use (for now assume all files need fips fixes)
#   pop1 <- fipsfix(pop1)
# 
#   dp.demand4 <- merge(dp.demand3, pop1, by="fips")
# 
# # these columns are not quite right
# # dp.demand5 <- dp.demand4[,c(1,5:16,2,18:28)]
# 
# # pull climate from below
#   dp.demand5 <- merge(dp.demand4, climate.dp, by="fips")
# 
#   dp.dem <- dp.demand5
# 
#   dp.dem$Y2015 <- ifelse(dp.dem$Y2015 != 0, dp.dem$pr_pop_2015 * (dp.dem$Y2015 + dp.dem$climate15), dp.dem$Y2015) 
#   dp.dem$Y2016 <- ifelse(dp.dem$Y2016 != 0, dp.dem$pr_pop_2016 * (dp.dem$Y2016 + dp.dem$climate16), dp.dem$Y2016)
#   dp.dem$Y2017 <- ifelse(dp.dem$Y2017 != 0, dp.dem$pr_pop_2017 * (dp.dem$Y2017 + dp.dem$climate17), dp.dem$Y2017)
#   dp.dem$Y2018 <- ifelse(dp.dem$Y2018 != 0, dp.dem$pr_pop_2018 * (dp.dem$Y2018 + dp.dem$climate18), dp.dem$Y2018)
#   dp.dem$Y2019 <- ifelse(dp.dem$Y2019 != 0, dp.dem$pr_pop_2019 * (dp.dem$Y2019 + dp.dem$climate19), dp.dem$Y2019)
#   dp.dem$Y2020 <- ifelse(dp.dem$Y2020 != 0, dp.dem$pr_pop_2020 * (dp.dem$Y2020 + dp.dem$climate20), dp.dem$Y2020)
#   dp.dem$Y2021 <- ifelse(dp.dem$Y2021 != 0, dp.dem$pr_pop_2021 * (dp.dem$Y2021 + dp.dem$climate21), dp.dem$Y2021)
#   dp.dem$Y2022 <- ifelse(dp.dem$Y2022 != 0, dp.dem$pr_pop_2022 * (dp.dem$Y2022 + dp.dem$climate22), dp.dem$Y2022)
#   dp.dem$Y2023 <- ifelse(dp.dem$Y2023 != 0, dp.dem$pr_pop_2023 * (dp.dem$Y2023 + dp.dem$climate23), dp.dem$Y2023)
#   dp.dem$Y2024 <- ifelse(dp.dem$Y2024 != 0, dp.dem$pr_pop_2024 * (dp.dem$Y2024 + dp.dem$climate24), dp.dem$Y2024)
#   dp.dem$Y2025 <- ifelse(dp.dem$Y2025 != 0, dp.dem$pr_pop_2025 * (dp.dem$Y2025 + dp.dem$climate25), dp.dem$Y2025)
#   dp.dem$Y2026 <- ifelse(dp.dem$Y2026 != 0, dp.dem$pr_pop_2026 * (dp.dem$Y2026 + dp.dem$climate26), dp.dem$Y2026)
#   dp.dem$Y2027 <- ifelse(dp.dem$Y2027 != 0, dp.dem$pr_pop_2027 * (dp.dem$Y2027 + dp.dem$climate27), dp.dem$Y2027)
#   dp.dem$Y2028 <- ifelse(dp.dem$Y2028 != 0, dp.dem$pr_pop_2028 * (dp.dem$Y2028 + dp.dem$climate28), dp.dem$Y2028)
#   dp.dem$Y2029 <- ifelse(dp.dem$Y2029 != 0, dp.dem$pr_pop_2029 * (dp.dem$Y2029 + dp.dem$climate29), dp.dem$Y2029)
#   dp.dem$Y2030 <- ifelse(dp.dem$Y2030 != 0, dp.dem$pr_pop_2030 * (dp.dem$Y2030 + dp.dem$climate30), dp.dem$Y2030)
#   dp.dem$Y2031 <- ifelse(dp.dem$Y2031 != 0, dp.dem$pr_pop_2031 * (dp.dem$Y2031 + dp.dem$climate31), dp.dem$Y2031)
#   dp.dem$Y2032 <- ifelse(dp.dem$Y2032 != 0, dp.dem$pr_pop_2032 * (dp.dem$Y2032 + dp.dem$climate32), dp.dem$Y2032)
#   dp.dem$Y2033 <- ifelse(dp.dem$Y2033 != 0, dp.dem$pr_pop_2033 * (dp.dem$Y2033 + dp.dem$climate33), dp.dem$Y2033)
#   dp.dem$Y2034 <- ifelse(dp.dem$Y2034 != 0, dp.dem$pr_pop_2034 * (dp.dem$Y2034 + dp.dem$climate34), dp.dem$Y2034)
#   dp.dem$Y2035 <- ifelse(dp.dem$Y2035 != 0, dp.dem$pr_pop_2035 * (dp.dem$Y2035 + dp.dem$climate35), dp.dem$Y2035)
#   dp.dem$Y2036 <- ifelse(dp.dem$Y2036 != 0, dp.dem$pr_pop_2036 * (dp.dem$Y2036 + dp.dem$climate36), dp.dem$Y2036)
#   dp.dem$Y2037 <- ifelse(dp.dem$Y2037 != 0, dp.dem$pr_pop_2037 * (dp.dem$Y2037 + dp.dem$climate37), dp.dem$Y2037)
#   dp.dem$Y2038 <- ifelse(dp.dem$Y2038 != 0, dp.dem$pr_pop_2038 * (dp.dem$Y2038 + dp.dem$climate38), dp.dem$Y2038)
#   dp.dem$Y2039 <- ifelse(dp.dem$Y2039 != 0, dp.dem$pr_pop_2039 * (dp.dem$Y2039 + dp.dem$climate39), dp.dem$Y2039)
#   dp.dem$Y2040 <- ifelse(dp.dem$Y2040 != 0, dp.dem$pr_pop_2040 * (dp.dem$Y2040 + dp.dem$climate40), dp.dem$Y2040)
#   dp.dem$Y2041 <- ifelse(dp.dem$Y2041 != 0, dp.dem$pr_pop_2041 * (dp.dem$Y2041 + dp.dem$climate41), dp.dem$Y2041)
#   dp.dem$Y2042 <- ifelse(dp.dem$Y2042 != 0, dp.dem$pr_pop_2042 * (dp.dem$Y2042 + dp.dem$climate42), dp.dem$Y2042)
#   dp.dem$Y2043 <- ifelse(dp.dem$Y2043 != 0, dp.dem$pr_pop_2043 * (dp.dem$Y2043 + dp.dem$climate43), dp.dem$Y2043)
#   dp.dem$Y2044 <- ifelse(dp.dem$Y2044 != 0, dp.dem$pr_pop_2044 * (dp.dem$Y2044 + dp.dem$climate44), dp.dem$Y2044)
#   dp.dem$Y2045 <- ifelse(dp.dem$Y2045 != 0, dp.dem$pr_pop_2045 * (dp.dem$Y2045 + dp.dem$climate45), dp.dem$Y2045)
#   dp.dem$Y2046 <- ifelse(dp.dem$Y2046 != 0, dp.dem$pr_pop_2046 * (dp.dem$Y2046 + dp.dem$climate46), dp.dem$Y2046)
#   dp.dem$Y2047 <- ifelse(dp.dem$Y2047 != 0, dp.dem$pr_pop_2047 * (dp.dem$Y2047 + dp.dem$climate47), dp.dem$Y2047)
#   dp.dem$Y2048 <- ifelse(dp.dem$Y2048 != 0, dp.dem$pr_pop_2048 * (dp.dem$Y2048 + dp.dem$climate48), dp.dem$Y2048)
#   dp.dem$Y2049 <- ifelse(dp.dem$Y2049 != 0, dp.dem$pr_pop_2049 * (dp.dem$Y2049 + dp.dem$climate49), dp.dem$Y2049)
#   dp.dem$Y2050 <- ifelse(dp.dem$Y2050 != 0, dp.dem$pr_pop_2050 * (dp.dem$Y2050 + dp.dem$climate50), dp.dem$Y2050)
#   dp.dem$Y2051 <- ifelse(dp.dem$Y2051 != 0, dp.dem$pr_pop_2051 * (dp.dem$Y2051 + dp.dem$climate51), dp.dem$Y2051)
#   dp.dem$Y2052 <- ifelse(dp.dem$Y2052 != 0, dp.dem$pr_pop_2052 * (dp.dem$Y2052 + dp.dem$climate52), dp.dem$Y2052)
#   dp.dem$Y2053 <- ifelse(dp.dem$Y2053 != 0, dp.dem$pr_pop_2053 * (dp.dem$Y2053 + dp.dem$climate53), dp.dem$Y2053)
#   dp.dem$Y2054 <- ifelse(dp.dem$Y2054 != 0, dp.dem$pr_pop_2054 * (dp.dem$Y2054 + dp.dem$climate54), dp.dem$Y2054)
#   dp.dem$Y2055 <- ifelse(dp.dem$Y2055 != 0, dp.dem$pr_pop_2055 * (dp.dem$Y2055 + dp.dem$climate55), dp.dem$Y2055)
#   dp.dem$Y2056 <- ifelse(dp.dem$Y2056 != 0, dp.dem$pr_pop_2056 * (dp.dem$Y2056 + dp.dem$climate56), dp.dem$Y2056)
#   dp.dem$Y2057 <- ifelse(dp.dem$Y2057 != 0, dp.dem$pr_pop_2057 * (dp.dem$Y2057 + dp.dem$climate57), dp.dem$Y2057)
#   dp.dem$Y2058 <- ifelse(dp.dem$Y2058 != 0, dp.dem$pr_pop_2058 * (dp.dem$Y2058 + dp.dem$climate58), dp.dem$Y2058)
#   dp.dem$Y2059 <- ifelse(dp.dem$Y2059 != 0, dp.dem$pr_pop_2059 * (dp.dem$Y2059 + dp.dem$climate59), dp.dem$Y2059)
#   dp.dem$Y2060 <- ifelse(dp.dem$Y2060 != 0, dp.dem$pr_pop_2060 * (dp.dem$Y2060 + dp.dem$climate60), dp.dem$Y2060)
#   dp.dem$Y2061 <- ifelse(dp.dem$Y2061 != 0, dp.dem$pr_pop_2061 * (dp.dem$Y2061 + dp.dem$climate61), dp.dem$Y2061)
#   dp.dem$Y2062 <- ifelse(dp.dem$Y2062 != 0, dp.dem$pr_pop_2062 * (dp.dem$Y2062 + dp.dem$climate62), dp.dem$Y2062)
#   dp.dem$Y2063 <- ifelse(dp.dem$Y2063 != 0, dp.dem$pr_pop_2063 * (dp.dem$Y2063 + dp.dem$climate63), dp.dem$Y2063)
#   dp.dem$Y2064 <- ifelse(dp.dem$Y2064 != 0, dp.dem$pr_pop_2064 * (dp.dem$Y2064 + dp.dem$climate64), dp.dem$Y2064)
#   dp.dem$Y2065 <- ifelse(dp.dem$Y2065 != 0, dp.dem$pr_pop_2065 * (dp.dem$Y2065 + dp.dem$climate65), dp.dem$Y2065)
#   dp.dem$Y2066 <- ifelse(dp.dem$Y2066 != 0, dp.dem$pr_pop_2066 * (dp.dem$Y2066 + dp.dem$climate66), dp.dem$Y2066)
#   dp.dem$Y2067 <- ifelse(dp.dem$Y2067 != 0, dp.dem$pr_pop_2067 * (dp.dem$Y2067 + dp.dem$climate67), dp.dem$Y2067)
#   dp.dem$Y2068 <- ifelse(dp.dem$Y2068 != 0, dp.dem$pr_pop_2068 * (dp.dem$Y2068 + dp.dem$climate68), dp.dem$Y2068)
#   dp.dem$Y2069 <- ifelse(dp.dem$Y2069 != 0, dp.dem$pr_pop_2069 * (dp.dem$Y2069 + dp.dem$climate69), dp.dem$Y2069)
#   dp.dem$Y2070 <- ifelse(dp.dem$Y2070 != 0, dp.dem$pr_pop_2070 * (dp.dem$Y2070 + dp.dem$climate70), dp.dem$Y2070)
# 
# 
#  water1<-dp.dem[,c(1:58)]
# # colnames(water1p) <- c("fips", "sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
# return(water1)
# }
# 
# #----------------------------------------------------------------------------------------------------.
# 
# # creating separate function for withdrawals without climate impacts
# 
# dp.withdrawals <- function(water1, pop1) 
# {
#   
#   ##### This is isolating columns for FIPS, population (unit?), growth & decay rates, and dp per unit (per 1000 people I believe?) #####
#   dp.demand <- water1 
#   
# # withdrawals per unit grow and a decaying rate  
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2020 = DP.perUnit*(1+DP.growth*(1+DP.decay)^(2020-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2025 = dp.wpu.2020*(1+DP.growth*(1+DP.decay)^(2025-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2030 = dp.wpu.2025*(1+DP.growth*(1+DP.decay)^(2030-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2035 = dp.wpu.2030*(1+DP.growth*(1+DP.decay)^(2035-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2040 = dp.wpu.2035*(1+DP.growth*(1+DP.decay)^(2040-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2045 = dp.wpu.2040*(1+DP.growth*(1+DP.decay)^(2045-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2050 = dp.wpu.2045*(1+DP.growth*(1+DP.decay)^(2050-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2055 = dp.wpu.2050*(1+DP.growth*(1+DP.decay)^(2055-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2060 = dp.wpu.2055*(1+DP.growth*(1+DP.decay)^(2060-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2065 = dp.wpu.2060*(1+DP.growth*(1+DP.decay)^(2065-2015))^5 )
#   dp.demand<-dp.demand %>% mutate(dp.wpu.2070 = dp.wpu.2065*(1+DP.growth*(1+DP.decay)^(2070-2015))^5 )
#   dp.demand$sector <- "dp"
#   
#   # fixing population fips codes and then merging with Domestic water use (for now assume all files need fips fixes)
#   pop1 <- fipsfix(pop1)
#   
#   dp.dem$wd <- ifelse(dp.dem$Y2015 != 0, dp.dem$pr_pop_2015 * (dp.dem$Y2015 + dp.dem$climate15), dp.dem$Y2015) 
#   dp.dem$wd <- ifelse(dp.dem$Y2016 != 0, dp.dem$pr_pop_2016 * (dp.dem$Y2016 + dp.dem$climate16), dp.dem$Y2016)
#   dp.dem$wd <- ifelse(dp.dem$Y2017 != 0, dp.dem$pr_pop_2017 * (dp.dem$Y2017 + dp.dem$climate17), dp.dem$Y2017)
#   dp.dem$wd <- ifelse(dp.dem$Y2018 != 0, dp.dem$pr_pop_2018 * (dp.dem$Y2018 + dp.dem$climate18), dp.dem$Y2018)
#   dp.dem$wd <- ifelse(dp.dem$Y2019 != 0, dp.dem$pr_pop_2019 * (dp.dem$Y2019 + dp.dem$climate19), dp.dem$Y2019)
#   dp.dem$wd <- ifelse(dp.dem$Y2020 != 0, dp.dem$pr_pop_2020 * (dp.dem$Y2020 + dp.dem$climate20), dp.dem$Y2020)
#   dp.dem$wd <- ifelse(dp.dem$Y2021 != 0, dp.dem$pr_pop_2021 * (dp.dem$Y2021 + dp.dem$climate21), dp.dem$Y2021)
#   dp.dem$wd <- ifelse(dp.dem$Y2022 != 0, dp.dem$pr_pop_2022 * (dp.dem$Y2022 + dp.dem$climate22), dp.dem$Y2022)
#   dp.dem$wd <- ifelse(dp.dem$Y2023 != 0, dp.dem$pr_pop_2023 * (dp.dem$Y2023 + dp.dem$climate23), dp.dem$Y2023)
#   dp.dem$wd <- ifelse(dp.dem$Y2024 != 0, dp.dem$pr_pop_2024 * (dp.dem$Y2024 + dp.dem$climate24), dp.dem$Y2024)
#   dp.dem$wd <- ifelse(dp.dem$Y2025 != 0, dp.dem$pr_pop_2025 * (dp.dem$Y2025 + dp.dem$climate25), dp.dem$Y2025)
#   dp.dem$wd <- ifelse(dp.dem$Y2026 != 0, dp.dem$pr_pop_2026 * (dp.dem$Y2026 + dp.dem$climate26), dp.dem$Y2026)
#   dp.dem$wd <- ifelse(dp.dem$Y2027 != 0, dp.dem$pr_pop_2027 * (dp.dem$Y2027 + dp.dem$climate27), dp.dem$Y2027)
#   dp.dem$wd <- ifelse(dp.dem$Y2028 != 0, dp.dem$pr_pop_2028 * (dp.dem$Y2028 + dp.dem$climate28), dp.dem$Y2028)
#   dp.dem$wd <- ifelse(dp.dem$Y2029 != 0, dp.dem$pr_pop_2029 * (dp.dem$Y2029 + dp.dem$climate29), dp.dem$Y2029)
#   dp.dem$wd <- ifelse(dp.dem$Y2030 != 0, dp.dem$pr_pop_2030 * (dp.dem$Y2030 + dp.dem$climate30), dp.dem$Y2030)
#   dp.dem$wd <- ifelse(dp.dem$Y2031 != 0, dp.dem$pr_pop_2031 * (dp.dem$Y2031 + dp.dem$climate31), dp.dem$Y2031)
#   dp.dem$wd <- ifelse(dp.dem$Y2032 != 0, dp.dem$pr_pop_2032 * (dp.dem$Y2032 + dp.dem$climate32), dp.dem$Y2032)
#   dp.dem$wd <- ifelse(dp.dem$Y2033 != 0, dp.dem$pr_pop_2033 * (dp.dem$Y2033 + dp.dem$climate33), dp.dem$Y2033)
#   dp.dem$wd <- ifelse(dp.dem$Y2034 != 0, dp.dem$pr_pop_2034 * (dp.dem$Y2034 + dp.dem$climate34), dp.dem$Y2034)
#   dp.dem$wd <- ifelse(dp.dem$Y2035 != 0, dp.dem$pr_pop_2035 * (dp.dem$Y2035 + dp.dem$climate35), dp.dem$Y2035)
#   dp.dem$wd <- ifelse(dp.dem$Y2036 != 0, dp.dem$pr_pop_2036 * (dp.dem$Y2036 + dp.dem$climate36), dp.dem$Y2036)
#   dp.dem$wd <- ifelse(dp.dem$Y2037 != 0, dp.dem$pr_pop_2037 * (dp.dem$Y2037 + dp.dem$climate37), dp.dem$Y2037)
#   dp.dem$wd <- ifelse(dp.dem$Y2038 != 0, dp.dem$pr_pop_2038 * (dp.dem$Y2038 + dp.dem$climate38), dp.dem$Y2038)
#   dp.dem$wd <- ifelse(dp.dem$Y2039 != 0, dp.dem$pr_pop_2039 * (dp.dem$Y2039 + dp.dem$climate39), dp.dem$Y2039)
#   dp.dem$wd <- ifelse(dp.dem$Y2040 != 0, dp.dem$pr_pop_2040 * (dp.dem$Y2040 + dp.dem$climate40), dp.dem$Y2040)
#   dp.dem$wd <- ifelse(dp.dem$Y2041 != 0, dp.dem$pr_pop_2041 * (dp.dem$Y2041 + dp.dem$climate41), dp.dem$Y2041)
#   dp.dem$wd <- ifelse(dp.dem$Y2042 != 0, dp.dem$pr_pop_2042 * (dp.dem$Y2042 + dp.dem$climate42), dp.dem$Y2042)
#   dp.dem$wd <- ifelse(dp.dem$Y2043 != 0, dp.dem$pr_pop_2043 * (dp.dem$Y2043 + dp.dem$climate43), dp.dem$Y2043)
#   dp.dem$wd <- ifelse(dp.dem$Y2044 != 0, dp.dem$pr_pop_2044 * (dp.dem$Y2044 + dp.dem$climate44), dp.dem$Y2044)
#   dp.dem$wd <- ifelse(dp.dem$Y2045 != 0, dp.dem$pr_pop_2045 * (dp.dem$Y2045 + dp.dem$climate45), dp.dem$Y2045)
#   dp.dem$wd <- ifelse(dp.dem$Y2046 != 0, dp.dem$pr_pop_2046 * (dp.dem$Y2046 + dp.dem$climate46), dp.dem$Y2046)
#   dp.dem$wd <- ifelse(dp.dem$Y2047 != 0, dp.dem$pr_pop_2047 * (dp.dem$Y2047 + dp.dem$climate47), dp.dem$Y2047)
#   dp.dem$wd <- ifelse(dp.dem$Y2048 != 0, dp.dem$pr_pop_2048 * (dp.dem$Y2048 + dp.dem$climate48), dp.dem$Y2048)
#   dp.dem$wd <- ifelse(dp.dem$Y2049 != 0, dp.dem$pr_pop_2049 * (dp.dem$Y2049 + dp.dem$climate49), dp.dem$Y2049)
#   dp.dem$wd <- ifelse(dp.dem$Y2050 != 0, dp.dem$pr_pop_2050 * (dp.dem$Y2050 + dp.dem$climate50), dp.dem$Y2050)
#   dp.dem$wd <- ifelse(dp.dem$Y2051 != 0, dp.dem$pr_pop_2051 * (dp.dem$Y2051 + dp.dem$climate51), dp.dem$Y2051)
#   dp.dem$wd <- ifelse(dp.dem$Y2052 != 0, dp.dem$pr_pop_2052 * (dp.dem$Y2052 + dp.dem$climate52), dp.dem$Y2052)
#   dp.dem$wd <- ifelse(dp.dem$Y2053 != 0, dp.dem$pr_pop_2053 * (dp.dem$Y2053 + dp.dem$climate53), dp.dem$Y2053)
#   dp.dem$wd <- ifelse(dp.dem$Y2054 != 0, dp.dem$pr_pop_2054 * (dp.dem$Y2054 + dp.dem$climate54), dp.dem$Y2054)
#   dp.dem$wd <- ifelse(dp.dem$Y2055 != 0, dp.dem$pr_pop_2055 * (dp.dem$Y2055 + dp.dem$climate55), dp.dem$Y2055)
#   dp.dem$wd <- ifelse(dp.dem$Y2056 != 0, dp.dem$pr_pop_2056 * (dp.dem$Y2056 + dp.dem$climate56), dp.dem$Y2056)
#   dp.dem$wd <- ifelse(dp.dem$Y2057 != 0, dp.dem$pr_pop_2057 * (dp.dem$Y2057 + dp.dem$climate57), dp.dem$Y2057)
#   dp.dem$Y2058 <- ifelse(dp.dem$Y2058 != 0, dp.dem$pr_pop_2058 * (dp.dem$Y2058 + dp.dem$climate58), dp.dem$Y2058)
#   dp.dem$Y2059 <- ifelse(dp.dem$Y2059 != 0, dp.dem$pr_pop_2059 * (dp.dem$Y2059 + dp.dem$climate59), dp.dem$Y2059)
#   dp.dem$Y2060 <- ifelse(dp.dem$Y2060 != 0, dp.dem$pr_pop_2060 * (dp.dem$Y2060 + dp.dem$climate60), dp.dem$Y2060)
#   dp.dem$Y2061 <- ifelse(dp.dem$Y2061 != 0, dp.dem$pr_pop_2061 * (dp.dem$Y2061 + dp.dem$climate61), dp.dem$Y2061)
#   dp.dem$Y2062 <- ifelse(dp.dem$Y2062 != 0, dp.dem$pr_pop_2062 * (dp.dem$Y2062 + dp.dem$climate62), dp.dem$Y2062)
#   dp.dem$Y2063 <- ifelse(dp.dem$Y2063 != 0, dp.dem$pr_pop_2063 * (dp.dem$Y2063 + dp.dem$climate63), dp.dem$Y2063)
#   dp.dem$Y2064 <- ifelse(dp.dem$Y2064 != 0, dp.dem$pr_pop_2064 * (dp.dem$Y2064 + dp.dem$climate64), dp.dem$Y2064)
#   dp.dem$Y2065 <- ifelse(dp.dem$Y2065 != 0, dp.dem$pr_pop_2065 * (dp.dem$Y2065 + dp.dem$climate65), dp.dem$Y2065)
#   dp.dem$Y2066 <- ifelse(dp.dem$Y2066 != 0, dp.dem$pr_pop_2066 * (dp.dem$Y2066 + dp.dem$climate66), dp.dem$Y2066)
#   dp.dem$Y2067 <- ifelse(dp.dem$Y2067 != 0, dp.dem$pr_pop_2067 * (dp.dem$Y2067 + dp.dem$climate67), dp.dem$Y2067)
#   dp.dem$Y2068 <- ifelse(dp.dem$Y2068 != 0, dp.dem$pr_pop_2068 * (dp.dem$Y2068 + dp.dem$climate68), dp.dem$Y2068)
#   dp.dem$Y2069 <- ifelse(dp.dem$Y2069 != 0, dp.dem$pr_pop_2069 * (dp.dem$Y2069 + dp.dem$climate69), dp.dem$Y2069)
#   dp.dem$Y2070 <- ifelse(dp.dem$Y2070 != 0, dp.dem$pr_pop_2070 * (dp.dem$Y2070 + dp.dem$climate70), dp.dem$Y2070)
#   
# 
#   water1<-dp.dem[,c(1:58)]
#   # colnames(water1p) <- c("fips", "sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
#   return(water1)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# water1$DP.perUnit <- water1$dp / water1$pr_pop_2015
# water2$DP.perUnit <- water2$dp / water2$pr_pop_2015
# water3$DP.perUnit <- water3$dp / water3$pr_pop_2015
# water4$DP.perUnit <- water4$dp / water4$pr_pop_2015
# water5$DP.perUnit <- water5$dp / water5$pr_pop_2015
# 
# #bring in water inputs, population, and income 
# # these don't output monthly, they output annual
# 
# # need to make sure dp.climate = 0 if dp.Perunit = 0
# dp.ssp1 <- dp.project(water1,pop1,climate.dp)
# dp.ssp2 <- dp.project(water2,pop2,climate.dp)
# dp.ssp3 <- dp.project(water3,pop3,climate.dp)
# dp.ssp4 <- dp.project(water4,pop4,climate.dp)
# dp.ssp5 <- dp.project(water5,pop5,climate.dp)
# # 
# 
# dp.ssp1.county <- dp.ssp1
# dp.ssp2.county <- dp.ssp2
# dp.ssp3.county <- dp.ssp3
# dp.ssp4.county <- dp.ssp4
# dp.ssp5.county <- dp.ssp5


# # per unit calc
# dpUnit <- select(water1, fips, domestic, pr_pop_2015)
# dpUnit$domestic <- dpUnit$domestic*1000000
# dpUnit$pr_pop_2015 <- dpUnit$pr_pop_2015*1000
# dpUnit$perUnit <- dpUnit$domestic / dpUnit$pr_pop_2015
# dpUnitUSGS <- select(wd.2015, fips, dp.perCapita)
# dpUnit <- merge(dpUnit, dpUnitUSGS, by = "fips")
# dpUnit$aveperUnit <- (dpUnit$perUnit + dpUnit$dp.perCapita) / 2
# dpUnit$dp.perCapita <- ifelse(is.na(dpUnit$dp.perCapita), dpUnit$perUnit, dpUnit$dp.perCapita)
# write.csv(dpUnit, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/PerUnit/d_perUnit.csv")

#----------------------------------------------------------------------------------------------------.
