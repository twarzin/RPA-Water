
# File for aggregating water demand results into subregions
# Initial code was done by Shaunie Rasmussen, currently 
# maintained by Travis Warziniack

# This whole file is working with consumptive use
# You should download the ConsumptiveOnly folder to get the input files for this first section

rm(list = ls())

setwd("D:/Subregions")
# Travis desktop:
setwd("D:/5_RPA/Demand model/Demand Results - Supplemental for Publication/ConsumptiveOnly")



library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)
library(choroplethr)
library(choroplethrMaps)

################## Read in consumptive use data (all sectors) ###############
# below are bringing in all water data from all sectors
# BELOW ARE ALSO CONSUMPTIVE USE ONLY
cnrm45ssp1 <- read.csv("./Projections_cnrm45/ssp1_all.csv")
cnrm85ssp2 <- read.csv("./Projections_cnrm85/ssp2_all.csv")
cnrm85ssp3 <- read.csv("./Projections_cnrm85/ssp3_all.csv")
cnrm85ssp5 <- read.csv("./Projections_cnrm85/ssp5_all.csv")
hadgem45ssp1 <- read.csv("./Projections_hadgem45/ssp1_all.csv")
hadgem85ssp2 <- read.csv("./Projections_hadgem85/ssp2_all.csv")
hadgem85ssp3 <- read.csv("./Projections_hadgem85/ssp3_all.csv")
hadgem85ssp5 <- read.csv("./Projections_hadgem85/ssp5_all.csv")
ipsl45ssp1 <- read.csv("./Projections_ipsl45/ssp1_all.csv")
ipsl85ssp2 <- read.csv("./Projections_ipsl85/ssp2_all.csv")
ipsl85ssp3 <- read.csv("./Projections_ipsl85/ssp3_all.csv")
ipsl85ssp5 <- read.csv("./Projections_ipsl85/ssp5_all.csv")
mri45ssp1 <- read.csv("./Projections_mri45/ssp1_all.csv")
mri85ssp2 <- read.csv("./Projections_mri85/ssp2_all.csv")
mri85ssp3 <- read.csv("./Projections_mri85/ssp3_all.csv")
mri85ssp5 <- read.csv("./Projections_mri85/ssp5_all.csv")
noresm45ssp1 <- read.csv("./Projections_noresm45/ssp1_all.csv")
noresm85ssp2 <- read.csv("./Projections_noresm85/ssp2_all.csv")
noresm85ssp3 <- read.csv("./Projections_noresm85/ssp3_all.csv")
noresm85ssp5 <- read.csv("./Projections_noresm85/ssp5_all.csv")

# 2015 data is same for all scenarios, so choose one to use as current
current <- cnrm45ssp1

current <- select(current, fips, sector, Y2015)
current[,3][is.na(current[,3])] <- 0

# Make one large data file?

cnrm45ssp1$model <- "cnrm45-1"
cnrm85ssp2$model <- "cnrm85-2"
cnrm85ssp3$model <- "cnrm85-3"
cnrm85ssp5$model <- "cnrm85-5"
hadgem45ssp1$model <- "hadgem45-1"
hadgem85ssp2$model <- "hadgem85-2"
hadgem85ssp3$model <- "hadgem85-3"
hadgem85ssp5$model <- "hadgem85-5"
ipsl45ssp1$model <- "ipsl45-1"
ipsl85ssp2$model <- "ipsl85-2"
ipsl85ssp3$model <- "ipsl85-3"
ipsl85ssp5$model <- "ipsl85-5"
mri45ssp1$model <- "mri45-1"
mri85ssp2$model <- "mri85-2"
mri85ssp3$model <- "mri85-3"
mri85ssp5$model <- "mri85-5"
noresm45ssp1$model <- "noresm45-1"
noresm85ssp2$model <- "noresm85-2"
noresm85ssp3$model <- "noresm85-3"
noresm85ssp5$model <- "noresm85-5"

df <- rbind(cnrm45ssp1,
            cnrm85ssp2,
            cnrm85ssp3,
            cnrm85ssp5,
            hadgem45ssp1,
            hadgem85ssp2,
            hadgem85ssp3,
            hadgem85ssp5,
            ipsl45ssp1,
            ipsl85ssp2,
            ipsl85ssp3,
            ipsl85ssp5,
            mri45ssp1,
            mri85ssp2,
            mri85ssp3,
            mri85ssp5,
            noresm45ssp1,
            noresm85ssp2,
            noresm85ssp3,
            noresm85ssp5
)
            


df$Y2015[is.na(df$Y2015)] <- 0
df$Y2070[is.na(df$Y2070)] <- 0

df$change <- (df$Y2070 / df$Y2015) - 1

# national totals by model
df2 <- select(df, sector, Y2015, Y2070, model)
df3 <- df2[!(is.na(df2$sector) | df2$sector==""),] 
df3 <- select(df3, Y2015, Y2070, model)
df3 <- df3 %>% group_by(model) %>% summarise_each(funs(sum, sd))
df3$change <- (df3$Y2070_sum / df3$Y2015_sum) - 1

# sector results

dp <- subset(df, sector=='dp')
dp2 <- select(dp, fips, model, change)
dp2 <- dp2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))
dp.tot <- select(dp, model, Y2015, Y2070)
dp.tot <- dp.tot %>% group_by(model) %>% summarise_each(funs(sum, sd))

ic <- subset(df, sector=='ic')
ic2 <- select(ic, fips, model, change)
ic2 <- ic2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

ir <- subset(df, sector=='ir')
ir2 <- select(ir, fips, model, change)
ir2 <- ir2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

th <- subset(df, sector=='th')
th2 <- select(th, fips, model, change)
th2 <- th2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

## -- missing ls and aq

setwd("D:/5_RPA/Demand model/Demand Results - Supplemental for Publication/Core Scenarios")

cnrm45.1.ls <- read.csv("./RCP 4.5 SSP 1/CNRM-CM5/ls_ssp1.csv")
cnrm45.1.aq <- read.csv("./RCP 4.5 SSP 1/CNRM-CM5/aq_ssp1.csv")
hadgem45.1.ls <- read.csv("./RCP 4.5 SSP 1/hadGEM2-ES365/ls_ssp1.csv")
hadgem45.1.aq <- read.csv("./RCP 4.5 SSP 1/hadGEM2-ES365/aq_ssp1.csv")
ipsl45.1.ls <- read.csv("./RCP 4.5 SSP 1/IPSL-CM5A-M4/ls_ssp1.csv")
ipsl45.1.aq <- read.csv("./RCP 4.5 SSP 1/IPSL-CM5A-M4/aq_ssp1.csv")
mri45.1.ls <- read.csv("./RCP 4.5 SSP 1/MRI-CGCM3/ls_ssp1.csv")
mri45.1.aq <- read.csv("./RCP 4.5 SSP 1/MRI-CGCM3/aq_ssp1.csv")
noresm45.1.ls <- read.csv("./RCP 4.5 SSP 1/NorESM1-M/ls_ssp1.csv")
noresm45.1.aq <- read.csv("./RCP 4.5 SSP 1/NorESM1-M/aq_ssp1.csv")

cnrm85.2.ls <- read.csv("./RCP 8.5 SSP 2/CNRM-CM5/ls_ssp2.csv")
cnrm85.2.aq <- read.csv("./RCP 8.5 SSP 2/CNRM-CM5/aq_ssp2.csv")
hadgem85.2.ls <- read.csv("./RCP 8.5 SSP 2/hadGEM2-ES365/ls_ssp2.csv")
hadgem85.2.aq <- read.csv("./RCP 8.5 SSP 2/hadGEM2-ES365/aq_ssp2.csv")
ipsl85.2.ls <- read.csv("./RCP 8.5 SSP 2/IPSL-CM5A-M4/ls_ssp2.csv")
ipsl85.2.aq <- read.csv("./RCP 8.5 SSP 2/IPSL-CM5A-M4/aq_ssp2.csv")
mri85.2.ls <- read.csv("./RCP 8.5 SSP 2/MRI-CGCM3/ls_ssp2.csv")
mri85.2.aq <- read.csv("./RCP 8.5 SSP 2/MRI-CGCM3/aq_ssp2.csv")
noresm85.2.ls <- read.csv("./RCP 8.5 SSP 2/NorESM1-M/ls_ssp2.csv")
noresm85.2.aq <- read.csv("./RCP 8.5 SSP 2/NorESM1-M/aq_ssp2.csv")

cnrm85.3.ls <- read.csv("./RCP 8.5 SSP 3/CNRM-CM5/ls_ssp3.csv")
cnrm85.3.aq <- read.csv("./RCP 8.5 SSP 3/CNRM-CM5/aq_ssp3.csv")
hadgem85.3.ls <- read.csv("./RCP 8.5 SSP 3/hadGEM2-ES365/ls_ssp3.csv")
hadgem85.3.aq <- read.csv("./RCP 8.5 SSP 3/hadGEM2-ES365/aq_ssp3.csv")
ipsl85.3.ls <- read.csv("./RCP 8.5 SSP 3/IPSL-CM5A-M4/ls_ssp3.csv")
ipsl85.3.aq <- read.csv("./RCP 8.5 SSP 3/IPSL-CM5A-M4/aq_ssp3.csv")
mri85.3.ls <- read.csv("./RCP 8.5 SSP 3/MRI-CGCM3/ls_ssp3.csv")
mri85.3.aq <- read.csv("./RCP 8.5 SSP 3/MRI-CGCM3/aq_ssp3.csv")
noresm85.3.ls <- read.csv("./RCP 8.5 SSP 3/NorESM1-M/ls_ssp3.csv")
noresm85.3.aq <- read.csv("./RCP 8.5 SSP 3/NorESM1-M/aq_ssp3.csv")

cnrm85.5.ls <- read.csv("./RCP 8.5 SSP 5/CNRM-CM5/ls_ssp5.csv")
cnrm85.5.aq <- read.csv("./RCP 8.5 SSP 5/CNRM-CM5/aq_ssp5.csv")
hadgem85.5.ls <- read.csv("./RCP 8.5 SSP 5/hadGEM2-ES365/ls_ssp5.csv")
hadgem85.5.aq <- read.csv("./RCP 8.5 SSP 5/hadGEM2-ES365/aq_ssp5.csv")
ipsl85.5.ls <- read.csv("./RCP 8.5 SSP 5/IPSL-CM5A-M4/ls_ssp5.csv")
ipsl85.5.aq <- read.csv("./RCP 8.5 SSP 5/IPSL-CM5A-M4/aq_ssp5.csv")
mri85.5.ls <- read.csv("./RCP 8.5 SSP 5/MRI-CGCM3/ls_ssp5.csv")
mri85.5.aq <- read.csv("./RCP 8.5 SSP 5/MRI-CGCM3/aq_ssp5.csv")
noresm85.5.ls <- read.csv("./RCP 8.5 SSP 5/NorESM1-M/ls_ssp5.csv")
noresm85.5.aq <- read.csv("./RCP 8.5 SSP 5/NorESM1-M/aq_ssp5.csv")

cnrm45.1.aq$model <- 'cnrm45.1'
cnrm45.1.ls$model <- 'cnrm45.1'
cnrm45.1 <- rbind(cnrm45.1.aq, cnrm45.1.ls)

hadgem45.1.ls$model <- 'hadgem45.1'
hadgem45.1.aq$model <- 'hadgem45.1'
hadgem45.1 <- rbind(hadgem45.1.aq, hadgem45.1.ls)

ipsl45.1.ls$model <- 'ipsl45.1'
ipsl45.1.aq$model <- 'ipsl45.1'
ipsl45.1 <- rbind(ipsl45.1.aq, ipsl45.1.ls)

mri45.1.ls$model <- 'mri45.1'
mri45.1.aq$model <- 'mri45.1'
mri45.1 <- rbind(mri45.1.aq, mri45.1.ls)

noresm45.1.ls$model <- 'noresm45.1'
noresm45.1.aq$model <- 'noresm45.1'
noresm45.1 <- rbind(noresm45.1.aq, noresm45.1.ls)

cnrm85.2.ls$model <- 'cnrm85.2'
cnrm85.2.aq$model <- 'cnrm85.2'
cnrm85.2 <- rbind(cnrm85.2.aq, cnrm85.2.ls)

hadgem85.2.ls$model <- 'hadgem85.2'
hadgem85.2.aq$model <- 'hadgem85.2'
hadgem85.2 <- rbind(hadgem85.2.aq, hadgem85.2.ls)

ipsl85.2.ls$model <- 'ipsl85.2'
ipsl85.2.aq$model <- 'ipsl85.2'
ipsl85.2 <- rbind(ipsl85.2.aq, ipsl85.2.ls)

mri85.2.ls$model <- 'mri85.2'
mri85.2.aq$model <- 'mri85.2'
mri85.2 <- rbind(mri85.2.aq, mri85.2.ls)

noresm85.2.ls$model <- 'noresm85.2'
noresm85.2.aq$model <- 'noresm85.2'
noresm85.2 <- rbind(noresm85.2.aq, noresm85.2.ls)

cnrm85.3.ls$model <- 'cnrm85.3'
cnrm85.3.aq$model <- 'cnrm85.3'
cnrm85.3 <- rbind(cnrm85.3.aq, cnrm85.3.ls)

hadgem85.3.ls$model <- 'hadgem85.3'
hadgem85.3.aq$model <- 'hadgem85.3'
hadgem85.3 <- rbind(hadgem85.3.aq, hadgem85.3.ls)

ipsl85.3.ls$model <- 'ipsl85.3'
ipsl85.3.aq$model <- 'ipsl85.3'
ipsl85.3 <- rbind(ipsl85.3.aq, ipsl85.3.ls)

mri85.3.ls$model <- 'mri85.3'
mri85.3.aq$model <- 'mri85.3'
mri85.3 <- rbind(mri85.3.aq, mri85.3.ls)

noresm85.3.ls$model <- 'noresm85.3'
noresm85.3.aq$model <- 'noresm85.3'
noresm85.3 <- rbind(noresm85.3.aq, noresm85.3.ls)

cnrm85.5.ls$model <- 'cnrm85.5'
cnrm85.5.aq$model <- 'cnrm85.5'
cnrm85.5 <- rbind(cnrm85.5.aq, cnrm85.5.ls)

hadgem85.5.ls$model <- 'hadgem85.5'
hadgem85.5.aq$model <- 'hadgem85.5'
hadgem85.5 <- rbind(hadgem85.5.aq, hadgem85.5.ls)

ipsl85.5.ls$model <- 'ipsl85.5'
ipsl85.5.aq$model <- 'ipsl85.5'
ipsl85.5 <- rbind(ipsl85.5.aq, ipsl85.5.ls)

mri85.5.ls$model <- 'mri85.5'
mri85.5.aq$model <- 'mri85.5'
mri85.5 <- rbind(mri85.5.aq, mri85.5.ls)

noresm85.5.ls$model <- 'noresm85.5'
noresm85.5.aq$model <- 'noresm85.5'
noresm85.5 <- rbind(noresm85.5.aq, noresm85.5.ls)

cnrm45.1 <- cnrm45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem45.1 <- hadgem45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl45.1 <- ipsl45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri45.1 <- mri45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm45.1 <- noresm45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.2 <- cnrm85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.2 <- hadgem85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.2 <- ipsl85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.2 <- mri85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.2 <- noresm85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.3 <- cnrm85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.3 <- hadgem85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.3 <- ipsl85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.3 <- mri85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.3 <- noresm85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.5 <- cnrm85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.5 <- hadgem85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.5 <- ipsl85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.5 <- mri85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.5 <- noresm85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

# no climate feedbacks in la and aq - above could have been done just for ssp

cnrm45.1$model <- "cnrm45-1"
cnrm85.2$model <- "cnrm85-2"
cnrm85.3$model <- "cnrm85-3"
cnrm85.5$model <- "cnrm85-5"
  
all.la <- rbind(cnrm45.1,
                cnrm85.2,
                cnrm85.3,
                cnrm85.5)
all.la$change <- (all.la$Y2070/all.la$Y2015)-1

all.la2 <- all.la %>% group_by(fips) %>% summarise_each(funs(mean, sd))

dp$dp15 <- dp$Y2015
dp$dp70 <- dp$Y2070

ic$ic15 <- ic$Y2015
ic$ic70 <- ic$Y2070

ir$ir15 <- ir$Y2015
ir$ir70 <- ir$Y2070

th$th15 <- th$Y2015
th$th70 <- th$Y2070

all.la$la15 <- all.la$Y2015
all.la$la70 <- all.la$Y2070

dp <- select(dp, fips, dp15, dp70, model)
ic <- select(ic, fips, ic15, ic70, model)
ir <- select(ir, fips, ir15, ir70, model)
th <- select(th, fips, th15, th70, model)
la <- select(all.la, fips, la15, la70, model)
la$fips <-as.character(la$fips)

# mean change by model
sector.mean <- df %>% group_by(sector,fips) %>% summarise_each(funs(mean, sd))
sector.mean <- select(sector.mean, sector, fips, change_mean)

joined <- left_join(dp, ir, by = c('fips','model'))
joined <- left_join(joined, ic, by = c('fips','model'))
joined <- left_join(joined, th, by = c('fips','model'))
joined <- left_join(joined, la, by = c('fips','model'))

joined[is.na(joined)]<-0
joined$total15 <- joined$dp15+joined$ic15+joined$ir15+joined$th15+joined$la15
joined$total70 <- joined$dp70+joined$ic70+joined$ir70+joined$th70+joined$la70

joined$change <- (joined$total70/joined$total15)-1
joined <- joined %>% group_by(fips) %>% summarise_each(funs(mean, sd))

# mapping
# install.packages(c("choroplethr", "choroplethrMaps")) 

# livestock - aquaculture figures
all.la2$region <- all.la2$fips
all.la2$value <- all.la2$change_mean
county_choropleth(all.la2, num_colors = 0)

all.la2$value <- all.la2$change_sd
county_choropleth(all.la2, num_colors = 0)

# dp figures
dp2$region <- as.double(dp2$fips)
dp2$value <- dp2$change_mean
county_choropleth(dp2, num_colors = 0)

dp2$value <- dp2$change_sd
county_choropleth(dp2, num_colors = 0)

# ic figures
ic2$region <- as.double(ic2$fips)
ic2$value <- ic2$change_mean
ic2$value[is.na(ic2$value)] <- 0
county_choropleth(ic2, num_colors = 0)

ic2$value <- ic2$change_sd
ic2$value[is.na(ic2$value)] <- 0
county_choropleth(ic2, num_colors = 0)

# ir2 figures
ir2$region <- as.double(ir2$fips)
ir2$value <- ir2$change_mean
ir2$value[is.na(ir2$value)] <- 0
county_choropleth(ir2, num_colors = 0)

ir2$value <- ir2$change_sd
ir2$value[is.na(ir2$value)] <- 0
county_choropleth(ir2, num_colors = 0)

# th figures
th2$region <- as.double(th2$fips)
th2$value <- th2$change_mean
th2$value[is.na(th2$value)] <- 0
county_choropleth(th2, num_colors = 0)

th2$value <- th2$change_sd
th2$value[is.na(th2$value)] <- 0
county_choropleth(th2, num_colors = 0)

joined$region <- as.double(joined$fips)
joined$value <- joined$change_mean
county_choropleth(joined, num_colors = 0)

joined$region <- as.double(joined$fips)
joined$value <- joined$change_sd
county_choropleth(joined, num_colors = 0)

#--------------------------------

# # below is just arbitrarily testing which industries have high use
# great50 <- filter(ipsl85ssp5, Y2070 > 50)%>%
#   group_by(sector) 
 
# great50 %>% 
#   group_by(sector) %>%
#   summarise(no_rows = length(sector))

##### Read in subregion data #####

setwd("D:/Demand model/Demand Results - Supplemental for Publication/Subregions")
subregions <- read.csv("Counties_Subregions.csv")%>%
  select(GEOID, RPA_SubReg)
names(subregions) <- c("fips","subregion")

# sub-regional mean changes by sector

df.tot <- select(df, fips, sector, model, Y2015, Y2070)
sector.region <- merge(df.tot, subregions, by="fips")
sector.region <- select(sector.region, subregion, sector, model, Y2015, Y2070)
sector.nation <- sector.region

sector.region <- sector.region %>%
  group_by(subregion, sector, model) %>%
  summarise_each(funs(sum))

sector.region$change <- (sector.region$Y2070 / sector.region$Y2015) - 1

sector.region <- sector.region %>%
  group_by(subregion, sector) %>%
  summarise_each(funs(mean))

sector.nation <- select(sector.nation, sector, model, Y2015, Y2070)
sector.nation <- sector.nation %>%
  group_by(model, sector) %>%
  summarise_each(funs(sum))
sector.nation$change <- (sector.nation$Y2070 / sector.nation$Y2015) - 1
sector.nation <- sector.nation %>%
  group_by(sector) %>%
  summarise_each(funs(mean))

# -----------------------------------------------------------.
##### Current use #####
# first we can calculate the current use by subregion (year 2015)
# just pulling this for one of the datasets since it is the same for everything

# calculating current use - first merge with subregions
current <- merge(cnrm45ssp1, subregions, by = "fips")

current[, 4:59][is.na(current[, 4:59])] <- 0
current$Current <- current$Y2015
current <- current %>%
  group_by(subregion) %>%
  summarise(Current = sum(Current))
names(current)[2] <- "Current"

# -----------------------------------------------------------.
##### 2070 use #####
# second we find total withdrawals by subregion for the year 2070 for each climate model

##### cnrm climate model: #####
cnrm45ssp1 <- merge(cnrm45ssp1, subregions, by = "fips")
cnrm85ssp2 <- merge(cnrm85ssp2, subregions, by = "fips")
cnrm85ssp3 <- merge(cnrm85ssp3, subregions, by = "fips")
cnrm85ssp5 <- merge(cnrm85ssp5, subregions, by = "fips")
# 
cnrm45ssp1[, 4:59][is.na(cnrm45ssp1[, 4:59])] <- 0
cnrm45ssp1$Total <- rowSums(cnrm45ssp1[,c(4:59)])

cnrm45ssp1 <- cnrm45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(cnrm45ssp1)[2] <- "cnrm45ssp1"

cnrm85ssp2[, 4:59][is.na(cnrm85ssp2[, 4:59])] <- 0
cnrm85ssp2$Total <- rowSums(cnrm85ssp2[,c(4:59)])
cnrm85ssp2 <- cnrm85ssp2 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp2)[2] <- "cnrm85ssp2"
 
 # ignore below idk maybe itll come in handy at some point
 # imwest <- cnrm85ssp2 %>%
 #   filter(subregion == "Inter-mountain")#%>%
 #   #summarise(Total = sum(Total))
 # # total 1174495 for southwest
 # # total 1374838 for im west
 # ag <- filter(imwest, sector == "ir")%>%
 #   summarise(Total = sum(Total))
 # # total 1119036 for ag in southwest
 # # total 1269291 for ag in im west
 # 
 # (1269291 / 1374838)*100
 # 

 cnrm85ssp3[, 4:59][is.na(cnrm85ssp3[, 4:59])] <- 0
# cnrm85ssp3$Total <- rowSums(cnrm85ssp3[,c(4:59)])
 cnrm85ssp3 <- cnrm85ssp3 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp3)[2] <- "cnrm85ssp3"
#
 cnrm85ssp5[, 4:59][is.na(cnrm85ssp5[, 4:59])] <- 0
# cnrm85ssp5$Total <- rowSums(cnrm85ssp5[,c(4:59)])
 cnrm85ssp5 <- cnrm85ssp5 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp5)[2] <- "cnrm85ssp5"
 
# merge all the data for the climate scenario
cnrm <- merge(cnrm45ssp1, cnrm85ssp2)
cnrm <- merge(cnrm, cnrm85ssp3)
cnrm <- merge(cnrm, cnrm85ssp5)
cnrm <- merge(cnrm, current)
#write.csv(cnrm, file = "C:/ConsumptiveOnly/cnrm_subregions.csv")

# -----------------------------------------------------------.
##### hadgem climate model: #####
hadgem45ssp1 <- merge(hadgem45ssp1, subregions, by = "fips")
hadgem85ssp2 <- merge(hadgem85ssp2, subregions, by = "fips")
hadgem85ssp3 <- merge(hadgem85ssp3, subregions, by = "fips")
hadgem85ssp5 <- merge(hadgem85ssp5, subregions, by = "fips")

hadgem45ssp1[, 4:59][is.na(hadgem45ssp1[, 4:59])] <- 0
#hadgem45ssp1$Total <- rowSums(hadgem45ssp1[,c(4:59)])
hadgem45ssp1 <- hadgem45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem45ssp1)[2] <- "hadgem45ssp1"

hadgem85ssp2[, 4:59][is.na(hadgem85ssp2[, 4:59])] <- 0
#hadgem85ssp2$Total <- rowSums(hadgem85ssp2[,c(4:59)])
hadgem85ssp2 <- hadgem85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp2)[2] <- "hadgem85ssp2"

hadgem85ssp3[, 4:59][is.na(hadgem85ssp3[, 4:59])] <- 0
#hadgem85ssp3$Total <- rowSums(hadgem85ssp3[,c(4:59)])
hadgem85ssp3 <- hadgem85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp3)[2] <- "hadgem85ssp3"

hadgem85ssp5[, 4:59][is.na(hadgem85ssp5[, 4:59])] <- 0
#hadgem85ssp5$Total <- rowSums(hadgem85ssp5[,c(4:59)])
hadgem85ssp5 <- hadgem85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp5)[2] <- "hadgem85ssp5"

# merge all the data for the climate scenario
hadgem <- merge(hadgem45ssp1, hadgem85ssp2)
hadgem <- merge(hadgem, hadgem85ssp3)
hadgem <- merge(hadgem, hadgem85ssp5)
hadgem <- merge(hadgem, current)
#write.csv(hadgem, file = "C:/ConsumptiveOnly/hadgem_subregions.csv")

# -----------------------------------------------------------.
##### ipsl climate model: #####
ipsl45ssp1 <- merge(ipsl45ssp1, subregions, by = "fips")
ipsl85ssp2 <- merge(ipsl85ssp2, subregions, by = "fips")
ipsl85ssp3 <- merge(ipsl85ssp3, subregions, by = "fips")
ipsl85ssp5 <- merge(ipsl85ssp5, subregions, by = "fips")

ipsl45ssp1[, 4:59][is.na(ipsl45ssp1[, 4:59])] <- 0
#ipsl45ssp1$Total <- rowSums(ipsl45ssp1[,c(4:59)])
ipsl45ssp1 <- ipsl45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl45ssp1)[2] <- "ipsl45ssp1"

ipsl85ssp2[, 4:59][is.na(ipsl85ssp2[, 4:59])] <- 0
ipsl85ssp2$Total <- rowSums(ipsl85ssp2[,c(4:59)])
ipsl85ssp2 <- ipsl85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp2)[2] <- "ipsl85ssp2"

# some random computing ignore below
# southwest <- ipsl85ssp2 %>%
#   filter(subregion == "Pacific Southwest")%>%
# summarise(Total = sum(Total))
# # total 371825.2 for southwest
# ag <- filter(southwest, sector == "ir")%>%
#   summarise(Total = sum(Total))
# # total 325915.2 for ag in southwest

# (325915.2 / 371825.2)*100

ipsl85ssp3[, 4:59][is.na(ipsl85ssp3[, 4:59])] <- 0
#ipsl85ssp3$Total <- rowSums(ipsl85ssp3[,c(4:59)])
ipsl85ssp3 <- ipsl85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp3)[2] <- "ipsl85ssp3"

ipsl85ssp5[, 4:59][is.na(ipsl85ssp5[, 4:59])] <- 0
#ipsl85ssp5$Total <- rowSums(ipsl85ssp5[,c(4:59)])
ipsl85ssp5 <- ipsl85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp5)[2] <- "ipsl85ssp5"

# merge all the data for the climate scenario
ipsl <- merge(ipsl45ssp1, ipsl85ssp2)
ipsl <- merge(ipsl, ipsl85ssp3)
ipsl <- merge(ipsl, ipsl85ssp5)
ipsl <- merge(ipsl, current)
#write.csv(ipsl, file = "C:/ConsumptiveOnly/ipsl_subregions.csv")

# -----------------------------------------------------------.
##### mri climate model: #####
mri45ssp1 <- merge(mri45ssp1, subregions, by = "fips")
mri85ssp2 <- merge(mri85ssp2, subregions, by = "fips")
mri85ssp3 <- merge(mri85ssp3, subregions, by = "fips")
mri85ssp5 <- merge(mri85ssp5, subregions, by = "fips")

mri45ssp1[, 4:59][is.na(mri45ssp1[, 4:59])] <- 0
#mri45ssp1$Total <- rowSums(mri45ssp1[,c(4:59)])
mri45ssp1 <- mri45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri45ssp1)[2] <- "mri45ssp1"

mri85ssp2[, 4:59][is.na(mri85ssp2[, 4:59])] <- 0
#mri85ssp2$Total <- rowSums(mri85ssp2[,c(4:59)])
mri85ssp2 <- mri85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp2)[2] <- "mri85ssp2"

mri85ssp3[, 4:59][is.na(mri85ssp3[, 4:59])] <- 0
#mri85ssp3$Total <- rowSums(mri85ssp3[,c(4:59)])
mri85ssp3 <- mri85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp3)[2] <- "mri85ssp3"

mri85ssp5[, 4:59][is.na(mri85ssp5[, 4:59])] <- 0
#mri85ssp5$Total <- rowSums(mri85ssp5[,c(4:59)])
mri85ssp5 <- mri85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp5)[2] <- "mri85ssp5"

# merge all the data for the climate scenario
mri <- merge(mri45ssp1, mri85ssp2)
mri <- merge(mri, mri85ssp3)
mri <- merge(mri, mri85ssp5)
mri <- merge(mri, current)
#write.csv(mri, file = "C:/ConsumptiveOnly/mri_subregions.csv")

# -----------------------------------------------------------.
##### noresm climate model: #####
noresm45ssp1 <- merge(noresm45ssp1, subregions, by = "fips")
noresm85ssp2 <- merge(noresm85ssp2, subregions, by = "fips")
noresm85ssp3 <- merge(noresm85ssp3, subregions, by = "fips")
noresm85ssp5 <- merge(noresm85ssp5, subregions, by = "fips")

noresm45ssp1[, 4:59][is.na(noresm45ssp1[, 4:59])] <- 0
#noresm45ssp1$Total <- rowSums(noresm45ssp1[,c(4:59)])
noresm45ssp1 <- noresm45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm45ssp1)[2] <- "noresm45ssp1"

noresm85ssp2[, 4:59][is.na(noresm85ssp2[, 4:59])] <- 0
#noresm85ssp2$Total <- rowSums(noresm85ssp2[,c(4:59)])
noresm85ssp2 <- noresm85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp2)[2] <- "noresm85ssp2"

noresm85ssp3[, 4:59][is.na(noresm85ssp3[, 4:59])] <- 0
#noresm85ssp3$Total <- rowSums(noresm85ssp3[,c(4:59)])
noresm85ssp3 <- noresm85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp3)[2] <- "noresm85ssp3"

noresm85ssp5[, 4:59][is.na(noresm85ssp5[, 4:59])] <- 0
#noresm85ssp5$Total <- rowSums(noresm85ssp5[,c(4:59)])
noresm85ssp5 <- noresm85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp5)[2] <- "noresm85ssp5"

# merge all the data for the climate scenario
noresm <- merge(noresm45ssp1, noresm85ssp2)
noresm <- merge(noresm, cnrm85ssp3)
noresm <- merge(noresm, cnrm85ssp5)
noresm <- merge(noresm, current)
#write.csv(noresm, file = "C:/ConsumptiveOnly/noresm_subregions.csv")



# -----------------------------------------------------------.
# Below can be ignored, it just aggregated data by RCP / SSP instead of climate model

# HH <- merge(cnrm85ssp5, hadgem85ssp5)
# HH <- merge(HH,ipsl85ssp5)
# HH <- merge(HH,mri85ssp5)
# HH <- merge(HH, noresm85ssp5)
# #write.csv(HH, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HH.csv")
# 
# HM <- merge(cnrm85ssp2, hadgem85ssp2)
# HM <- merge(HM,ipsl85ssp2)
# HM <- merge(HM,mri85ssp2)
# HM <- merge(HM, noresm85ssp2)
# #write.csv(HM, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HM.csv")
# 
# HL <- merge(cnrm85ssp3, hadgem85ssp3)
# HL <- merge(HL,ipsl85ssp3)
# HL <- merge(HL,mri85ssp3)
# HL <- merge(HL, noresm85ssp3)
# #write.csv(HL, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HL.csv")
# 
# LM <- merge(cnrm45ssp1, hadgem45ssp1)
# LM <- merge(LM,ipsl45ssp1)
# LM <- merge(LM,mri45ssp1)
# LM <- merge(LM, noresm45ssp1)
# #write.csv(LM, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/LM.csv")

# ----------------------------------
# Sector calc for consumptive use
# ----------------------------------

# calculating current use - first merge with subregions
dp.sub <- merge(dp, subregions, by = "fips")

dp.sub.mri45.1 <- subset(dp.sub, dp.sub$model == "mri45-1")
dp.sub.mri45.1 <- dp.sub.mri45.1 %>%
  group_by(subregion) %>%
  summarise(
    dp15 = sum(dp15),
    dp70 = sum(dp70))


# ----------------------------------------------------------------.
##### livestock and aquaculture #####

# livestock and aquaculture are being combined into one sector
# these two sectors do not change based on climate - only SSP

livestock1 <- read.csv("./Projections_noresm45/ls_ssp1.csv")
livestock2 <- read.csv("./Projections_noresm85/ls_ssp2.csv")
livestock3 <- read.csv("./Projections_noresm85/ls_ssp3.csv")
livestock5 <- read.csv("./Projections_noresm85/ls_ssp5.csv")

aqua1 <- read.csv("./Projections_noresm45/aq_ssp1.csv")
aqua2 <- read.csv("./Projections_noresm85/aq_ssp2.csv")
aqua3 <- read.csv("./Projections_noresm85/aq_ssp3.csv")
aqua5 <- read.csv("./Projections_noresm85/aq_ssp5.csv")

fips <- select(aqua1, fips)
aqls1 <- aqua1[,4:59] + livestock1[,4:59]
aqls1$sector <- "LsAq"
aqls1 <- aqls1 %>%
  select(sector, everything())
aqls1 <- cbind(fips, aqls1)

aqls2 <- aqua2[,4:59] + livestock2[,4:59]
aqls2$sector <- "LsAq"
aqls2 <- aqls2 %>%
  select(sector, everything())
aqls2 <- cbind(fips, aqls2)

aqls3 <- aqua3[,4:59] + livestock3[,4:59]
aqls3$sector <- "LsAq"
aqls3 <- aqls3 %>%
  select(sector, everything())
aqls3 <- cbind(fips, aqls3)

aqls5 <- aqua5[,4:59] + livestock5[,4:59]
aqls5$sector <- "LsAq"
aqls5 <- aqls5 %>%
  select(sector, everything())
aqls5 <- cbind(fips, aqls5)

aqlsssp1 <- merge(aqls1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(aqlsssp1) <- c("subregion", "Y2015","Y2070ssp1")
aqlsssp2 <- merge(aqls2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp2) <- c("subregion", "Y2070ssp2")
aqlsssp3 <- merge(aqls3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp3) <- c("subregion", "Y2070ssp3")
aqlsssp5 <- merge(aqls5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp5) <- c("subregion", "Y2070ssp5")

aqlstot <- merge(aqlsssp1, aqlsssp2, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp3, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp5, by = "subregion")
#write.csv(aqlstot, file = "/Sector Totals/aqLs.csv")

# -----------------------------------------------------------.

##### thermo #####

# noresm
thermo1 <- read.csv("./Projections_Noresm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_Noresm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_Noresm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_Noresm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_noresm.csv")



# cnrm
thermo1 <- read.csv("./Projections_cnrm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_cnrm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_cnrm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_cnrm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_cnrm.csv")

# hadgem
thermo1 <- read.csv("./Projections_hadgem45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_hadgem85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_hadgem85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_hadgem85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_hadgem.csv")

# ipsl
thermo1 <- read.csv("./Projections_ipsl45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_ipsl85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_ipsl85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_ipsl85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_ipsl.csv")



# mri
thermo1 <- read.csv("./Projections_mri45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_mri85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_mri85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_mri85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_mri.csv")


# -----------------------------------------------------------.
##### Sector withdrawals #####
# Below is basically doing the same as above, but not using consumptive data and 
# separated by each sector instead of combining all sectors.

# in this part below, I am pulling from total withdrawal data (not consumptive use)
setwd("H:Subregions")

# withdrawal data seems to be by sector. There is no all data file

##### domestic #####

# noresm
domestic1n <- read.csv("./Projections_noresm45/dp_ssp1.csv")
domestic2n <- read.csv("./Projections_noresm85/dp_ssp2.csv")
domestic3n <- read.csv("./Projections_noresm85/dp_ssp3.csv")
domestic5n <- read.csv("./Projections_noresm85/dp_ssp5.csv")

domestic1n$model <- 'noresm45-1'
domestic2n$model <- "noresm85-2"
domestic3n$model <- "noresm85-3"
domestic5n$model <- "noresm85-5"

# cnrm
domestic1c <- read.csv("./Projections_cnrm45/dp_ssp1.csv")
domestic2c <- read.csv("./Projections_cnrm85/dp_ssp2.csv")
domestic3c <- read.csv("./Projections_cnrm85/dp_ssp3.csv")
domestic5c <- read.csv("./Projections_cnrm85/dp_ssp5.csv")

domestic1c$model <- 'cnrm45-1'
domestic2c$model <- "cnrm85-2"
domestic3c$model <- "cnrm85-3"
domestic5c$model <- "cnrm85-5"

# hadgem
domestic1h <- read.csv("./Projections_hadgem45/dp_ssp1.csv")
domestic2h <- read.csv("./Projections_hadgem85/dp_ssp2.csv")
domestic3h <- read.csv("./Projections_hadgem85/dp_ssp3.csv")
domestic5h <- read.csv("./Projections_hadgem85/dp_ssp5.csv")

domestic1h$model <- 'hadgem45-1'
domestic2h$model <- "hadgem85-2"
domestic3h$model <- "hadgem85-3"
domestic5h$model <- "hadgem85-5"

# ipsl
domestic1i <- read.csv("./Projections_ipsl45/dp_ssp1.csv")
domestic2i <- read.csv("./Projections_ipsl85/dp_ssp2.csv")
domestic3i <- read.csv("./Projections_ipsl85/dp_ssp3.csv")
domestic5i <- read.csv("./Projections_ipsl85/dp_ssp5.csv")

domestic1i$model <- 'ipsl45-1'
domestic2i$model <- "ipsl85-2"
domestic3i$model <- "ipsl85-3"
domestic5i$model <- "ipsl85-5"

# mri
domestic1m <- read.csv("./Projections_mri45/dp_ssp1.csv")
domestic2m <- read.csv("./Projections_mri85/dp_ssp2.csv")
domestic3m <- read.csv("./Projections_mri85/dp_ssp3.csv")
domestic5m <- read.csv("./Projections_mri85/dp_ssp5.csv")

domestic1m$model <- 'mri45-1'
domestic2m$model <- "mri85-2"
domestic3m$model <- "mri85-3"
domestic5m$model <- "mri85-5"

dp.wd <- rbind(
  domestic1n,
  domestic2n,
  domestic3n,
  domestic5n,
  domestic1c,
  domestic2c,
  domestic3c,
  domestic5c,
  domestic1h,
  domestic2h,
  domestic3h,
  domestic5h,
  domestic1i,
  domestic2i,
  domestic3i,
  domestic5i,
  domestic1m,
  domestic2m,
  domestic3m,
  domestic5m
  )

##### industrial #####

# using noresm BUT industrial does not change based on any of the climate scenarios

industrial1 <- read.csv("./Projections_noresm45/i_ssp1.csv")
industrial1[, 4:59][is.na(industrial1[, 4:59])] <- 0
industrial2 <- read.csv("./Projections_noresm85/i_ssp2.csv")
industrial2[, 4:59][is.na(industrial2[, 4:59])] <- 0
industrial3 <- read.csv("./Projections_noresm85/i_ssp3.csv")
industrial3[, 4:59][is.na(industrial3[, 4:59])] <- 0
industrial5 <- read.csv("./Projections_noresm85/i_ssp5.csv")
industrial5[, 4:59][is.na(industrial5[, 4:59])] <- 0

industrial1n <- industrial1
industrial1c <- industrial1
industrial1h <- industrial1
industrial1i <- industrial1
industrial1m <- industrial1

industrial2n <- industrial2
industrial2c <- industrial2
industrial2h <- industrial2
industrial2i <- industrial2
industrial2m <- industrial2

industrial3n <- industrial3
industrial3c <- industrial3
industrial3h <- industrial3
industrial3i <- industrial3
industrial3m <- industrial3

industrial5n <- industrial5
industrial5c <- industrial5
industrial5h <- industrial5
industrial5i <- industrial5
industrial5m <- industrial5

industrial1n$model <- 'noresm45-1'
industrial2n$model <- "noresm85-2"
industrial3n$model <- "noresm85-3"
industrial5n$model <- "noresm85-5"

industrial1c$model <- 'cnrm45-1'
industrial2c$model <- "cnrm85-2"
industrial3c$model <- "cnrm85-3"
industrial5c$model <- "cnrm85-5"

industrial1h$model <- 'hadgem45-1'
industrial2h$model <- "hadgem85-2"
industrial3h$model <- "hadgem85-3"
industrial5h$model <- "hadgem85-5"

industrial1i$model <- 'ipsl45-1'
industrial2i$model <- "ipsl85-2"
industrial3i$model <- "ipsl85-3"
industrial5i$model <- "ipsl85-5"

industrial1m$model <- 'mri45-1'
industrial2m$model <- "mri85-2"
industrial3m$model <- "mri85-3"
industrial5m$model <- "mri85-5"

ind.wd <- rbind(
  industrial1n,
  industrial2n,
  industrial3n,
  industrial5n,
  industrial1c,
  industrial2c,
  industrial3c,
  industrial5c,
  industrial1h,
  industrial2h,
  industrial3h,
  industrial5h,
  industrial1i,
  industrial2i,
  industrial3i,
  industrial5i,
  industrial1m,
  industrial2m,
  industrial3m,
  industrial5m
)

##### irrigation ##### 

# irrigation does not change based on SSP scenario, only climate & RCP

# noresm
irrigation45n<- read.csv("./Projections_noresm45/ir.csv")
irrigation85n <- read.csv("./Projections_noresm85/ir.csv")

# cnrm
irrigation45c<- read.csv("./Projections_cnrm45/ir.csv")
irrigation85c <- read.csv("./Projections_cnrm85/ir.csv")

# hadgem 
irrigation45h<- read.csv("./Projections_hadgem45/ir.csv")
irrigation85h <- read.csv("./Projections_hadgem85/ir.csv")

# ipsl 
irrigation45i<- read.csv("./Projections_ipsl45/ir.csv")
irrigation85i <- read.csv("./Projections_ipsl85/ir.csv")

# mri 
irrigation45m<- read.csv("./Projections_mri45/ir.csv")
irrigation85m <- read.csv("./Projections_mri85/ir.csv")

irrigation1n <- irrigation45n
irrigation1c <- irrigation45c
irrigation1h <- irrigation45h
irrigation1i <- irrigation45i
irrigation1m <- irrigation45m

irrigation2n <- irrigation85n
irrigation2c <- irrigation85c
irrigation2h <- irrigation85h
irrigation2i <- irrigation85i
irrigation2m <- irrigation85m

irrigation3n <- irrigation85n
irrigation3c <- irrigation85c
irrigation3h <- irrigation85h
irrigation3i <- irrigation85i
irrigation3m <- irrigation85m

irrigation5n <- irrigation85n
irrigation5c <- irrigation85c
irrigation5h <- irrigation85h
irrigation5i <- irrigation85i
irrigation5m <- irrigation85m

irrigation1n$model <- 'noresm45-1'
irrigation2n$model <- "noresm85-2"
irrigation3n$model <- "noresm85-3"
irrigation5n$model <- "noresm85-5"

irrigation1c$model <- 'cnrm45-1'
irrigation2c$model <- "cnrm85-2"
irrigation3c$model <- "cnrm85-3"
irrigation5c$model <- "cnrm85-5"

irrigation1h$model <- 'hadgem45-1'
irrigation2h$model <- "hadgem85-2"
irrigation3h$model <- "hadgem85-3"
irrigation5h$model <- "hadgem85-5"

irrigation1i$model <- 'ipsl45-1'
irrigation2i$model <- "ipsl85-2"
irrigation3i$model <- "ipsl85-3"
irrigation5i$model <- "ipsl85-5"

irrigation1m$model <- 'mri45-1'
irrigation2m$model <- "mri85-2"
irrigation3m$model <- "mri85-3"
irrigation5m$model <- "mri85-5"

irr.wd <- rbind(
  irrigation1n,
  irrigation2n,
  irrigation3n,
  irrigation5n,
  irrigation1c,
  irrigation2c,
  irrigation3c,
  irrigation5c,
  irrigation1h,
  irrigation2h,
  irrigation3h,
  irrigation5h,
  irrigation1i,
  irrigation2i,
  irrigation3i,
  irrigation5i,
  irrigation1m,
  irrigation2m,
  irrigation3m,
  irrigation5m
)

##### livestock and aquaculture #####

# livestock and aquaculture are being combined into one sector
# these two sectors do not change based on climate - only SSP

livestock1 <- read.csv("./Projections_noresm45/ls_ssp1.csv")
livestock2 <- read.csv("./Projections_noresm85/ls_ssp2.csv")
livestock3 <- read.csv("./Projections_noresm85/ls_ssp3.csv")
livestock5 <- read.csv("./Projections_noresm85/ls_ssp5.csv")

aqua1 <- read.csv("./Projections_noresm45/aq_ssp1.csv")
aqua2 <- read.csv("./Projections_noresm85/aq_ssp2.csv")
aqua3 <- read.csv("./Projections_noresm85/aq_ssp3.csv")
aqua5 <- read.csv("./Projections_noresm85/aq_ssp5.csv")

ls1 <- rbind(livestock1, aqua1)
ls1 <- subset(ls1, select = -c(sector))
ls1 <- ls1 %>% group_by(fips) %>% summarise_each(funs(sum))
ls1$sector <- 'ls'

ls2 <- rbind(livestock2, aqua2)
ls2 <- subset(ls2, select = -c(sector))
ls2 <- ls2 %>% group_by(fips) %>% summarise_each(funs(sum))
ls2$sector <- 'ls'

ls3 <- rbind(livestock3, aqua3)
ls3 <- subset(ls3, select = -c(sector))
ls3 <- ls3 %>% group_by(fips) %>% summarise_each(funs(sum))
ls3$sector <- 'ls'

ls5 <- rbind(livestock5, aqua5)
ls5 <- subset(ls5, select = -c(sector))
ls5 <- ls5 %>% group_by(fips) %>% summarise_each(funs(sum))
ls5$sector <- 'ls'

ls1n <- ls1
ls1c <- ls1
ls1h <- ls1
ls1i <- ls1
ls1m <- ls1

ls2n <- ls2
ls2c <- ls2
ls2h <- ls2
ls2i <- ls2
ls2m <- ls2

ls3n <- ls3
ls3c <- ls3
ls3h <- ls3
ls3i <- ls3
ls3m <- ls3

ls5n <- ls5
ls5c <- ls5
ls5h <- ls5
ls5i <- ls5
ls5m <- ls5

ls1n$model <- 'noresm45-1'
ls2n$model <- "noresm85-2"
ls3n$model <- "noresm85-3"
ls5n$model <- "noresm85-5"

ls1c$model <- 'cnrm45-1'
ls2c$model <- "cnrm85-2"
ls3c$model <- "cnrm85-3"
ls5c$model <- "cnrm85-5"

ls1h$model <- 'hadgem45-1'
ls2h$model <- "hadgem85-2"
ls3h$model <- "hadgem85-3"
ls5h$model <- "hadgem85-5"

ls1i$model <- 'ipsl45-1'
ls2i$model <- "ipsl85-2"
ls3i$model <- "ipsl85-3"
ls5i$model <- "ipsl85-5"

ls1m$model <- 'mri45-1'
ls2m$model <- "mri85-2"
ls3m$model <- "mri85-3"
ls5m$model <- "mri85-5"

ls.wd <- rbind(
  ls1n,
  ls2n,
  ls3n,
  ls5n,
  ls1c,
  ls2c,
  ls3c,
  ls5c,
  ls1h,
  ls2h,
  ls3h,
  ls5h,
  ls1i,
  ls2i,
  ls3i,
  ls5i,
  ls1m,
  ls2m,
  ls3m,
  ls5m
)

##### thermo #####

# noresm
thermo1n <- read.csv("./Projections_Noresm45/th_ssp1.csv")
thermo1n[, 4:59][is.na(thermo1n[, 4:59])] <- 0
thermo2n <- read.csv("./Projections_Noresm85/th_ssp2.csv")
thermo2n[, 4:59][is.na(thermo2n[, 4:59])] <- 0
thermo3n <- read.csv("./Projections_Noresm85/th_ssp3.csv")
thermo3n[, 4:59][is.na(thermo3n[, 4:59])] <- 0
thermo5n <- read.csv("./Projections_Noresm85/th_ssp5.csv")
thermo5n[, 4:59][is.na(thermo5n[, 4:59])] <- 0

# cnrm
thermo1c <- read.csv("./Projections_cnrm45/th_ssp1.csv")
thermo1c[, 4:59][is.na(thermo1c[, 4:59])] <- 0
thermo2c <- read.csv("./Projections_cnrm85/th_ssp2.csv")
thermo2c[, 4:59][is.na(thermo2c[, 4:59])] <- 0
thermo3c <- read.csv("./Projections_cnrm85/th_ssp3.csv")
thermo3c[, 4:59][is.na(thermo3c[, 4:59])] <- 0
thermo5c <- read.csv("./Projections_cnrm85/th_ssp5.csv")
thermo5c[, 4:59][is.na(thermo5c[, 4:59])] <- 0

# hadgem
thermo1h <- read.csv("./Projections_hadgem45/th_ssp1.csv")
thermo1h[, 4:59][is.na(thermo1h[, 4:59])] <- 0
thermo2h <- read.csv("./Projections_hadgem85/th_ssp2.csv")
thermo2h[, 4:59][is.na(thermo2h[, 4:59])] <- 0
thermo3h <- read.csv("./Projections_hadgem85/th_ssp3.csv")
thermo3h[, 4:59][is.na(thermo3h[, 4:59])] <- 0
thermo5h <- read.csv("./Projections_hadgem85/th_ssp5.csv")
thermo5h[, 4:59][is.na(thermo5h[, 4:59])] <- 0

# ipsl
thermo1i <- read.csv("./Projections_ipsl45/th_ssp1.csv")
thermo1i[, 4:59][is.na(thermo1i[, 4:59])] <- 0
thermo2i <- read.csv("./Projections_ipsl85/th_ssp2.csv")
thermo2i[, 4:59][is.na(thermo2i[, 4:59])] <- 0
thermo3i <- read.csv("./Projections_ipsl85/th_ssp3.csv")
thermo3i[, 4:59][is.na(thermo3i[, 4:59])] <- 0
thermo5i <- read.csv("./Projections_ipsl85/th_ssp5.csv")
thermo5i[, 4:59][is.na(thermo5i[, 4:59])] <- 0

# mri
thermo1m <- read.csv("./Projections_mri45/th_ssp1.csv")
thermo1m[, 4:59][is.na(thermo1m[, 4:59])] <- 0
thermo2m <- read.csv("./Projections_mri85/th_ssp2.csv")
thermo2m[, 4:59][is.na(thermo2m[, 4:59])] <- 0
thermo3m <- read.csv("./Projections_mri85/th_ssp3.csv")
thermo3m[, 4:59][is.na(thermo3m[, 4:59])] <- 0
thermo5m <- read.csv("./Projections_mri85/th_ssp5.csv")
thermo5m[, 4:59][is.na(thermo5m[, 4:59])] <- 0

thermo1n$model <- 'noresm45-1'
thermo2n$model <- "noresm85-2"
thermo3n$model <- "noresm85-3"
thermo5n$model <- "noresm85-5"

thermo1c$model <- 'cnrm45-1'
thermo2c$model <- "cnrm85-2"
thermo3c$model <- "cnrm85-3"
thermo5c$model <- "cnrm85-5"

thermo1h$model <- 'hadgem45-1'
thermo2h$model <- "hadgem85-2"
thermo3h$model <- "hadgem85-3"
thermo5h$model <- "hadgem85-5"

thermo1i$model <- 'ipsl45-1'
thermo2i$model <- "ipsl85-2"
thermo3i$model <- "ipsl85-3"
thermo5i$model <- "ipsl85-5"

thermo1m$model <- 'mri45-1'
thermo2m$model <- "mri85-2"
thermo3m$model <- "mri85-3"
thermo5m$model <- "mri85-5"

# get consistent column names

names(thermo1c) <- names(thermo2h)
names(thermo1h) <- names(thermo2h)
names(thermo1i) <- names(thermo2h)
names(thermo1m) <- names(thermo2h)
names(thermo1n) <- names(thermo2h)

names(thermo2c) <- names(thermo2h)
#names(thermo2h) <- names(thermo2h)
names(thermo2i) <- names(thermo2h)
names(thermo2m) <- names(thermo2h)
names(thermo2n) <- names(thermo2h)

names(thermo3c) <- names(thermo2h)
names(thermo3h) <- names(thermo2h)
names(thermo3i) <- names(thermo2h)
names(thermo3m) <- names(thermo2h)
names(thermo3n) <- names(thermo2h)

names(thermo5c) <- names(thermo2h)
names(thermo5h) <- names(thermo2h)
names(thermo5i) <- names(thermo2h)
names(thermo5m) <- names(thermo2h)
names(thermo5n) <- names(thermo2h)

thermo.wd <- rbind(
  thermo1n,
  thermo2n,
  thermo3n,
  thermo5n,
  thermo1c,
  thermo2c,
  thermo3c,
  thermo5c,
  thermo1h,
  thermo2h,
  thermo3h,
  thermo5h,
  thermo1i,
  thermo2i,
  thermo3i,
  thermo5i,
  thermo1m,
  thermo2m,
  thermo3m,
  thermo5m
)

thermo.wd$sector <- 'th'

withdrawals <- rbind(dp.wd, ind.wd, irr.wd, ls.wd, thermo.wd)

# national total withdrawals

wd.tot <- subset(withdrawals, select = c(fips, sector, model, Y2015, Y2070))
wd.tot.scenario <- subset(wd.tot, select=c(model,Y2015,Y2070))
wd.tot.scenario <- wd.tot.scenario %>% group_by(model) %>% summarise_each(funs(sum))
wd.tot.scenario$change <- (wd.tot.scenario$Y2070 / wd.tot.scenario$Y2015)-1
ls5$sector <- 'ls'

# need to fix FIPS codes

dp.wd[,2] <- ifelse(dp.wd[,2] == 51003, 51901, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51540, 51901, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51015, 51907, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51790, 51907, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51820, 51907, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51031, 51911, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51680, 51911, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51035, 51913, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51640, 51913, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51053, 51918, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51570, 51918, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51730, 51918, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51059, 51919, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51600, 51919, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51610, 51919, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51069, 51921, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51840, 51921, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51081, 51923, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51595, 51923, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51089, 51929, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51690, 51929, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51095, 51931, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51830, 51931, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51121, 51933, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51750, 51933, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51143, 51939, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51590, 51939, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51149, 51941, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51670, 51941, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51153, 51942, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51683, 51942, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51685, 51942, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51161, 51944, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51775, 51944, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51163, 51945, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51530, 51945, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51678, 51945, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51165, 51947, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51660, 51947, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51175, 51949, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51620, 51949, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51177, 51951, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51630, 51951, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51191, 51953, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51520, 51953, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51195, 51955, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51720, 51955, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51199, 51958, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51735, 51958, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51560, 51903, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51005, 51903, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51580, 51903, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51780, 51083, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51515, 51909, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51019, 51909, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 51149, 51941, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 12025, 12086, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 46102, 46113, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 55078, 55901, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 55115, 55901, dp.wd[,2])
#dp.wd[,2] <- ifelse(dp.wd[,2] == 08014, 08013, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 04012, 04027, dp.wd[,2])
dp.wd[,2] <- ifelse(dp.wd[,2] == 35006, 35061, dp.wd[,2])


# maps of mean change and standard deviation (note, percent change
# is same for withdrawals and consumptive use)

dp.wd$change <- (dp.wd$Y2070/dp.wd$Y2015) - 1
dp2 <- select(dp.wd, fips, model, change)
dp2 <- dp2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

dp2$region <- as.double(dp2$fips)
dp2$value <- dp2$change_mean
county_choropleth(dp2, num_colors = 0)

dp2$value <- dp2$change_sd
county_choropleth(dp2, num_colors = 0)

ind.wd$change <- (ind.wd$Y2070/ind.wd$Y2015) - 1
ind2 <- select(ind.wd, fips, model, change)
ind2 <- ind2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))
ind2$region <- as.double(ind2$fips)
ind2$value <- ind2$change_mean
county_choropleth(ind2, num_colors = 0)
ind2$value <- ind2$change_sd
county_choropleth(ind2, num_colors = 0)


irr.wd$change <- (irr.wd$Y2070/irr.wd$Y2015) - 1
irr2 <- select(irr.wd, fips, model, change)
irr2 <- irr2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))
irr2$region <- as.double(irr2$fips)
irr2$value <- irr2$change_mean
county_choropleth(irr2, num_colors = 0)
irr2$value <- irr2$change_sd
county_choropleth(irr2, num_colors = 0)

thermo.wd$change <- (thermo.wd$Y2070/thermo.wd$Y2015) - 1
thermo2 <- select(thermo.wd, fips, model, change)
thermo2 <- thermo2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))
thermo2$region <- as.double(thermo2$fips)
thermo2$value <- thermo2$change_mean
county_choropleth(thermo2, num_colors = 0)
thermo2$value <- thermo2$change_sd
county_choropleth(thermo2, num_colors = 0)

#############################################
# Group withdrawals by RPA subregion 
#############################################


##### domestic #####

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
  names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
  names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
  names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_noresm.csv")


domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_cnrm.csv")


domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_hadgem.csv")




domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_ipsl.csv")



domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_mri.csv")


#----------------------------------------------------------------------------------.

industrialssp1 <- merge(industrial1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(industrialssp1) <- c("subregion", "Y2015","Y2070ssp1")
industrialssp2 <- merge(industrial2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp2) <- c("subregion", "Y2070ssp2")
industrialssp3 <- merge(industrial3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp3) <- c("subregion", "Y2070ssp3")
industrialssp5 <- merge(industrial5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp5) <- c("subregion", "Y2070ssp5")

industrialtot <- merge(industrialssp1, industrialssp2, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp3, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp5, by = "subregion")
#write.csv(industrialtot, file = "/Sector Totals/industrial.csv")

# --------------------------------------------------------------------------------------------.

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_noresm.csv")



irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_cnrm.csv")




irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_hadgem.csv")




irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_ipsl.csv")




irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp3, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp5, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_mri.csv")



# ----------------------------------------------------------------.

fips <- select(aqua1, fips)
aqls1 <- aqua1[,4:59] + livestock1[,4:59]
aqls1$sector <- "LsAq"
aqls1 <- aqls1 %>%
  select(sector, everything())
aqls1 <- cbind(fips, aqls1)

aqls2 <- aqua2[,4:59] + livestock2[,4:59]
aqls2$sector <- "LsAq"
aqls2 <- aqls2 %>%
  select(sector, everything())
aqls2 <- cbind(fips, aqls2)

aqls3 <- aqua3[,4:59] + livestock3[,4:59]
aqls3$sector <- "LsAq"
aqls3 <- aqls3 %>%
  select(sector, everything())
aqls3 <- cbind(fips, aqls3)

aqls5 <- aqua5[,4:59] + livestock5[,4:59]
aqls5$sector <- "LsAq"
aqls5 <- aqls5 %>%
  select(sector, everything())
aqls5 <- cbind(fips, aqls5)

aqlsssp1 <- merge(aqls1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(aqlsssp1) <- c("subregion", "Y2015","Y2070ssp1")
aqlsssp2 <- merge(aqls2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp2) <- c("subregion", "Y2070ssp2")
aqlsssp3 <- merge(aqls3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp3) <- c("subregion", "Y2070ssp3")
aqlsssp5 <- merge(aqls5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp5) <- c("subregion", "Y2070ssp5")

aqlstot <- merge(aqlsssp1, aqlsssp2, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp3, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp5, by = "subregion")
#write.csv(aqlstot, file = "/Sector Totals/aqLs.csv")



# -----------------------------------------------------------.

