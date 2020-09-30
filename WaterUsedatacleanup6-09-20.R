rm(list = ls())
setwd("C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDataCleaning")



#import this library 
library(Hmisc)
library(memisc)
library(plyr)
library(dplyr)
library(magrittr)

#----------------------------------------------------------------------------------------------------------------------
#This file converts raw USGS water use data to a useable format. Output from this file feeds into WEAPinput final.R file.
#----------------------------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------
#FIPS codes need to match population/income projection FIPS codes. Virginia cities are merged with the surrounding county.
#Other minor changes with other states.
#----------------------------------------------------------------------------------------------------------------------


#fix VA FIPS codes
VA<-read.delim2("Virginia_water_use.txt", header=TRUE, na.strings = "-")
VA<-VA[,c(1,3,5:283)]
VA$county_cd <- as.numeric(as.character(VA$county_cd))

VA$county_cd <-formatC(VA$county_cd, width = 3, format = "d", flag = "0")
VA$FIPS <- paste(VA$state_cd, VA$county_cd, sep="")
VA$FIPS <- ifelse(VA$FIPS == 51003, 51901, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51540, 51901, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51015, 51907, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51790, 51907, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51820, 51907, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51031, 51911, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51680, 51911, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51035, 51913, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51640, 51913, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51053, 51918, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51570, 51918, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51730, 51918, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51059, 51919, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51600, 51919, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51610, 51919, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51069, 51921, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51840, 51921, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51081, 51923, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51595, 51923, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51089, 51929, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51690, 51929, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51095, 51931, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51830, 51931, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51121, 51933, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51750, 51933, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51143, 51939, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51590, 51939, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51143, 51941, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51670, 51941, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51153, 51942, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51683, 51942, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51685, 51942, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51161, 51944, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51775, 51944, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51163, 51945, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51530, 51945, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51687, 51945, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51165, 51947, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51660, 51947, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51175, 51949, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51620, 51949, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51177, 51951, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51630, 51951, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51191, 51953, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51520, 51953, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51195, 51955, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51720, 51955, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51199, 51958, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51735, 51958, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51515, 51019, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51560, 51005, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51005, 51903, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51580, 51903, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51780, 51925, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51083, 51925, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51019, 51909, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51149, 51941, VA$FIPS)
VA$FIPS <- ifelse(VA$FIPS == 51678, 51945, VA$FIPS)

VA$FIPS <-as.numeric(VA$FIPS)
VA[] <- lapply(VA, function(x) as.numeric(as.character(x)))
VA <- VA %>% group_by(FIPS, year) %>% summarise_all(funs(sum))

VA <- VA[order(VA$FIPS),] 


VA$state_cd <- "NA"
VA$state_name <-"Virginia"
VA$county_cd <- "NA"
VA$county_nm <- "NA"
VA$EastWest <- "East"
VA <- VA[,c(3,283,4,284,2,5:282,285,1)]
# write.csv(VA, file="VAtot.csv")


NM <- read.csv("WaterDataOriginal2015.csv", header=TRUE, na.strings = "-")
NM <- subset(NM, state_name=="New Mexico")
NM$state_cd <-formatC(NM$state_cd, width = 2, format = "d", flag = "0")
NM$county_cd <-formatC(NM$county_cd, width = 3, format = "d", flag = "0")
NM$FIPS <- paste(NM$state_cd, NM$county_cd, sep="")
NM$FIPS <- ifelse(NM$FIPS == 35006, 35007, NM$FIPS)
NM<-NM[,c(1,3,5:284)]
NM$FIPS <-as.numeric(NM$FIPS)
NM[] <- lapply(NM, function(x) as.numeric(as.character(x)))
NM <- NM %>% group_by(FIPS, year) %>% summarise_all(funs(sum))
NM <- NM[order(NM$FIPS),] 
NM$state_cd <- "NA"
NM$state_name <-"New Mexico"
NM$county_cd <- "NA"
NM$county_nm <- "NA"
NM$EastWest <- "West"
NM <- NM[,c(3,283,4,284,2,5:282,285,1)]


AZ <- read.csv("WaterDataOriginal2015.csv", header=TRUE, na.strings = "-")
AZ <- subset(AZ, state_name=="Arizona")
AZ$state_cd <-formatC(AZ$state_cd, width = 2, format = "d", flag = "0")
AZ$county_cd <-formatC(AZ$county_cd, width = 3, format = "d", flag = "0")
AZ$FIPS <- paste(AZ$state_cd, AZ$county_cd, sep="")
AZ$FIPS <- ifelse(AZ$FIPS == 04012, 04027, AZ$FIPS)
AZ<-AZ[,c(1,3,5:284)]
AZ$FIPS <-as.numeric(AZ$FIPS)
AZ[] <- lapply(AZ, function(x) as.numeric(as.character(x)))
AZ <- AZ %>% group_by(FIPS, year) %>% summarise_all(funs(sum))
AZ <- AZ[order(AZ$FIPS),] 
AZ$state_cd <- "NA"
AZ$state_name <-"Arizona"
AZ$county_cd <- "NA"
AZ$county_nm <- "NA"
AZ$EastWest <- "West"
AZ <- AZ[,c(3,283,4,284,2,5:282,285,1)]


CO <- read.csv("WaterDataOriginal2015.csv", header=TRUE, na.strings = "-")
CO <- subset(CO, state_name=="Colorado")
CO$state_cd <-formatC(CO$state_cd, width = 2, format = "d", flag = "0")
CO$county_cd <-formatC(CO$county_cd, width = 3, format = "d", flag = "0")
CO$FIPS <- paste(CO$state_cd, CO$county_cd, sep="")
CO$FIPS <- ifelse(CO$FIPS == 08014, 08013, CO$FIPS)
CO<-CO[,c(1,3,5:284)]
CO$FIPS <-as.numeric(CO$FIPS)
CO[] <- lapply(CO, function(x) as.numeric(as.character(x)))
CO <- CO %>% group_by(FIPS, year) %>% summarise_all(funs(sum))
CO <- CO[order(CO$FIPS),] 
CO$state_cd <- "NA"
CO$state_name <-"Colorado"
CO$county_cd <- "NA"
CO$county_nm <- "NA"
CO$EastWest <- "West"
CO <- CO[,c(3,283,4,284,2,5:282,285,1)]


WI <- read.csv("WaterDataOriginal2015.csv", header=TRUE, na.strings = "-")
WI <- subset(WI, state_name=="Wisconsin")
WI$state_cd <-formatC(WI$state_cd, width = 2, format = "d", flag = "0")
WI$county_cd <-formatC(WI$county_cd, width = 3, format = "d", flag = "0")
WI$FIPS <- paste(WI$state_cd, WI$county_cd, sep="")
WI$FIPS <- ifelse(WI$FIPS == 55078, 55901, WI$FIPS)
WI$FIPS <- ifelse(WI$FIPS == 55115, 55901, WI$FIPS)
WI<-WI[,c(1,3,5:284)]
WI[] <- lapply(WI, function(x) as.numeric(as.character(x)))
WI <- WI %>% group_by(FIPS, year) %>% summarise_all(funs(sum))
WI <- WI[order(WI$FIPS),] 
WI$state_cd <- "NA"
WI$state_name <-"Wisconsin"
WI$county_cd <- "NA"
WI$county_nm <- "NA"
WI$EastWest <- "West"
WI <- WI[,c(3,283,4,284,2,5:282,285,1)]



#----------------------------------------------------------------------------------------------------------------------
#Bring in USGS water use data. 
#Deal with formatting issues and merge FIPS code fixes.
#----------------------------------------------------------------------------------------------------------------------



#import data
waterdata <- read.csv("WaterDataOriginal2015.csv", header=TRUE, na.strings = "-")
waterdata <- waterdata[,c(1:283)]


#East West calculation
waterdata$EastWest <- ifelse(waterdata$state_cd %in% c(4,6,8,16,20,30,31,32,35,38,40,41,46,48,49,53,56), "West", "East")


#leading zeros for FIPS codes
waterdata$state_cd <-formatC(waterdata$state_cd, width = 2, format = "d", flag = "0")
waterdata$county_cd <-formatC(waterdata$county_cd, width = 3, format = "d", flag = "0")

#creating FIPS codes
waterdata$FIPS <- paste(waterdata$state_cd, waterdata$county_cd, sep="")
waterdata$FIPS <- ifelse(waterdata$FIPS==12025, 12086, waterdata$FIPS)
waterdata$FIPS <- ifelse(waterdata$FIPS==46113, 46102, waterdata$FIPS)

#get rid of island counties. Projections are for continental US and exclude island counties.
waterdata <- filter(waterdata, FIPS != 25007)
waterdata <- filter(waterdata, FIPS != 25019)
waterdata <- filter(waterdata, FIPS != 53055)

#combining VA FIPS fixes
colnames(VA)<-colnames(waterdata)
waterdata <- filter(waterdata, state_name != "Virginia")
VA[,c(5:283,285)] <- lapply(VA[,c(5:283,285)], function(x) as.numeric(x))
waterdata[,c(5:283,285)] <- lapply(waterdata[,c(5:283,285)], function(x) as.numeric(x))
waterdata <-bind_rows(waterdata,VA)

#combining NM FIPS fixes
colnames(NM)<-colnames(waterdata)
waterdata <- filter(waterdata, state_name != "New Mexico")
NM[,c(5:283,285)] <- lapply(NM[,c(5:283,285)], function(x) as.numeric(x))
waterdata[,c(5:283,285)] <- lapply(waterdata[,c(5:283,285)], function(x) as.numeric(x))
waterdata <-bind_rows(waterdata,NM)

#combining AZ FIPS fixes
colnames(AZ)<-colnames(waterdata)
waterdata <- filter(waterdata, state_name != "Arizona")
AZ[,c(5:283,285)] <- lapply(AZ[,c(5:283,285)], function(x) as.numeric(x))
waterdata[,c(5:283,285)] <- lapply(waterdata[,c(5:283,285)], function(x) as.numeric(x))
waterdata <-bind_rows(waterdata,AZ)

#combining CO FIPS fixes
colnames(CO)<-colnames(waterdata)
waterdata <- filter(waterdata, state_name != "Colorado")
CO[,c(5:283,285)] <- lapply(CO[,c(5:283,285)], function(x) as.numeric(x))
waterdata[,c(5:283,285)] <- lapply(waterdata[,c(5:283,285)], function(x) as.numeric(x))
waterdata <-bind_rows(waterdata,CO)

#combining WI FIPS fixes
colnames(WI)<-colnames(waterdata)
waterdata <- filter(waterdata, state_name != "Wisconsin")
WI[,c(5:283,285)] <- lapply(WI[,c(5:283,285)], function(x) as.numeric(x))
waterdata[,c(5:283,285)] <- lapply(waterdata[,c(5:283,285)], function(x) as.numeric(x))
waterdata <-bind_rows(waterdata,WI)



# #outputing for states HISTORIC--------------------------
# Thermo<-waterdata[,c(285,5,1:4,85,86,87,101,104,118,121,135,136,152,157,169,174)]
# Thermo[is.na(Thermo)] <- 0
# Thermo$Total.Thermoelectric.Power.total.self.supplied.withdrawals..total..in.Mgal.d <- ifelse(is.na(Thermo$Total.Thermoelectric.Power.total.self.supplied.withdrawals..total..in.Mgal.d),Thermo$Fossil.fuel.Thermoelectric.Power.total.self.supplied.withdrawals..in.Mgal.d+Thermo$Geothermal.Thermoelectric.Power.total.self.supplied.withdrawals..in.Mgal.d+Thermo$Nuclear.Thermoelectric.Power.total.self.supplied.withdrawals..in.Mgal.d+Thermo$Thermoelectric.Power..Once.through.cooling..total.self.supplied.withdrawals..total..in.Mgal.d+Thermo$Thermoelectric.Power..Closed.loop.cooling..total.self.supplied.withdrawals..total..in.Mgal.d ,Thermo$Total.Thermoelectric.Power.total.self.supplied.withdrawals..total..in.Mgal.d )
# Thermo<- Thermo[,c(4,2,7)]
# names(Thermo)[names(Thermo) == 'Total.Thermoelectric.Power.total.self.supplied.withdrawals..total..in.Mgal.d'] <- 'th.wd.tot'
# #-------------------------------------------------------

#saving mining data for later
mining <- waterdata[,c(285,5,184)]

#drop unneeded columns
waterdata <- waterdata[,c(285,5,1:4,6,13,16,22,23,24,31,34,36,38,40,41,49,52,55,57,64,67,70,72,192,193,198,201,204,210,213,216,222,225,228,234,238,241,245:248,284)]

names(waterdata)[names(waterdata) == 'Total.Population.total.population.of.area..in.thousands'] <- 'pop'
waterdata$inc <- NA


waterdata <- waterdata[order(waterdata$FIPS, waterdata$year),]


#--------------------------------------------------------------------------------------------------------------------
#filling in missing data
#Texas is missing Domestic data for 2015, so take the average of 2005 and 2010
#South Dakota is missing Domestic, Public, and Industrial for 2015. Take the average of 2005 and 2010 for all of these.
#North Dakota is missing Domestic and Public for 2015. Take the average of 2005 and 2010. 
#--------------------------------------------------------------------------------------------------------------------
# #Texas
# waterdata$d.lag <- lag(waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d, 1)
# waterdata$d.lag2 <- lag(waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d, 2)
# waterdata$avgd <- (waterdata$d.lag + waterdata$d.lag2)/2
# waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==48,waterdata$avgd, waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d ) 
# 
# #South Dakota
# waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==46,waterdata$avgd, waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d ) 
# 
# waterdata$p.lag <- lag(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d, 1)
# waterdata$p.lag2 <- lag(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d, 2)
# waterdata$avgp <- (waterdata$p.lag + waterdata$p.lag2)/2
# waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==46,waterdata$avgp, waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ) 
# 
# waterdata$i.lag <- lag(waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d, 1)
# waterdata$i.lag2 <- lag(waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d, 2)
# waterdata$avgi <- (waterdata$i.lag + waterdata$i.lag2)/2
# waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==46,waterdata$avgi, waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d) 
# 
# #North Dakota
# waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==38,waterdata$avgd, waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d ) 
# 
# waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d  <- ifelse(waterdata$year==2015 & waterdata$state_cd==38,waterdata$avgp, waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ) 
# 
# 
# waterdata <- waterdata[,c(1:46)]

#----------------------------------------------------------------------------------------------------------------------
#Calculate withdrawal data.
#Lots of assumptions are made in order to get usuable withdrawal data. These are documented below.
#----------------------------------------------------------------------------------------------------------------------



#Since there is no delivery data for 2000, we take the average delivery of 1995 and 2005.
waterdata$check1 <- as.numeric(as.character(lag(waterdata$FIPS, 1)))
waterdata$check2 <- as.numeric(as.character(lead(waterdata$FIPS, 1)))
waterdata$dcheck1 <- as.numeric(as.character(lag(waterdata$Domestic.deliveries.from.public.supply..in.Mgal.d, 1)))
waterdata$dcheck2 <- as.numeric(as.character(lead(waterdata$Domestic.deliveries.from.public.supply..in.Mgal.d, 1)))
waterdata$D.2000.del <- ifelse(waterdata$year == 2000, ifelse(waterdata$check1 == waterdata$check2 , (waterdata$dcheck1 + waterdata$dcheck2)/2, NA) ,NA)
waterdata$D.tot.del <- ifelse(is.na(waterdata$Domestic.deliveries.from.public.supply..in.Mgal.d), waterdata$D.2000.del, waterdata$Domestic.deliveries.from.public.supply..in.Mgal.d)





#===================================================================================================================
#THIS DATA HAS BEEN UPDATED, BUT THIS SECTION REMAINS TO DOCUMENT OUR METHODS FOR FILLING IN MISSING DATA POINTS

#--------------------------------------------------------------------------------------------------------------------
#filling in missing data
#Oklahoma 2015
#public withdrawal data is not available but domestic delivery data is, so 2015 public withdrawal = (2010 public withdrawal/ 2010 domestic delivery)*2015 domestic delivery
#--------------------------------------------------------------------------------------------------------------------
# 
# waterdata$P.wd.lag <- lag(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d, 1)
# waterdata$D.del.lag <- lag(waterdata$D.tot.del, 1)
# waterdata$OKpub <- (waterdata$P.wd.lag / waterdata$D.del.lag) * waterdata$D.tot.del
# waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d <- ifelse(waterdata$year==2015 & waterdata$state_cd==40,waterdata$OKpub, waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d) 
# 
# waterdata <- waterdata[,c(1:52)]

#===================================================================================================================



# D.wd.surface is the amount of self-supplied surface water withdrawn plus the amount of surface water delivered from public. 
#Since Public does not differentiate between fresh water sources, the domestic delivery amount is multiplied by the proportion of public supply water that comes from surface water.

waterdata$P.Prop.Surface <- ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d == 0, NA, (waterdata$Public.Supply.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d))
waterdata$D.wd.surface <- ifelse(is.na(waterdata$P.Prop.Surface), waterdata$Domestic.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d, waterdata$Domestic.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d +(waterdata$D.tot.del*waterdata$P.Prop.Surface) )
                       

#Deliveries to IC are not recorded 2000 and after. For those years this column takes the proportion of deliveries to total Public water withdrawal of the previous year and multiplies 
#that by the public withdrawal amount for the current year.need to repeat this calc four times since it depends on the year prior.

waterdata$IC.tot.del <- ifelse(is.na(waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d +waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$IC.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d+waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d)
waterdata$IC.tot.del <- ifelse(is.na(waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d +waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$IC.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d+waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d)
waterdata$IC.tot.del <- ifelse(is.na(waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d +waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$IC.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d+waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d)
waterdata$IC.tot.del <- ifelse(is.na(waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d +waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$IC.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d+waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d)
waterdata$IC.tot.del <- ifelse(is.na(waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d +waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$IC.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$Industrial.deliveries.from.public.supply..in.Mgal.d+waterdata$Commercial.deliveries.from.public.supply..in.Mgal.d)

waterdata$IC.tot.del <- replace(waterdata$IC.tot.del, is.na(waterdata$IC.tot.del), 0)


#IC.wd.surface assumes that if commercial data is missing it is 0. There is no data for commercial self supplied after 95 since it was no longer required.
#similar to domestic, we assume that the proportion of public surface water withdrawn applies to the amount deliveried to IC.

waterdata$IC.del.surf <- waterdata$IC.tot.del*waterdata$P.Prop.Surface
waterdata$IC.wd.surface <- ifelse(is.na(waterdata$Industrial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d), waterdata$IC.del.surf, ifelse(is.na(waterdata$Commercial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d), waterdata$Industrial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d+waterdata$IC.del.surf, waterdata$IC.del.surf+waterdata$Industrial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d+waterdata$Commercial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d ))

#calculated irrigated acres. 85 and 90 did not collect microirrigated acres. For those years, this is the sum of surface and sprinkler.
#For more recent years this is the total (including microirrigated)

waterdata$IR.acres <- ifelse(is.na(waterdata$Irrigation..Total.total.irrigation..in.thousand.acres), waterdata$Irrigation..Total.sprinkler.irrigation..in.thousand.acres+waterdata$Irrigation..Total.surface.irrigation..in.thousand.acres, waterdata$Irrigation..Total.total.irrigation..in.thousand.acres)


#rename IR.wd.surface
names(waterdata)[names(waterdata) == 'Irrigation..Total.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d'] <- 'IR.wd.surface'

#This is for Surface water. LS changed multiple times over the years. As such, different years are calculated in different columns. See USGS website for changes in definitions.
waterdata$LS.85.95 <- ifelse(is.na(waterdata$Livestock..Animal.Specialties..self.supplied.surface.water.withdrawals..fresh..in.Mgal.d), waterdata$Livestock..Stock..self.supplied.surface.water.withdrawals..fresh..in.Mgal.d, waterdata$Livestock..Animal.Specialties..self.supplied.surface.water.withdrawals..fresh..in.Mgal.d+waterdata$Livestock..Stock..self.supplied.surface.water.withdrawals..fresh..in.Mgal.d)
waterdata$LS.wd.surface <- ifelse(is.na(waterdata$LS.85.95), waterdata$Livestock.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d, waterdata$LS.85.95)


#rename AQ.wd.surface
names(waterdata)[names(waterdata) == 'Aquaculture.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d'] <- 'AQ.wd.surface'


#calculating Total domestic water withdrawal. Again, took averages for the year 2000.
waterdata$dtotcheck1 <- as.numeric(as.character(lag(waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d, 1)))
waterdata$dtotcheck2 <- as.numeric(as.character(lead(waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d, 1)))
waterdata$D.wd.tot.2000 <- ifelse(waterdata$year == 2000, ifelse(is.na(waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d), ifelse(waterdata$check1 == waterdata$check2 , (waterdata$dtotcheck1 + waterdata$dtotcheck2)/2, NA) , waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d),NA)
waterdata$D.wd.tot <- ifelse(waterdata$year==2000, waterdata$D.wd.tot.2000, waterdata$Domestic.total.self.supplied.withdrawals.plus.deliveries..in.Mgal.d)
waterdata$D.prop <- ifelse(waterdata$D.wd.tot==0, 0, waterdata$D.wd.surface/waterdata$D.wd.tot)

#changing names
names(waterdata)[names(waterdata) == 'Irrigation..Total.total.self.supplied.withdrawals..fresh..in.Mgal.d'] <- 'IR.wd.tot'
names(waterdata)[names(waterdata) == 'Aquaculture.total.self.supplied.withdrawals..fresh..in.Mgal.d'] <- 'AQ.wd.tot'


#Total livestock withdrawal - since collection methods changed different variables are only appropriate for certain years. LS changed multiple times over the years. As such, different years are calculated in different columns. See USGS website for changes in definitions.
waterdata$LS.wd.tot <- if_else(waterdata$year == 1985 |waterdata$year == 1990 |waterdata$year == 1995, if_else(is.na(waterdata$Livestock..Animal.Specialties..total.self.supplied.withdrawals..fresh..in.Mgal.d), waterdata$Livestock..Stock..total.self.supplied.withdrawals..fresh..in.Mgal.d, waterdata$Livestock..Animal.Specialties..total.self.supplied.withdrawals..fresh..in.Mgal.d + waterdata$Livestock..Stock..total.self.supplied.withdrawals..fresh..in.Mgal.d)
                               , waterdata$Livestock.total.self.supplied.withdrawals..fresh..in.Mgal.d)

#total IC withdrawals
waterdata$IC.wd.tot <- if_else(is.na(waterdata$Commercial.total.self.supplied.withdrawals..fresh..in.Mgal.d) & is.na(waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d), waterdata$IC.tot.del,
                               if_else(is.na(waterdata$Commercial.total.self.supplied.withdrawals..fresh..in.Mgal.d), waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d + waterdata$IC.tot.del,
                                       if_else(is.na(waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d), waterdata$Commercial.total.self.supplied.withdrawals..fresh..in.Mgal.d +waterdata$IC.tot.del,waterdata$Commercial.total.self.supplied.withdrawals..fresh..in.Mgal.d+waterdata$Industrial.total.self.supplied.withdrawals..fresh..in.Mgal.d+waterdata$IC.tot.del)
                               ))
waterdata[mapply(is.infinite, waterdata)] <- 0

#add mining data to IC
mining[is.na(mining)] <- 0
waterdata <- merge(waterdata, mining, by=c("FIPS","year"))
waterdata$IC.wd.tot <- waterdata$IC.wd.tot + waterdata$Mining.total.self.supplied.withdrawals..fresh..in.Mgal.d
waterdata <- waterdata[,-68]

#calculating total withdrawals (total and surface) not including thermo. This is not explicitly used later.

waterdata$wdtot.not.TH <- if_else(is.na(waterdata$AQ.wd.tot),waterdata$IC.wd.tot + waterdata$LS.wd.tot + waterdata$D.wd.tot +waterdata$IR.wd.tot ,waterdata$IC.wd.tot + waterdata$AQ.wd.tot + waterdata$LS.wd.tot + waterdata$D.wd.tot +waterdata$IR.wd.tot)
waterdata$wdsurf.not.TH <- waterdata$IC.wd.surface + waterdata$LS.wd.surface +waterdata$D.wd.surface + waterdata$IR.wd.surface





#------------------------------------------------------------------------------------------------------------------
#calculating Public use:
#Public Use = Total public withdrawals - IC deliveries - D deliveries - TH deliveries
#In 1995, USGS included this calculation. Previously Tom used the proportion of Public use to total public withdrawal from 1995. Here we calculate 
#deliveries for each sector and subtract that from total public withdrawals to find public use.
#Note that some values are negative in the 1995 data. This is a huge problem that needs to be addressed. We assume that any negative values are 0. We cannot have more deliveries than total withdrawals.
#------------------------------------------------------------------------------------------------------------------

#TH delivery calculation
colnames(waterdata)[colnames(waterdata)=="Public.Supply.deliveries.to.thermoelectric..in.Mgal.d"] <- "TH.tot.del"

#Washington only has data for TH del for 2015 so use proportion from 2015 to fill in previous years
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)
waterdata$TH.tot.del <-ifelse(waterdata$state_cd==53, ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lead(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del), waterdata$TH.tot.del)


#fill in TH delivery data for every other state. For missing values, We use the proportion of TH deliveries to total Public self supply and multiply that by the total public self supply for the missing year.
waterdata$TH.tot.del <- ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del)
waterdata$TH.tot.del <- ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del)
waterdata$TH.tot.del <- ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del)
waterdata$TH.tot.del <- ifelse(is.na(waterdata$TH.tot.del), ifelse(waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d==0, 0, lag(waterdata$TH.tot.del/waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ,1)*waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d ), waterdata$TH.tot.del)

waterdata$TH.tot.del <- replace(waterdata$TH.tot.del, is.na(waterdata$TH.tot.del), 0)

colnames(waterdata)[colnames(waterdata)=="Public.Supply.public.use.and.losses..in.Mgal.d"] <- "Public"
waterdata$Public <- ifelse(is.na(waterdata$Public), waterdata$Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d - waterdata$IC.tot.del - waterdata$D.tot.del - waterdata$TH.tot.del, waterdata$Public )

waterdata$Public <- ifelse(waterdata$Public <= 0, 0, waterdata$Public)



#----------------------------------------------------------------------------------------------------------------------
#This section calculates consumptive use proportions. 
#Due to missing data points at the county level, the average of consumptive use for 1980, 1985, and 1990 are aggregated to the state level.
#Missing data is assumed to be 0 so that it does not factor into the average. Sum up withdrawal and consumptive use over those three datapoints, and
#then take proportion at the state level and apply that to each county.
#----------------------------------------------------------------------------------------------------------------------

#adding proportions of surface water for each industry

waterdata$IC.prop <- ifelse(waterdata$IC.wd.tot ==0, 0, waterdata$IC.wd.surface/waterdata$IC.wd.tot)
waterdata$IR.prop <- ifelse(waterdata$IR.wd.tot ==0, 0, waterdata$IR.wd.surface/waterdata$IR.wd.tot)

#IC consumptive use proportion
waterdata$IC.consumptive <-  (waterdata$Commercial.consumptive.use..fresh..in.Mgal.d+waterdata$Industrial.consumptive.use..fresh..in.Mgal.d)
waterdata$IC.cons.lag <- lag(waterdata$IC.consumptive, 1)
waterdata$IC.wd.tot.lag <- lag(waterdata$IC.wd.tot, 1)
waterdata$IC.wd.tot.lag[is.na(waterdata$IC.wd.tot.lag)] <- 0
waterdata$IC.wd.tot[is.na(waterdata$IC.wd.tot)] <- 0
waterdata$IC.consumptive[is.na(waterdata$IC.consumptive)] <- 0
waterdata$IC.cons.lag[is.na(waterdata$IC.cons.lag)] <- 0

waterdata$IC.calc.cons <- NA

#DP consumptive use proportion
waterdata$D.consumptive <- waterdata$Domestic.consumptive.use..fresh..in.Mgal.d
waterdata$D.cons.lag <- lag(waterdata$D.consumptive, 1)
waterdata$D.wd.tot.lag <- lag(waterdata$D.wd.tot, 1)
waterdata$D.wd.tot.lag[is.na(waterdata$D.wd.tot.lag)] <- 0
waterdata$D.wd.tot[is.na(waterdata$D.wd.tot)] <- 0
waterdata$D.consumptive[is.na(waterdata$D.consumptive)] <- 0
waterdata$D.cons.lag[is.na(waterdata$D.cons.lag)] <- 0
waterdata$D.calc.cons <- NA
  

#LS consumptive use proportion
waterdata$LS.consumptive <- waterdata$Livestock..Stock..consumptive.use..fresh..in.Mgal.d + waterdata$Livestock..Animal.Specialties..consumptive.use..fresh..in.Mgal.d
waterdata$LS.cons.lag <- lag(waterdata$LS.consumptive, 1)
waterdata$LS.wd.tot.lag <- lag(waterdata$LS.wd.tot, 1)
waterdata$LS.wd.tot.lag[is.na(waterdata$LS.wd.tot.lag)] <- 0
waterdata$LS.wd.tot[is.na(waterdata$LS.wd.tot)] <- 0
waterdata$LS.consumptive[is.na(waterdata$LS.consumptive)] <- 0
waterdata$LS.cons.lag[is.na(waterdata$LS.cons.lag)] <- 0
waterdata$LS.calc.cons <- NA
  

#IR consumptive use proportion
waterdata$IR.consumptive <- waterdata$Irrigation..Total.consumptive.use..fresh..in.Mgal.d
waterdata$IR.cons.lag <- lag(waterdata$IR.consumptive, 1)
waterdata$IR.wd.tot.lag <- lag(waterdata$IR.wd.tot, 1)
waterdata$IR.wd.tot.lag[is.na(waterdata$IR.wd.tot.lag)] <- 0
waterdata$IR.wd.tot[is.na(waterdata$IR.wd.tot)] <- 0
waterdata$IR.consumptive[is.na(waterdata$IR.consumptive)] <- 0
waterdata$IR.cons.lag[is.na(waterdata$IR.cons.lag)] <- 0
waterdata$IR.calc.cons <- NA



cons <- waterdata[,c(4,2,64,66,67,39,72:87)]



cons$D.wd <- cons$D.wd.tot
cons$D.cons <- cons$D.cons.lag 
cons$IC.wd <- cons$IC.wd.tot
cons$IC.cons <- cons$IC.consumptive
cons$LS.wd <- cons$LS.wd.tot
cons$LS.cons <- cons$LS.consumptive
cons$IR.wd <- cons$IR.wd.tot
cons$IR.cons <- cons$IR.consumptive

cons1 <- subset(cons, year==1990)
cons2 <- subset(cons, year==1985)
cons3 <- subset(cons, year==1980)
cons <- rbind(cons1,cons2,cons3)

#calculates consumptive use proportions at state level in an attempt to reduce county level noise
cons <- cons %>% group_by(state_name) %>% summarise_all(funs(sum))

cons$D.calc.cons <- cons$D.cons/cons$D.wd
cons$IC.calc.cons<- cons$IC.cons/cons$IC.wd
cons$LS.calc.cons<- cons$LS.cons/cons$LS.wd
cons$IR.calc.cons<- cons$IR.cons/cons$IR.wd


is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


cons[is.nan(cons)]<-1
cons <- cons[,c(1,10,14,18,22)]


waterdata <- merge(waterdata,cons, by="state_name")




#===================================================================================================================
#This section outputs historic data for states rather than counties. We used this for creating some maps and summary data.


# #outputing for states HISTORIC--------------------------
# states <- waterdata[,c(1,3,7,36,39,64,66,67,87:91)]
# states[is.na(states)] <-0
# states <- states %>% group_by(state_name, year) %>% summarise_all(funs(sum))
# Thermo <- Thermo %>% group_by(state_name, year) %>% summarise_all(funs(sum))
# states <- merge(states, Thermo, by=c("state_name", "year"))
# states$wd <- states$AQ.wd.tot+states$LS.wd.tot+states$IR.wd.tot+states$D.wd.tot+states$IC.wd.tot+states$th.wd.tot
# states$pop <- states$pop * 1000
# states$percapita <- states$wd/states$pop
# totus <- states[,c(2,3,15)]
# totus <- totus %>% group_by(year) %>% summarise_all(funs(sum))
# totus$percapita <- totus$wd/totus$pop
# totus <- totus[,c(1,4)]
# states <- states[,c(1,2,16)]
# write.csv(totus, file="US_historicpercapita.csv")
# write.csv(states, file="states_historicpercapita.csv")
#===================================================================================================================





#---------------------------------------------------------------------------------------
#AQ consumption is at the HUC4 level, approximate it at the county level.


huc<-read.csv("CountyWatershedPct.csv")
huc$HUC_10 <-signif(huc$HUC_10,4)/1000000
huc<-huc[,c(1:4)]
huc$FIPS <- as.numeric(as.character(huc$FIPS))
huc$HUC_10 <- as.numeric(as.character(huc$HUC_10))


huc$FIPS <- ifelse(huc$FIPS == 51003, 51901, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51540, 51901, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51015, 51907, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51790, 51907, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51820, 51907, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51031, 51911, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51680, 51911, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51035, 51913, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51640, 51913, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51053, 51918, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51570, 51918, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51730, 51918, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51059, 51919, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51600, 51919, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51610, 51919, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51069, 51921, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51840, 51921, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51081, 51923, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51595, 51923, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51089, 51929, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51690, 51929, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51095, 51931, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51830, 51931, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51121, 51933, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51750, 51933, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51143, 51939, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51590, 51939, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51143, 51941, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51670, 51941, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51153, 51942, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51683, 51942, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51685, 51942, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51161, 51944, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51775, 51944, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51163, 51945, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51530, 51945, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51687, 51945, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51165, 51947, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51660, 51947, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51175, 51949, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51620, 51949, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51177, 51951, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51630, 51951, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51191, 51953, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51520, 51953, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51195, 51955, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51720, 51955, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51199, 51958, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51735, 51958, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51515, 51019, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51560, 51005, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51005, 51903, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51580, 51903, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51780, 51925, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51083, 51925, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51019, 51909, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51149, 51941, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 51678, 51945, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 12025, 12086, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 46113, 46102, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 55078, 55901, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 55115, 55901, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 08014, 08013, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 04012, 04027, huc$FIPS)
huc$FIPS <- ifelse(huc$FIPS == 35006, 35007, huc$FIPS)




huc$FIPS <-as.numeric(huc$FIPS)


huc <- huc %>% group_by(FIPS,HUC_10) %>% summarise_all(funs(sum,mean))
huc$pcthuc8 <- huc$Shape_Area_sum/huc$CountyArea_mean
huc1 <- huc[,c(1,7)]
huc1 <- huc1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

huc <- merge(huc, huc1, by="FIPS")
huc$pcthuc8.x <- huc$pcthuc8.x/huc$pcthuc8.y
huc<- huc[,c(1,2,7)]




AQ <- huc
AQ$AQ.calc.cons <- ifelse(AQ[,2]< 300, 0.05, ifelse(AQ[,2]< 400, 0.3, ifelse(AQ[,2]< 600, 0.05, ifelse(AQ[,2]< 700, 0.02, ifelse(AQ[,2]< 800, 0.2,ifelse(AQ[,2]< 900, 0.4,ifelse(AQ[,2]< 1100, 0.2,ifelse(AQ[,2]< 1200, 0.3,ifelse(AQ[,2]< 1300, 0.4,ifelse(AQ[,2]< 1400, 0.2,ifelse(AQ[,2]< 1500, 0.01,ifelse(AQ[,2]< 1700, 0.2,ifelse(AQ[,2]< 1800, 0.01, 0.2 )  )  )  )  )  )  )  )  ) ) ) ) )
AQ$AQ.calc.cons <- AQ$AQ.calc.cons*AQ$pcthuc8.x
AQ <- AQ[,c(1,4)]
AQ <- AQ %>% group_by(FIPS) %>% summarise_all(funs(sum))




#===================================================================================================================

#This section outputs historic data for ML (drought paper) code

# 
# hist <- waterdata[,c(2,3,12,17,36,39,45,64,66,67,72:91)]
# hist <- hist[,c(1:3,5,6,8:10,27:30)]
# hist<-hist[,-4]
# #missing SD public, take average of 2005 and 2010 for 2015 value
# hist$p.lag <- lag(hist$Public, 1)
# hist$p.lag2 <- lag(hist$Public, 2)
# hist$avgp <- (hist$p.lag + hist$p.lag2)/2
# hist$state <- floor(hist$FIPS/1000)
# hist$Public  <- ifelse(hist$year==2015 & hist$state==46,hist$avgp, hist$Public)
# hist <- hist[,-c(12:15)]
# 
# # hist$D.wd.tot <- hist$D.wd.tot +hist$Public
# hist <- hist[,-3]
# hist <- hist[,c(1:2,6,4:5,3,7:10)]
# for (i in 7:10){
#   hist[,i]<- hist[,i]*hist[,i-4]
# }
# 
# 
# colnames(hist) <- c("FIPS","year","IC","DP","LS","IR","IC_cons","DP_cons","LS_cons","IR_cons")
# write.csv(hist, file="historic_wd_forML_corrected.csv")

#===================================================================================================================


#-------------------------------------------------------------------------------------------------------------------
#In order to minimize county level data noise, we take the average of 2005, 2010, 2015 as the base number to project out to 2070
#In other words, replace 2015 with average of 2005, 2010 and 2015
#This code also combines Public and Domestic
#-------------------------------------------------------------------------------------------------------------------
waterdata2005 <- waterdata[waterdata$year==2005,c(2,12,64,66,67,36,39)]
waterdata2010 <- waterdata[waterdata$year==2010,c(2,12,64,66,67,36,39)]
waterdata2015 <- waterdata[waterdata$year==2015,c(2,12,64,66,67,36,39)]

waterdata2015<- merge(waterdata2010, waterdata2015, by="FIPS")
waterdata2015<- merge(waterdata2005, waterdata2015, by="FIPS")

for (n in 1:length(rownames(waterdata2015))){
  waterdata2015[n,c(3)] <- ifelse(anyNA(c(waterdata2015[n,c(3)],waterdata2015[n,c(2)],waterdata2015[n,c(8)],waterdata2015[n,c(14)],waterdata2015[n,c(9)],waterdata2015[n,c(15)])), ifelse(is.na(waterdata2015[n,c(3)]),(waterdata2015[n,c(14)] + waterdata2015[n,c(15)] + waterdata2015[n,c(8)] + waterdata2015[n,c(9)])/2,waterdata2015[n,c(3)]+waterdata2015[n,c(2)] ) , (waterdata2015[n,c(2)] + waterdata2015[n,c(3)] + waterdata2015[n,c(8)] + waterdata2015[n,c(9)] + waterdata2015[n,c(14)] + waterdata2015[n,c(15)])/3)
  waterdata2015[n,c(4)] <- ifelse(anyNA(c(waterdata2015[n,c(4)],waterdata2015[n,c(10)],waterdata2015[n,c(16)])), ifelse(is.na(waterdata2015[n,c(4)]),( waterdata2015[n,c(10)] + waterdata2015[n,c(16)])/2,waterdata2015[n,c(4)] ) , (waterdata2015[n,c(4)] + waterdata2015[n,c(10)] + waterdata2015[n,c(16)])/3)
  waterdata2015[n,c(5)] <- ifelse(anyNA(c(waterdata2015[n,c(5)],waterdata2015[n,c(11)],waterdata2015[n,c(17)])), ifelse(is.na(waterdata2015[n,c(5)]),( waterdata2015[n,c(11)] + waterdata2015[n,c(17)])/2,waterdata2015[n,c(5)] ) , (waterdata2015[n,c(5)] + waterdata2015[n,c(11)] + waterdata2015[n,c(17)])/3)
  waterdata2015[n,c(6)] <- ifelse(anyNA(c(waterdata2015[n,c(6)],waterdata2015[n,c(12)],waterdata2015[n,c(18)])), ifelse(is.na(waterdata2015[n,c(6)]),( waterdata2015[n,c(12)] + waterdata2015[n,c(18)])/2,waterdata2015[n,c(6)] ) , (waterdata2015[n,c(6)] + waterdata2015[n,c(12)] + waterdata2015[n,c(18)])/3)
  waterdata2015[n,c(7)] <- ifelse(anyNA(c(waterdata2015[n,c(7)],waterdata2015[n,c(13)],waterdata2015[n,c(19)])), ifelse(is.na(waterdata2015[n,c(7)]),( waterdata2015[n,c(13)] + waterdata2015[n,c(19)])/2,waterdata2015[n,c(7)] ) , (waterdata2015[n,c(7)] + waterdata2015[n,c(13)] + waterdata2015[n,c(19)])/3)
}


waterdata2015 <- waterdata2015[,c(1:7)]

consumptive <- waterdata[waterdata$year==2015,c(2,7,46,58,45,88:91)]
waterdata2015 <- merge(waterdata2015,consumptive, by="FIPS")
waterdata2015<- merge(waterdata2015,AQ, by="FIPS")


waterdata2015$DP.growth<- ifelse(waterdata2015$EastWest == "East",-.0095,-.0083)
waterdata2015$DP.decay <- ifelse(waterdata2015$EastWest == "East",-.03,-.03)
waterdata2015$IC.growth<- ifelse(waterdata2015$EastWest == "East",-.0331,-.0296)
waterdata2015$IC.decay <- ifelse(waterdata2015$EastWest == "East",-.035,-.042)
waterdata2015$LS.growth<- ifelse(waterdata2015$EastWest == "East",-.0113,-.0221)
waterdata2015$LS.decay <- ifelse(waterdata2015$EastWest == "East",-.04,-.04)
waterdata2015$IR.growth<- ifelse(waterdata2015$EastWest == "East",0,-.0096)
waterdata2015$IR.decay <- ifelse(waterdata2015$EastWest == "East",0,-.0250)
waterdata2015$AQ.growth<- ifelse(waterdata2015$EastWest == "East",0.0336,0.0829)
waterdata2015$AQ.decay <- ifelse(waterdata2015$EastWest == "East",-.05,-.1)

names(waterdata2015)[names(waterdata2015) == 'FIPS'] <- 'fips'



#---------------------------------------------------------------------------------------------------------------------
# predicted acreage for input into WEAP code


ir <- waterdata[, c('FIPS','year','IR.acres','EastWest')]

ir$ir.g <- ifelse(ir$EastWest == "East", 0.0211,-0.0010)
ir$ir.d <- ifelse(ir$EastWest == "East", -0.0350,-0.0100)



#growth/decay rates for acreage are at HUC4 level, so apply percentages of HUC4 to rates and then aggregate back up to county level

ir <- merge(ir, huc, by="FIPS")
ir <- ir[,c(1,2,3,5:8)]
ir$IR.acres <- ir$IR.acres*ir$pcthuc8.x
ir$ir.g <- ir$ir.g*ir$pcthuc8.x
ir$ir.d <- ir$ir.d*ir$pcthuc8.x


ir <- ir %>% group_by(FIPS,year,HUC_10) %>% summarise_all(funs(sum,mean))
ir$acre.g <-  ifelse(ir$HUC_10 < 1000, 0.0211,-0.001 )
ir$acre.d <-  ifelse(ir$HUC_10 < 300, -0.035,ifelse(ir$HUC_10 < 400, -0.09,ifelse(ir$HUC_10 < 800, -0.035,ifelse(ir$HUC_10 < 900, -0.08,ifelse(ir$HUC_10 < 1000, -0.07,ifelse(ir$HUC_10 < 1100, -0.05,-0.01 ) ) ) ) ) )
ir <-ir[,c(1,2,4,9:13)]
ir$acre.g <- ir$acre.g*ir$pcthuc8.x_mean
ir$acre.d <- ir$acre.d*ir$pcthuc8.x_mean 

ir <- ir %>% group_by(FIPS,year) %>% summarise_all(funs(sum,mean))
ir <- ir[,c(1:3,7,8,10:11)]

#some 2015 acreage data is missing, so project from 2010 in this case
ir2010 <- ir[ir$year==2010,]
ir2010$acresproj <- ir2010$IR.acres_sum_sum*(1+ir2010$acre.g_sum * (1+ir2010$acre.d_sum)^(2015-2010))^5
ir2010 <- ir2010[,c(1,3,8)]

ir <- ir[ir$year==2015,]
ir <- merge(ir,ir2010, by="FIPS" )
ir <- ir[,c(1,3,4:7,9)]
ir2 <- ir[rowSums(is.na(ir)) == 0,]
ir1 <- ir[rowSums(is.na(ir)) > 0,]
ir1$IR.acres_sum_sum.x <- ir1$acresproj

ir <- rbind(ir1,ir2)
ir <- ir[,c(1:6)]

#project acres out to 2070
ir$acres_2020 <- ir$IR.acres_sum_sum.x*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2020-2015))^5
ir$acres_2025 <- ir$acres_2020*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2025-2015))^5
ir$acres_2030 <- ir$acres_2025*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2030-2015))^5
ir$acres_2035 <- ir$acres_2030*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2035-2015))^5
ir$acres_2040 <- ir$acres_2035*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2040-2015))^5
ir$acres_2045 <- ir$acres_2040*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2045-2015))^5
ir$acres_2050 <- ir$acres_2045*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2050-2015))^5
ir$acres_2055 <- ir$acres_2050*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2055-2015))^5
ir$acres_2060 <- ir$acres_2055*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2060-2015))^5
ir$acres_2065 <- ir$acres_2060*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2065-2015))^5
ir$acres_2070 <- ir$acres_2065*(1+ir$acre.g_sum * (1+ir$acre.d_sum)^(2070-2015))^5

ir <- ir[,c(1,2,7:17)]
colnames(ir) <- c("fips","pr_acre_2015","pr_acre_2020","pr_acre_2025","pr_acre_2030","pr_acre_2035","pr_acre_2040","pr_acre_2045","pr_acre_2050","pr_acre_2055","pr_acre_2060","pr_acre_2065","pr_acre_2070")



write.csv(ir, file="acredata-updated.csv")

waterdata2015 <- merge(waterdata2015, ir, by="fips")
waterdata2015$IR.acres <- waterdata2015$pr_acre_2015

waterdata2015<- waterdata2015[,c(1:26)]
waterdata2015$year <- 2015
waterdata2015<- waterdata2015[,c(1,27,11,8,9,3,5,4,7,6,12:15,10,16,17:26)]
df<-waterdata2015
#----------------------------------------------------------------------------------------------------------------------
#inputting population and income from Wear data projections. projections start from 2010, so 2015 data points are projections and differ by scenario.

ssp1 <- read.csv("pipc_ssp1.csv")
temp <- read.csv("pop_ssp1.csv")

popinc <- function(inc, pop, wateruse)
{
  #fix fips codes
inc <- merge(inc, pop, by="fips")
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
inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51670, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51153, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51683, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51685, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51161, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51775, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51163, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51530, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51687, 51945, inc[,c(1)])
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
inc[,c(1)] <- ifelse(inc[,c(1)] == 51515, 51019, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51560, 51005, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51005, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51580, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51780, 51925, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51083, 51925, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51019, 51909, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 12025, 12086, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 46113, 46102, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55078, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55115, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 08014, 08013, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 04012, 04027, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 35006, 35007, inc[,c(1)])
inc <- inc %>% group_by(fips) %>% summarise_all(funs(sum))

inc <- inc[,c(1,3,60)]
inc <- merge(wateruse,inc,by="fips")
#adjust to wpu rather than wd
inc[,c(5)] <- inc[,c(27)]
inc[,c(4)] <- inc[,c(28)]
inc[,c(5)] <- inc[,c(5)]*inc[,c(4)]
inc[,c(6)] <- inc[,c(6)]/inc[,c(4)]
inc[,c(7)] <- inc[,c(7)]/inc[,c(5)]
inc[,c(8)] <- inc[,c(8)]/inc[,c(4)]
inc[,c(9)] <- inc[,c(9)]/inc[,c(15)]
inc[,c(10)] <- inc[,c(10)]/inc[,c(4)]
inc <- inc[,c(1:26)]
inc <- inc[,c(1:5,15,6,12,17,18,7,10,8,9,19,25,21,23,20,26,22,24,11,16,13,14)]
colnames(inc) <- c("fips","year","EastWest","pop","inc.in.thousands","AcresIrrig","DP.perUnit","DP.cu.ratio","DP.growth","DP.decay","IC.perUnit","AQ.perUnit","LS.perUnit","IR.perUnit","IC.growth","AQ.growth","LS.growth","IR.growth","IC.decay","AQ.decay","LS.decay","IR.decay","IC.cu.ratio","AQ.cu.ratio","LS.cu.ratio","IR.cu.ratio")

return(inc)
}
ssp1 <- popinc(ssp1, temp,df)

write.csv(ssp1, file="inputs_ssp1.csv")


ssp2 <- read.csv("pipc_ssp2.csv")
temp <- read.csv("pop_ssp2.csv")
ssp2 <- popinc(ssp2, temp,df)
write.csv(ssp2, file="inputs_ssp2.csv")


ssp3 <- read.csv("pipc_ssp3.csv")
temp <- read.csv("pop_ssp3.csv")
ssp3 <- popinc(ssp3, temp,df)
write.csv(ssp3, file="inputs_ssp3.csv")


ssp4 <- read.csv("pipc_ssp4.csv")
temp <- read.csv("pop_ssp4.csv")
ssp4 <- popinc(ssp4, temp,df)
write.csv(ssp4, file="inputs_ssp4.csv")


ssp5 <- read.csv("pipc_ssp5.csv")
temp <- read.csv("pop_ssp5.csv")
ssp5 <- popinc(ssp5, temp,df)
write.csv(ssp5, file="inputs_ssp5.csv")


#===================================================================================================================

library(readxl)
watertest <- read_excel("TotalWaterDemands2015.xlsx")
countypct <- read.csv("CountyWatershedPct.csv")
