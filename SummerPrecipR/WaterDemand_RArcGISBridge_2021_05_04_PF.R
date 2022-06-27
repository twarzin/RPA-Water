# Updates (Pam Froemke)
#   2021 April - Created document outline.
#   2021 April 16 - Replaced periods in filenames with underscores.
#   2021 May 3 - Created document outline by adding dashes to the main sections of the script.
#   2021 May 4 - Added code for the R-ArcGIS Bridge function so I can add this to a geoprocessing tool.

# ArcGIS wrapping function ----------------------------------------------------------------------------------------
tool_exec <- function(in_params, out_params){
  workDir <- in_params[[1]]     # Working directory, location of input/output files
  outCSV <- out_params[[1]]     # Output csv

# Remove objects from environment. --------------------------------------------------------------------------------
#    Clears out memory, stored values, etc.
  rm(list = ls())

# Set working directory to file location.
#    Use forward slashes in the path (the escape character is '\').
  setwd("E:/WaterDemand/WaterDemandProject/DataWaterDemand") # Pam's WorkDir

# Import libraries. -----------------------------------------------------------------------------------------------
  library(tidyr)
  library(ggplot2)    # Plotting graphs
  library(reshape2)
  library(dplyr)      # Provides access to the pipe operator '%>%'.
  library(data.table)

# Define models. --------------------------------------------------------------------------------------------------
# *  Define models via user input. --------------------------------------------------------------------------------
#    The symbols '<-' define an 'assignment operator', shortcut keys are 'Alt-'. Read as 'gets' (ex: x gets y).
# carbon <- readline(prompt="Enter carbon assumption (45 or 85): ")
# gcm <- readline(prompt="Enter global climate model (cnrm_c5, hadgem, ipsl_cm5a, mri_cgcm3, noresm, or base): ")
# *  Define models manually. --------------------------------------------------------------------------------------
#    carbon=45 or carbon=85 depending on which carbon climate model
  carbon <- 85  # Assign the value 85 to the 'carbon' variable.
#    Set the global climate model (gcm).
#    Options are: "cnrm_c5", "hadgem","ipsl_cm5a","mri_cgcm3","noresm", and "base".
  gcm <- "noresm"  # Assign the value '85'noresm' to the 'gcm' variable.

# Load population and withdrawal data. ----------------------------------------------------------------------------
#    Population by FIPS: fields are ID (x), fips, year, pop, ssp, inc; 860,440 records.
  pop_inc <- read.csv("popinc_proj.csv")
  head(pop_inc)
#    Water withdrawals in 2015 (3,075 records):
#    fields are x, fips, state, county, year, and 22 data fields
  wd_2015 <- read.csv("wd2015.csv")
# cu.ratios <- read.csv("consumptive use.csv")

# Choose common variables in withdrawal data. ---------------------------------------------------------------------
#    (handy for merging later).
#    Select 4 fields from the wd_2015 data and save them in a dataset called 'ew'.
  ew <- wd_2015 %>%
    select(fips, 
           EastWest, 
           DP.growth, 
           DP.decay)

# water <- merge(wd_2015, cu.ratios, by="fips")
# wd_2015$PD <- wd_2015$Public + wd_2015$Domestic

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

# Load precip data based on values of "carbon" & "gcm". -----------------------------------------------------------
#    (These statements define the precip data based on the 'carbon & 'gcm' variable values.)
#    precip has 672 records, fields are date, year (data range from 2015 to 2070),
#    month (data include all 12), and 3,110 columns (one for each fips).
#    F in the statements below indicates the logical operator 'FALSE'. Can be spelled
#    out--in all caps--or just use F. Same thing with TRUE or T.
  if(carbon==45 & gcm=="cnrm_c5"){
    precip <-read.csv("CountyPrecip/Monthly/pr_CNRM_CM5rcp45_month.csv", check.names=F)
  }
  if(carbon==85 & gcm=="cnrm_c5"){
    precip <-read.csv("CountyPrecip/Monthly/pr_CNRM_CM5rcp85_month.csv", check.names=F)
  }
  if(carbon==45 & gcm=="hadgem"){
    precip <-read.csv("CountyPrecip/Monthly/pr_HadGEM2_ES365rcp45_month.csv", check.names=F)
  }
  if(carbon==85 & gcm=="hadgem"){
    precip <-read.csv("CountyPrecip/Monthly/pr_HadGEM2_ES365rcp85_month.csv", check.names=F)
  }
  if(carbon==45 & gcm=="ipsl_cm5a"){
    precip <-read.csv("CountyPrecip/Monthly/pr_IPSL_CM5A_MRrcp45_month.csv", check.names=F)
  }
  if(carbon==85 & gcm=="ipsl_cm5a"){
    precip <-read.csv("CountyPrecip/Monthly/pr_IPSL_CM5A_MRrcp85_month.csv", check.names=F)
  }
  if(carbon==45 & gcm=="mri_cgcm3"){
    precip <-read.csv("CountyPrecip/Monthly/pr_MRI_CGCM3rcp45_month.csv", check.names=F)
  }
  if(carbon==85 & gcm=="mri_cgcm3"){
    precip <-read.csv("CountyPrecip/Monthly/pr_MRI_CGCM3rcp85_month.csv", check.names=F)
  }
  if(carbon==45 & gcm=="noresm"){
    precip <-read.csv("CountyPrecip/Monthly/pr_NorESM1_Mrcp45_month.csv", check.names=F)
  }
  if(carbon==85 & gcm=="noresm"){
    precip <-read.csv("CountyPrecip/Monthly/pr_NorESM1_Mrcp85_month.csv", check.names=F)
  }

# Make a new table where columns = FIPS, rows = year --------------------------------------------------------------
#    Same as the original precip, except the date column is gone (3,109 columns).
#    Select from precip: all rows and all columns except the first one ('-1' = 'date').
  precip <- precip[,-1]

# Summarize summer precipitation. ---------------------------------------------------------------------------------
#    Sum ppt data by year (56 records, 3109 columns).
#    The month column values are all 39, which is the sum of 4, 5, 6, 7, 8, and 9.
#    The fips columns have the sum of annual ppt for each county, for every year from 2015 to 2070.
  growing_precip <- subset(precip, Month >= 4 & Month <= 9) %>%
    group_by(Year) %>%
    summarise_all(sum)
# Eliminate the month column from the summper.precip data.
#    The values are no longer useful since they all equal 39.
#    Select all rows and all columns except the second one ('-2' = 'month').
  growing_precip <- growing_precip[,-2]

# Transpose summer precip rows and columns. -----------------------------------------------------------------------
#    Transposes rows and columnsin the growing_precip table so that FIPS are in rows and years are in columns.
#    Values in the first row will be the years and field names are assigned as 'V[n]'.
#    Transpose columns and rows in growing_precip, save as summer_t.
  summer_t <- t(growing_precip)
#    Not sure what this does - summer_t looks the same.
  summer_t <- as.data.frame(summer_t)
#    Renames columns using values in the 1st row of data ([1, ] references the 1st row).
  colnames(summer_t) = summer_t[1, ]
#    Now that you've renamed the columns, eliminate the first row with the year values.
#    Removes the 1st-row year labels (selects all rows except the first and all columns).
  summer_t = summer_t[-1, ]
#    Exports to CSV file.
#    Possibly the loop section would work better in Python
#    This write.csv statement produces output where the column names are in the first row instead of actually
#    naming the columns.
  write.csv(summer_t, file = "SummerT.csv", col.names = T) # File for importing into Excel/ArcGIS

# This section transferred to Python script (output is the SummerT gdb table): ------------------------------------
# #   Saves the transposed data back to growing_precip. Both are now the same.
# growing_precip <- summer_t
# # Start loop: Copy data for 1 year, add & calculate fields for 'fips' & 'year'.
# #    Sample Python looping structure: list years; for yr in years: do the following...
# #    Years range from 2015 to 2070.
# #    Output tables would be saved to a folder, or appended to a final dataset at the end of the loop.
# #    Select just the data for 2015 (or yr) and save it to 'sy15' (or sy[yr]).
# #    New fields: fips and year 2015 ppt data.
# sy15 <- growing_precip %>% select('2015') # Or "sy[yr] <- growing_precip %>% select(yr)"
# #    Renames the '2015' column to 'growing_precip'.
# colnames(sy15) = 'growing_precip' # Or "colnames(sy[yr]) = 'growing_precip'"
# #    A '$' refers to the element of sy15 named fips. Adds a 3rd column called 'fips' and sets it equal to the row names.
# sy15$fips <- rownames(sy15) # or "sy[yr]$fips <- rownames(sy[yr])"
# #    The element of sy15 named year. Adds a 4th column called 'year' and sets it equal to 2015.
# sy15$year <- 2015 # Or "sy[yr]$year <- yr"
# #    Could end the loop here (option 1 of 2).
# 
# # Loop
# #    Define the range of years
# years <- 2015:2020
# 
# #    For every year in years, do the following
# for (i in years){
#   #    Select the data for one year
#   #sy <- paste0('sy', i) # Append the year to the prefix 'sy'.
#   #print (sy)
#   sy <- growing_precip %>% select(i)
#   colnames(sy) = 'growing_precip'
#   sy$fips <- rownames(sy)
#   sy$year <- i
#   write.csv(sy, paste0('sy', i, '.csv'), col.names = T)
# }
# 
# # Append all the individual year tables into one dataset.
# #    Make a list of all the years in the output folder (allYears = [sy15, sy16, sy17, etc.]).
# sp_all <- rbind(sy15, sy16) # The year column includes growing_precip data for 2015 and 2016.
# #    Could end the loop here (option 2 of 2).
# 
# # Generate a 'base' table for the 2015 summer precip data.
# #    Not sure why this step is needed.
# #    Load sy15 data into 'base'. Select the columns fips and growing_precip.
# base <- sy15 %>% select(fips, growing_precip)  # base year precip to calc changes
# colnames(base) = c("fips", "s_precp0")  # Defines the field names as 'fips' and 's.precp0'.
# 
# # Add the 'base' 2015 data to sp_all (join the base s_precp0 field to sp_all by fips).
# #    Why do this when a column for 2015 already exists in sp_all?
# #    In Python, could add a field and make it equal to 2015.
# sp_all <- merge(sp_all, base, by="fips")  # The 's_precp0' data is added to sp_all.
# 
# # Add and calculate a 'delta.sprecip' field for change in precip values.
# #    Calculate change in summer precipitation from 2015 to 2016:
# #    'delta_sprecip' = 'growing_precip - 's_precp0'
# sp_all$delta_sprecip <- sp_all$growing_precip - sp_all$s_precp0

# Clean up --------------------------------------------------------------------------------------------------------
#    Clear packages
detach("packages:datasets", unload = TRUE)  # For base

#    Clear plots
dev.off()  # But only if there is a plot

#    Clear console
cat("\014")  # Same as Ctrl-L

#    Clear mind :o)

# End of ArcGIS wrapping.
}

