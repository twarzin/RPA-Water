# create acreage projections for 2020 RPA Water Assessment
# This code reads in projections of acreage with 5 year intervals and fills in the
# in-between years

rm(list = ls())  # clears memory

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)  # Has the pipe operator %>%.
library(data.table)

# for Travis desktop:
setwd("D:/5_RPA/Demand model")
acre.data <- read.csv('1_BaseData/acredata-updated.csv', header=TRUE)

# read in pop data since it has all the years
pop.inc <- read.csv("1_BaseData/popinc_proj.csv")

acres <- rename(acre.data, 
                ac2015 = pr_acre_2015,
                ac2020 = pr_acre_2020,
                ac2025 = pr_acre_2025,
                ac2030 = pr_acre_2030,
                ac2035 = pr_acre_2035,
                ac2040 = pr_acre_2040,
                ac2045 = pr_acre_2045,
                ac2050 = pr_acre_2050,
                ac2055 = pr_acre_2055,
                ac2060 = pr_acre_2060,
                ac2065 = pr_acre_2065,
                ac2070 = pr_acre_2070
                  )

df <- merge(acres, pop.inc)

df2015 <- df[df$year == 2015, ] 
df2016 <- df[df$year == 2016, ] 
df2017 <- df[df$year == 2017, ] 
df2018 <- df[df$year == 2018, ] 
df2019 <- df[df$year == 2019, ] 
df2020 <- df[df$year == 2020, ] 
df2021 <- df[df$year == 2021, ] 
df2022 <- df[df$year == 2022, ] 
df2023 <- df[df$year == 2023, ] 
df2024 <- df[df$year == 2024, ] 
df2025 <- df[df$year == 2025, ] 
df2026 <- df[df$year == 2026, ] 
df2027 <- df[df$year == 2027, ] 
df2028 <- df[df$year == 2028, ] 
df2029 <- df[df$year == 2029, ] 
df2030 <- df[df$year == 2030, ] 
df2031 <- df[df$year == 2031, ] 
df2032 <- df[df$year == 2032, ] 
df2033 <- df[df$year == 2033, ] 
df2034 <- df[df$year == 2034, ] 
df2035 <- df[df$year == 2035, ] 
df2036 <- df[df$year == 2036, ] 
df2037 <- df[df$year == 2037, ] 
df2038 <- df[df$year == 2038, ] 
df2039 <- df[df$year == 2039, ] 
df2040 <- df[df$year == 2040, ] 
df2041 <- df[df$year == 2041, ] 
df2042 <- df[df$year == 2042, ] 
df2043 <- df[df$year == 2043, ] 
df2044 <- df[df$year == 2044, ] 
df2045 <- df[df$year == 2045, ] 
df2046 <- df[df$year == 2046, ] 
df2047 <- df[df$year == 2047, ] 
df2048 <- df[df$year == 2048, ] 
df2049 <- df[df$year == 2049, ] 
df2050 <- df[df$year == 2050, ] 
df2051 <- df[df$year == 2051, ] 
df2052 <- df[df$year == 2052, ] 
df2053 <- df[df$year == 2053, ] 
df2054 <- df[df$year == 2054, ] 
df2055 <- df[df$year == 2055, ] 
df2056 <- df[df$year == 2056, ] 
df2057 <- df[df$year == 2057, ] 
df2058 <- df[df$year == 2058, ] 
df2059 <- df[df$year == 2059, ] 
df2060 <- df[df$year == 2060, ] 
df2061 <- df[df$year == 2061, ] 
df2062 <- df[df$year == 2062, ] 
df2063 <- df[df$year == 2063, ] 
df2064 <- df[df$year == 2064, ] 
df2065 <- df[df$year == 2065, ] 
df2066 <- df[df$year == 2066, ] 
df2067 <- df[df$year == 2067, ] 
df2068 <- df[df$year == 2068, ] 
df2069 <- df[df$year == 2069, ] 
df2070 <- df[df$year == 2070, ] 

# projections are every five years
df2015$acres <- df2015$ac2015
df2020$acres <- df2020$ac2020
df2025$acres <- df2025$ac2025
df2030$acres <- df2030$ac2030
df2035$acres <- df2035$ac2035
df2040$acres <- df2040$ac2040
df2045$acres <- df2045$ac2045
df2050$acres <- df2050$ac2050
df2055$acres <- df2055$ac2055
df2060$acres <- df2060$ac2060
df2065$acres <- df2065$ac2065
df2070$acres <- df2070$ac2070

df2016$acres <- df$pr_acre_2015+1*(df$pr_acre_2020-df$pr_acre_2015)/5
df2017$acres <- df$pr_acre_2015+2*(df$pr_acre_2020-df$pr_acre_2015)/5
df2018$acres <- df$pr_acre_2015+3*(df$pr_acre_2020-df$pr_acre_2015)/5
df2019$acres <- df$pr_acre_2015+4*(df$pr_acre_2020-df$pr_acre_2015)/5

df2021$acres <- df$pr_acre_2020+1*(df$pr_acre_2025-df$pr_acre_2020)/5
df2022$acres <- df$pr_acre_2020+2*(df$pr_acre_2025-df$pr_acre_2020)/5
df2023$acres <- df$pr_acre_2020+3*(df$pr_acre_2025-df$pr_acre_2020)/5
df2024$acres <- df$pr_acre_2020+4*(df$pr_acre_2025-df$pr_acre_2020)/5

df2026$acres <- df$pr_acre_2025+1*(df$pr_acre_2030-df$pr_acre_2025)/5
df2027$acres <- df$pr_acre_2025+2*(df$pr_acre_2030-df$pr_acre_2025)/5
df2028$acres <- df$pr_acre_2025+3*(df$pr_acre_2030-df$pr_acre_2025)/5
df2029$acres <- df$pr_acre_2025+4*(df$pr_acre_2030-df$pr_acre_2025)/5

df2031$acres <- df$pr_acre_2030+1*(df$pr_acre_2035-df$pr_acre_2030)/5
df2032$acres <- df$pr_acre_2030+2*(df$pr_acre_2035-df$pr_acre_2030)/5
df2033$acres <- df$pr_acre_2030+3*(df$pr_acre_2035-df$pr_acre_2030)/5
df2034$acres <- df$pr_acre_2030+4*(df$pr_acre_2035-df$pr_acre_2030)/5

df2036$acres <- df$pr_acre_2035+1*(df$pr_acre_2040-df$pr_acre_2035)/5
df2037$acres <- df$pr_acre_2035+2*(df$pr_acre_2040-df$pr_acre_2035)/5
df2038$acres <- df$pr_acre_2035+3*(df$pr_acre_2040-df$pr_acre_2035)/5
df2039$acres <- df$pr_acre_2035+4*(df$pr_acre_2040-df$pr_acre_2035)/5

df2041$acres <- df$pr_acre_2040+1*(df$pr_acre_2045-df$pr_acre_2040)/5
df2042$acres <- df$pr_acre_2040+2*(df$pr_acre_2045-df$pr_acre_2040)/5
df2043$acres <- df$pr_acre_2040+3*(df$pr_acre_2045-df$pr_acre_2040)/5
df2044$acres <- df$pr_acre_2040+4*(df$pr_acre_2045-df$pr_acre_2040)/5

df2046$acres <- df$pr_acre_2045+1*(df$pr_acre_2050-df$pr_acre_2045)/5
df2047$acres <- df$pr_acre_2045+2*(df$pr_acre_2050-df$pr_acre_2045)/5
df2048$acres <- df$pr_acre_2045+3*(df$pr_acre_2050-df$pr_acre_2045)/5
df2049$acres <- df$pr_acre_2045+4*(df$pr_acre_2050-df$pr_acre_2045)/5

df2051$acres <- df$pr_acre_2050+1*(df$pr_acre_2055-df$pr_acre_2050)/5
df2052$acres <- df$pr_acre_2050+2*(df$pr_acre_2055-df$pr_acre_2050)/5
df2053$acres <- df$pr_acre_2050+3*(df$pr_acre_2055-df$pr_acre_2050)/5
df2054$acres <- df$pr_acre_2050+4*(df$pr_acre_2055-df$pr_acre_2050)/5

df2056$acres <- df$pr_acre_2055+1*(df$pr_acre_2060-df$pr_acre_2055)/5
df2057$acres <- df$pr_acre_2055+2*(df$pr_acre_2060-df$pr_acre_2055)/5
df2058$acres <- df$pr_acre_2055+3*(df$pr_acre_2060-df$pr_acre_2055)/5
df2059$acres <- df$pr_acre_2055+4*(df$pr_acre_2060-df$pr_acre_2055)/5

df2061$acres <- df$pr_acre_2060+1*(df$pr_acre_2065-df$pr_acre_2060)/5
df2062$acres <- df$pr_acre_2060+2*(df$pr_acre_2065-df$pr_acre_2060)/5
df2063$acres <- df$pr_acre_2060+3*(df$pr_acre_2065-df$pr_acre_2060)/5
df2064$acres <- df$pr_acre_2060+4*(df$pr_acre_2065-df$pr_acre_2060)/5

df2066$acres <- df$pr_acre_2065+1*(df$pr_acre_2070-df$pr_acre_2065)/5
df2067$acres <- df$pr_acre_2065+2*(df$pr_acre_2070-df$pr_acre_2065)/5
df2068$acres <- df$pr_acre_2065+3*(df$pr_acre_2070-df$pr_acre_2065)/5
df2069$acres <- df$pr_acre_2065+4*(df$pr_acre_2070-df$pr_acre_2065)/5

dfall <- rbind(df2015, 
      df2016,
      df2017,
      df2018,
      df2019,

      df2020,
      df2021,
      df2022,
      df2023,
      df2024,
      df2025,
      df2026,
      df2027,
      df2028,
      df2029,
      
      df2030,
      df2031,
      df2032,
      df2033,
      df2034,
      df2035,
      df2036,
      df2037,
      df2038,
      df2039,
      
      df2040,
      df2041,
      df2042,
      df2043,
      df2044,
      df2045,
      df2046,
      df2047,
      df2048,
      df2049,
      
      df2050,
      df2051,
      df2052,
      df2053,
      df2054,
      df2055,
      df2056,
      df2057,
      df2058,
      df2059,
      
      df2060,
      df2061,
      df2062,
      df2063,
      df2064,
      df2065,
      df2066,
      df2067,
      df2068,
      df2069,
      
      df2070)
 

