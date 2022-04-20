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

df2016$acres <- df2016$ac2015+1*(df2016$ac2020-df2016$ac2015)/5
df2017$acres <- df2017$ac2015+2*(df2017$ac2020-df2017$ac2015)/5
df2018$acres <- df2018$ac2015+3*(df2018$ac2020-df2018$ac2015)/5
df2019$acres <- df2019$ac2015+4*(df2019$ac2020-df2019$ac2015)/5

df2021$acres <- df2021$ac2020+1*(df2021$ac2025-df2021$ac2020)/5
df2022$acres <- df2022$ac2020+2*(df2022$ac2025-df2022$ac2020)/5
df2023$acres <- df2023$ac2020+3*(df2023$ac2025-df2023$ac2020)/5
df2024$acres <- df2024$ac2020+4*(df2024$ac2025-df2024$ac2020)/5

df2026$acres <- df2026$ac2025+1*(df2026$ac2030-df2026$ac2025)/5
df2027$acres <- df2027$ac2025+2*(df2027$ac2030-df2027$ac2025)/5
df2028$acres <- df2028$ac2025+3*(df2028$ac2030-df2028$ac2025)/5
df2029$acres <- df2029$ac2025+4*(df2029$ac2030-df2029$ac2025)/5

df2031$acres <- df2031$ac2030+1*(df2031$ac2035-df2031$ac2030)/5
df2032$acres <- df2032$ac2030+2*(df2032$ac2035-df2032$ac2030)/5
df2033$acres <- df2033$ac2030+3*(df2033$ac2035-df2033$ac2030)/5
df2034$acres <- df2034$ac2030+4*(df2034$ac2035-df2034$ac2030)/5

df2036$acres <- df2036$ac2035+1*(df2036$ac2040-df2036$ac2035)/5
df2037$acres <- df2037$ac2035+2*(df2037$ac2040-df2037$ac2035)/5
df2038$acres <- df2038$ac2035+3*(df2038$ac2040-df2038$ac2035)/5
df2039$acres <- df2039$ac2035+4*(df2039$ac2040-df2039$ac2035)/5

df2041$acres <- df2041$ac2040+1*(df2041$ac2045-df2041$ac2040)/5
df2042$acres <- df2042$ac2040+2*(df2042$ac2045-df2042$ac2040)/5
df2043$acres <- df2043$ac2040+3*(df2043$ac2045-df2043$ac2040)/5
df2044$acres <- df2044$ac2040+4*(df2044$ac2045-df2044$ac2040)/5

df2046$acres <- df2046$ac2045+1*(df2046$ac2050-df2046$ac2045)/5
df2047$acres <- df2047$ac2045+2*(df2047$ac2050-df2047$ac2045)/5
df2048$acres <- df2048$ac2045+3*(df2048$ac2050-df2048$ac2045)/5
df2049$acres <- df2049$ac2045+4*(df2049$ac2050-df2049$ac2045)/5

df2051$acres <- df2051$ac2050+1*(df2051$ac2055-df2051$ac2050)/5
df2052$acres <- df2052$ac2050+2*(df2052$ac2055-df2052$ac2050)/5
df2053$acres <- df2053$ac2050+3*(df2053$ac2055-df2053$ac2050)/5
df2054$acres <- df2054$ac2050+4*(df2054$ac2055-df2054$ac2050)/5

df2056$acres <- df2056$ac2055+1*(df2056$ac2060-df2056$ac2055)/5
df2057$acres <- df2057$ac2055+2*(df2057$ac2060-df2057$ac2055)/5
df2058$acres <- df2058$ac2055+3*(df2058$ac2060-df2058$ac2055)/5
df2059$acres <- df2059$ac2055+4*(df2059$ac2060-df2059$ac2055)/5

df2061$acres <- df2061$ac2060+1*(df2061$ac2065-df2061$ac2060)/5
df2062$acres <- df2062$ac2060+2*(df2062$ac2065-df2062$ac2060)/5
df2063$acres <- df2063$ac2060+3*(df2063$ac2065-df2063$ac2060)/5
df2064$acres <- df2064$ac2060+4*(df2064$ac2065-df2064$ac2060)/5

df2066$acres <- df2066$ac2065+1*(df2066$ac2070-df2066$ac2065)/5
df2067$acres <- df2067$ac2065+2*(df2067$ac2070-df2067$ac2065)/5
df2068$acres <- df2068$ac2065+3*(df2068$ac2070-df2068$ac2065)/5
df2069$acres <- df2069$ac2065+4*(df2069$ac2070-df2069$ac2065)/5

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

keeps <- c("fips","year","ssp","acres") 

acre.data <- dfall[,names(dfall) %in% keeps]
write.csv(acre.data, file="1_BaseData/acredata-use.csv", row.names = FALSE)
