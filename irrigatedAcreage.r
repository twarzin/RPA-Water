# create acreage projections for 2020 RPA Water Assessment
# This code reads in projections of acreage with 5 year intervals and fills in the
# in-between years

rm(list = ls())  # clears memory

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
df2015$acres <- df2015$ac2015




df2$acres <- mapply(df1$acres, function(x) if(df1$year == 2015) return(1*df1$ac2015))


else if (df$year == 2016) {df$pr_acre_2015+1*(df$pr_acre_2020-df$pr_acre_2015)/5}
else if (df$year == 2017) {df$pr_acre_2015+2*(df$pr_acre_2020-df$pr_acre_2015)/5}
else if (df$year == 2018) {df$pr_acre_2015+3*(df$pr_acre_2020-df$pr_acre_2015)/5}
else if (df$year == 2019) {df$pr_acre_2015+4*(df$pr_acre_2020-df$pr_acre_2015)/5}

else if (df$year == 2020) {df$pr_acre_2020} 
else if (df$year == 2021) {df$pr_acre_2020+1*(df$pr_acre_2025-df$pr_acre_2020)/5}
else if (df$year == 2022) {df$pr_acre_2020+2*(df$pr_acre_2025-df$pr_acre_2020)/5}
else if (df$year == 2023) {df$pr_acre_2020+3*(df$pr_acre_2025-df$pr_acre_2020)/5}
else if (df$year == 2024) {df$pr_acre_2020+4*(df$pr_acre_2025-df$pr_acre_2020)/5}

else if (df$year == 2025) {df$pr_acre_2025}
else if (df$year == 2026) {df$pr_acre_2025+1*(df$pr_acre_2030-df$pr_acre_2025)/5}
else if (df$year == 2027) {df$pr_acre_2025+2*(df$pr_acre_2030-df$pr_acre_2025)/5}
else if (df$year == 2028) {df$pr_acre_2025+3*(df$pr_acre_2030-df$pr_acre_2025)/5}
else if (df$year == 2029) {df$pr_acre_2025+4*(df$pr_acre_2030-df$pr_acre_2025)/5}

else if (df$year == 2030) {df$pr_acre_2030}
else if (df$year == 2031) {df$pr_acre_2030+1*(df$pr_acre_2035-df$pr_acre_2030)/5}
else if (df$year == 2032) {df$pr_acre_2030+2*(df$pr_acre_2035-df$pr_acre_2030)/5}
else if (df$year == 2033) {df$pr_acre_2030+3*(df$pr_acre_2035-df$pr_acre_2030)/5}
else if (df$year == 2034) {df$pr_acre_2030+4*(df$pr_acre_2035-df$pr_acre_2030)/5}

else if (df$year == 2035) {df$pr_acre_2035}
else if (df$year == 2036) {df$pr_acre_2035+1*(df$pr_acre_2040-df$pr_acre_2035)/5}
else if (df$year == 2037) {df$pr_acre_2035+2*(df$pr_acre_2040-df$pr_acre_2035)/5}
else if (df$year == 2038) {df$pr_acre_2035+3*(df$pr_acre_2040-df$pr_acre_2035)/5}
else if (df$year == 2039) {df$pr_acre_2035+4*(df$pr_acre_2040-df$pr_acre_2035)/5}

else if (df$year == 2040) {df$pr_acre_2040}
else if (df$year == 2045) {df$pr_acre_2045}
else if (df$year == 2050) {df$pr_acre_2050}
else if (df$year == 2055) {df$pr_acre_2055}
else if (df$year == 2060) {df$pr_acre_2060}
else if (df$year == 2065) {df$pr_acre_2065}
else if (df$year == 2070) {df$pr_acre_2070}

