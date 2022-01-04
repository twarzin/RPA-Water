"""
Script Name:    _______.py
Authors:        Pam Froemke (Rocky Mountain Research Station)
                Travis Warziniack (Rocky Mountain Research Station)

Description:    Projections of water use:
                Calculates projected domestic water use from the 2015 base data.
                
Required:       1. population projection csv data
                2. domestic water withdrawal csv data for 2015
                
Outputs:        1. Per-capita domestic water withdrawals for 2015
                2. Projections for per-capita domestic water withdrawals from 2016 to 2070
                
Notes:
"""
# Import modules
print('Loading modules and defining variables...')
print()
import pandas as pd
import numpy  # May not need this.
import os

# ______________________________________________________________________________________________________________________
# Locations
# Can be re-written for user input, then the resto of the script would not need modifications for different computers.
# on Pam's computer
dataDir = r'E:\_Projects\WaterDemand\ScriptsWaterDemand\RPA Water Scripts to Python\DataWaterDemandCSV'
# Data
# Can be re-written for user input, then the resto of the script would not need modifications for different data files.
popnCSV = 'popinc_proj.csv'  # input - Population and Income projections from Wear & Prestemon
wdCSV = 'wd2015.csv'  # input - water withdrawal data for 2015
pop2015_CSV = "pop2015.csv"  # output - 2015 population projection data
wpu_0_CSV = "wpu_0.csv"  # output - calculated 2015 domestic water withdrawals
joinPopWd = "PopWD2015.csv"  # output - 2015 water withdrawal data joined to 2015 population projection data
# ______________________________________________________________________________________________________________________

print('Starting analysis for per-capita withdrawal projections...')
# Change to the data directory.
os.chdir(dataDir)

# Load the data.
print('    Loading the data...')
dfPopn = pd.read_csv(popnCSV)
dfWd = pd.read_csv(wdCSV)

############ For the following code, we need a consistent ID. Every file's ID is different. ############################
# Select a subset of the population and income data for 2015.
# Check to see if this is correct--the popinc_proj data has 5 values for each year in each county (one for each of the 5 ssp's).
print('    Creating a subset of the population data for 2015...')
# Make a copy of the data frame to work with.
dfPop2015 = dfPopn.copy()
# Select only the records for 2015 and sort them by year. There are 5 records per county.
print('    Saving the 2015 population data frame to a csv file...')
dfPop2015 = dfPop2015[dfPop2015.year == 2015].sort_values(by=['fips', 'ssp'], ascending=True).to_csv(pop2015_CSV)
# Write 2015 population data frame to a CSV file.
# dfPop2015.to_csv(pop2015_CSV)  # This is just so I can view the data in Excel.

# Join the 2015 withdrawal data to the 2015 popinc data.
print('    Join the water withdrawal data to the population data...')
dfJoinPopWd = pd.merge(dfPop2015, dfWd, on='fips', how='left').sort_values(by=['fips', 'year_x'], ascending=True)
print('    Saving the joined data to a csv file...')
dfJoinPopWd.sort_values(by=['fips', 'year_x'], ascending=True)

# Calculate per-capita domestic water withdrawals for 2015.
# The wpu_0 output has ID numbers that don't maatch with the county IDs. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
print('    Calculating per-capita domestic water withdrawals for 2015...')
# Need to add fips ID to this output.
wpu_0 = dfWd["domestic"] / dfPop2015["pop"]  # Domestic withdrawals in ______; population in ________.
# Write the data frame to a CSV file.
wpu_0.to_csv(wpu_0_CSV)  # This is just so I can view the data in Excel.
########################################################################################################################

# Select
# Pseudo code:
#   1. Sort by fips and year
#   2. Filter by ssp:
#       1001  2015  57.25  ssp1
#       1001  2016  54.54  ssp1
#       1001  2017  59.25  ssp1
#       ....
#       1001  2070  95.25  ssp1
#   3. Calculate per-capita withdrawals for each fips and ssp
#

# Calculate future per-capita withdrawals.
#   ...or should this just be 'Calculate future withdrawals'?
#   Are we using the 2015 per-capita withdrawal value in all iterations of the equation? This assumes each person
#   will use the same amount of water each year through 2070. ?

# --------------------------------------------------------------------------------
# Python3 program to find compound interest for given values (geeksforgeeks.org).


def compound_interest(principle, rate, time):
    # Calculates compound interest for the specified number of years
    
    # For wpu, where
    #      principle = the 2015 baseline wpu_0 value,      or 'wpu2015'
    #      rate = current year's wpu_0 value + 1,          or 'wpu2015+1'
    #      time = number of years past 2015,               or 't':
    # So for the year 2020,
    #  compound_interest(2015, 2015 wpu_0 + 1,

    #  compound_interest(pow([add 1 to previous year's wpu_0 value]) [number of years past 2015])

    Amount = principle * (pow((1 + rate / 100), time))
    CI = Amount - principle
    print("Amount is", Amount)
    print("Compound interest is", CI)


# Driver Code - principle = $10,000, rate = 10.25%, time = 5 years
compound_interest(10000, 10.25, 5)
# --------------------------------------------------------------------------------

# Formula from Travis:
# wpuFuture(t) = (1 + wpu(t - 1)) ^ 2
# or wpuFuture(t) = (1 + [wpu of previous year]) ^ [number of years past 2015?]

# Define or find a lag or compound interest function.
# Try function 'shift' in pandas.
# For df.shift(), sort the popn data frame first, then shift all data down one row and fill the blanks with '0'
#   instead of 'NaN'.
dfPopnSort = dfPopn.sort_values(
    by=['fips', 'year'],
    ascending=True)

# Example of sorting the data frame before using shift function in pandas:
#   df['Data_lagged'] = (df.sort_values(by=['Date'], ascending=True).groupby(['Group'])['Data'].shift(1))

# Calculate difference in values from one row to the next.
# df['diff'] = df['sales'] - df.shift(1)['sales']
# Creates a sorted data frame of the population data and adds a column called 'diff', which is the
#   current row's pop value minus the previous row's pop value.
dfPopnSort['diff'] = dfPopnSort['pop'] - dfPopnSort.shift(1)['pop']

# Compares current day with 7 days prior.
# value_n = Day_N - Day_N-7


