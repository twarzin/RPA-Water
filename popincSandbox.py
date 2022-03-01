"""
Script Name:    popincSandbox.py
Authors:        Pam Froemke (Rocky Mountain Research Station)
                Travis Warziniack (Rocky Mountain Research Station)

Description:    Projections of water use for the RPA 2020 update:
                    Calculates projected domestic water use from the 2015 base data.
                    This is a sandbox file for playing with equations, pandas, and numpy.
                
Inputs:         1. "popinc_proj.csv"      Population projection data
                2. "wd2015.csv"           Domestic water withdrawal data for 2015
                
Outputs:        1. "PopWD2015.csv"        Joined data: 2015 water withdrawals + 2015 population projections
                2. "pop2015.csv"          Projections for per-capita domestic water withdrawals from 2016 to 2070
                3. "wpu_0.csv"            Per-capita domestic water withdrawals for 2015
              
Notes:          Wear & Prestemon data has issues with county FIPS codes. If you use the current counties feature class,
                some counties in Virginia and South Dakota come up missing after data joins.
                Metadata file (Metadata Water Demand.xlsx) is in the data folder
                (...RPA Water Scripts to Python\DataWaterDemandCSV).
"""
# Import modules
print('Loading modules and defining variables...')
print()
import pandas as pd
import numpy as np
import os

# ____ Locations and Data ______________________________________________________________________________________________
# Locations and Data variables can be re-written to accept user input, then the rest of the script would not need
#   modifications for different computers.

# Locations
# Pam
dataDir = r'E:\_Projects\WaterDemand\ScriptsWaterDemand\RPA-WaterScriptsToPython\DataWaterDemandCSV'
# Travis
# dataDir = r'D:\WaterDemand'

# Data
popnCSV = 'popinc_proj.csv'             # input  - Population and Income projections (Wear & Prestemon)
wdCSV = 'wd2015.csv'                    # input  - water withdrawal data for 2015
pop2015_CSV = "pop2015.csv"             # output - 2015 population baseline projection data
joinPopWd = "PopWD2015.csv"             # output - 2015 water withdrawal data joined to 2015-2070 population data
WdFinal = "WithdrawalProjections.csv"   # output - per-capita withdrawal projections 2015 to 2070
# ______________________________________________________________________________________________________________________
print('Starting analysis for water withdrawal projections...')
# Change to the data directory.
os.chdir(dataDir)

# Load the data.
print('    Loading the data...')
dfPopn = pd.read_csv(popnCSV)
dfWd = pd.read_csv(wdCSV)

# #### For this section of code, we need a consistent ID. Every file's ID is different. ################################
# Select a subset of the population and income data for 2015.
# The popinc_proj data has 5 values for each year in each county (one for each of the 5 ssp's).
print('    Creating a subset of the population data for 2015...')
# Make a copy of the data frame to work with.
dfPop2015 = dfPopn.copy()
# Select only the records for 2015 and sort them by year. There are 5 records per county.
print('    Selecting the 2015 population records and saving to a csv file...')
dfPop2015 = dfPop2015[dfPop2015.year == 2015].sort_values(by=['fips', 'ssp'], ascending=True)
# The 'to_csv' command in the following line was causing dfPop2015 to not be a data frame and generated error with joining.
# dfPop2015 = dfPop2015[dfPop2015.year == 2015].sort_values(by=['fips', 'ssp'], ascending=True).to_csv(pop2015_CSV)

# Join the 2015 withdrawal data to the 2015 popinc data.
print('    Join the water withdrawal data to the population data...')
dfJoinPopWd = pd.merge(dfPop2015, dfWd, on='fips', how='left').sort_values(by=['fips', 'year_x'], ascending=True)
# print('    Saving the joined data to a csv file...')
# dfJoinPopWd.sort_values(by=['fips', 'year_x'], ascending=True)
########################################################################################################################

# **** Pseudo code for domestic and ag withdrawal projections **********************************************************
# Project water withdrawals - pseudo code:
#   1. Sort by fips and year
#   2. Filter by ssp:
#       1001  2015  57.25  ssp1
#       1001  2016  54.54  ssp1
#       ....
#       1001  2070  95.25  ssp1
#   3. Calculate per-capita withdrawals for each fips and ssp
# **********************************************************************************************************************

# ---- Calculate withdrawals without climate impacts -------------------------------------------------------------------
# Make a copy of the joined data. (may not need this step?)
dfWd = dfJoinPopWd.copy()

# Domestic water consumption
print('Domestic water consumption projections:')
# Calculate per-capita domestic water withdrawals for 2015.
print('    Calculating per-capita withdrawals for 2015 (base data)...')
# Add and calculate a 'wpuDP0' field for per-capita domestic withdrawals.
# Need to add fips ID to this output? It has ID numbers that don't match with the county IDs.
dfWd["wpuDP0"] = dfWd["domestic"] / dfWd["pop"]  # Domestic withdrawals in [units?]; population in [units?].

# This section not working yet? The wpuDP0 and wpuDPt values are equal for all records.
# Estimate the growth function based on historic data
print('    Calculating growth function...')
dfWd["wpuDPt"] = dfWd["wpuDP0"] * np.exp(dfWd["DP.growth"]*(2015-dfWd["year_x"]))
dfWd["DPt"] = dfWd["wpuDPt"] * dfWd["pop"]

# to be completed.........
# # Agricultural water consumption
# # Agriculture needs to first either project acres or input the results from the last use chapters
# # Then estimate the irrigation depth. Ag withdrawals are then acres x wpu
# dfWd["wpuAG0"] = dfWd["irrigation"] / dfWd["IR.acres"]
# # To do: Estimate the growth function based on historic data
# dfWd["wpuAGt"] = dfWd["wpuAG0"] * np.exp(dfWd["IR.growth"]*(2015-dfWd["year_x"]))
# dfWd["DPt"] = dfWd["wpuDPt"] * dfWd["pop"]

# Save all results to a new csv file.
dfWd.to_csv(WdFinal)
# ----------------------------------------------------------------------------------------------------------------------

# ^^^^ pandas shift function and other notes ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# # Define or find a lag or compound interest function.
# # Try function 'shift' in pandas.
# # For df.shift(), sort the popn data frame first, then shift all data down one row and fill the blanks with '0'
# #   instead of 'NaN'.
# dfPopnSort = dfPopn.sort_values(
#     by=['fips', 'year'],
#     ascending=True)
#
# # Example of sorting the data frame before using shift function in pandas:
# #   df['Data_lagged'] = (df.sort_values(by=['Date'], ascending=True).groupby(['Group'])['Data'].shift(1))
#
# # Calculate difference in values from one row to the next.
# # df['diff'] = df['sales'] - df.shift(1)['sales']
# # Creates a sorted data frame of the population data and adds a column called 'diff', which is the
# #   current row's pop value minus the previous row's pop value.
# dfPopnSort['diff'] = dfPopnSort['pop'] - dfPopnSort.shift(1)['pop']
#
# # Compares current day with 7 days prior.
# # value_n = Day_N - Day_N-7
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


