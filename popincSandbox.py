"""
Script Name:    popincSandbox.py
Authors:        Pam Froemke (Rocky Mountain Research Station)
                Travis Warziniack (Rocky Mountain Research Station)

Description:    This is a sandbox file for playing with equations, pandas, and numpy.
                Calculaates projections of water use for the RPA 2020 update:
                    Calculates projected domestic and ag irrigation water use from the 2015 base data.
                    Saves output to a csv file.
                
Inputs:         "popinc_proj.csv"            Population projection data (Wear & Prestemon 2019)
                "wd2015.csv"                 Domestic water withdrawal data for 2015
                
Output:        "WithdrawalProjections.csv"  Domestic and ag withdrawal projections
              
Notes:          Wear & Prestemon data has issues with county FIPS codes. If you use the current counties feature class,
                some counties in Virginia and South Dakota come up missing after data joins.
                Metadata file (Metadata Water Demand.xlsx) is in the RPA-Water folder.
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

# Data Inputs
popnCSV = 'popinc_proj.csv'             # Population and Income projections (Wear & Prestemon)
wdCSV = 'wd2015.csv'                    # Water withdrawal data for 2015

# Data Outputs
WdFinal = "WithdrawalProjections.csv"   # Per-capita domestic and ag withdrawal projections 2015 to 2070
# ______________________________________________________________________________________________________________________

try:
    print('Starting analysis for water withdrawal projections...')
    # Change to the data directory and load the data.
    os.chdir(dataDir)
    print('    Loading the data...')
    dfPopn = pd.read_csv(popnCSV)
    dfWd = pd.read_csv(wdCSV)
    
    # #### Join the population and withdrawal data. ####################################################################
    # Select a subset of the population and income data for 2015.
    # The popinc_proj data has 5 values for each year in each county (one for each of the 5 ssp's).
    print('    Creating a subset of the population data for 2015...')
    # Make a copy of the data frame to work with.
    dfPop2015 = dfPopn.copy()
    # Select only the records for 2015 and sort them by year. There are 5 records per county.
    print('    Selecting the 2015 population records and saving to a csv file...')
    dfPop2015 = dfPop2015[dfPop2015.year == 2015].sort_values(by=['fips', 'ssp'], ascending=True)
    # The 'to_csv' command in the following line was causing dfPop2015 to not be a data frame and
    #   generated error with joining.
    # dfPop2015 = dfPop2015[dfPop2015.year == 2015].sort_values(by=['fips', 'ssp'], ascending=True).to_csv(pop2015_CSV)
    
    # Join the 2015 withdrawal data to the 2015 popinc data.
    print('    Join the water withdrawal data to the population data...')
    dfJoinPopWd = pd.merge(dfPopn, dfWd, on='fips', how='left').sort_values(by=['fips', 'year_x'], ascending=True)
    # Save to csv for viewing, debug, etc.
    # dfJoinPopWd.to_csv('dfJoinPopWd.csv')
    
    # #### Calculate withdrawals without climate impacts ###############################################################
    # Make a copy of the joined data. Once the code is finalized we can simply work with dfJoinPopWd, then save to a csv
    #   after calculation of withdrawal data.
    dfWd = dfJoinPopWd.copy()
    
    # Domestic water consumption
    print('    Domestic water consumption projections:')
    # Add and calculate a field for 2015 per-capita domestic withdrawals.
    print('        Calculating per-capita domestic withdrawals for 2015 (base data)...')
    # Domestic withdrawals in [units?]; population in [units?]. Data sorted by fips, then ssp. All values are < 1.
    dfWd["wpuDP0"] = dfWd["domestic"] / dfWd["pop"]
    # Estimate the growth function based on historic data
    print('        Calculating growth function...')
    # wpuDPt = [baseline 2015 per-cap wd]*[exp[domestic wd growth]*[# of years past 2015]]
    dfWd["wpuDPt"] = dfWd["wpuDP0"] * np.exp(dfWd["DP.growth"]*(2015-dfWd["year_x"]))
    dfWd["DPt"] = dfWd["wpuDPt"] * dfWd["pop"]
    
    # Agricultural water consumption
    print('    Agricultural irrigation water consumption projections:')
    # Agriculture needs to first either project acres or input the results from the last use chapters
    # Then estimate the irrigation depth. Ag withdrawals are then acres x wpu
    print('        Calculating per-capita irrigation withdrawals for the 2015 base data...')
    dfWd["wpuAG0"] = dfWd["irrigation"] / dfWd["IR.acres"]
    # Estimate the growth function based on historic data
    print('        Calculating growth function...')
    dfWd["wpuAGt"] = dfWd["wpuAG0"] * np.exp(dfWd["IR.growth"]*(2015-dfWd["year_x"]))
    dfWd["AGt"] = dfWd["wpuAGt"] * dfWd["IR.acres"]
    
    # #### Save the domestic and ag projections to a new csv file. #####################################################
    print('    Saving the final output to a csv file...')
    dfWd.to_csv(WdFinal, index=False, columns=['fips', 'state', 'county', 'year_x', 'ssp', 'wpuDP0', 'wpuDPt', 'DPt',
                                               'wpuAG0', 'wpuAGt', 'AGt'])
    
    print('Done!')
    
except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')