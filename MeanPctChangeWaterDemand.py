""""
Source Name:    MeanPctChangeWaterDemand.py
Tool Name:
Version:        ArcGIS Pro 3.02
                Python 3.9
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2022
Updates:
Description:    Calculate mean percent change in water demand from 2015 to 2070
                across all scenarios.
                    1. Group by
Required Args:
Optional Args:
Notes:
"""

# Import modules
print('Importing required modules...')
import arcpy
import os
from arcpy import env

env.overwriteOutput = True
arcpy.SpatialReference(
    "USA Contiguous Albers Equal Area Conic")

MAINFOLDER = \
    r'E:\_Projects\WaterDemand\WaterDemandProject'
# Location for final datasets
GDB = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')

# Data
# Raw input CSV water demand data
inputTable = os.path.join(
    MAINFOLDER,
    'DataWaterDemand',
    'MeanPctChange',
    'cons_and_withdrawal.csv')
# Raw water demand data imported to the gdb
outputTable = os.path.join(
    GDB,
    'ConsWithdrawal')
# Sectors of interest (domestic and thermoelectric)
sectorList = [
    'dp',
    'th']
rcpList = [
    '45',
    '85']
# Subset table for DP, RCP 4.5
tableDP45 = \
    'WithdrawalDP45'

# Set the workspace.
env.workspace = GDB

try:
    print(
        'Starting analysis for calculating mean percent change '
        'in water demand:')
    
    # # Import the CSV raw data table to the gdb.
    # print(
    #     '    Importing the raw data...')
    # arcpy.conversion.ExportTable(
    #     inputTable,
    #     outputTable)

    # For loops:
    # Select the sectors.
    for sct in sectorList:
        print(f'    Processing sector {sct}')
        #   Select the dp sector for RCP 4.5.
        #   Note - the 'year' field name is misspelled 'yearr' in the raw data.
        print(
            '    Selecting sector and scenario...')
        '"sector" = \'4\''
        '"sector" = ' + sct
        for rcp in rcpList:
            sqlDP45 = \
                "sector = sct " \
                "And scenario LIKE rcp " \
                "And yearr = 2015 " \
                "Or sector = sct " \
                "And scenario LIKE rcp " \
                "And yearr = 2070"
            arcpy.analysis.TableSelect(
                outputTable,
                tableDP45,
                sqlDP45)
    
        #       For each county, calculate % change from 2015 to 2070.
        #       ((2070wd-2015wd)/2015wd)*100
        #   Repeat for dp 8.5, th 4.5, and th 8.5.
    
    print('Done!')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')