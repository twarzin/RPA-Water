"""
Source Name:    _________.py
Tool Name:
Version:        ArcGIS Pro 2.8
                Python 3.9
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2022  
Updates:
Description:
Required Args:
Optional Args:
Notes:
"""

# Import modules
print('Importing required modules...')
import arcpy
import os
import sys, string, glob
from arcpy import env
from arcpy.sa import *

# Check for licenses
print('Checking for licenses...')
arcpy.CheckOutExtension("Spatial")

env.overwriteOutput = True
COORDS = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
MAINFOLDER = \
    r'E:\_Projects\WaterDemand\WaterDemandProject'
# Location for final datasets
GDB = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')
fc_wdPctChg = 'MeanPctChangeWithdrawals'
joinFieldCounties = 'FIPS'
joinFieldWithdrawalsFips = 'fips'

env.workspace = GDB

try:
    sct = 'ir'
    rcp = '45'
    pctField = "PctChange_" + sct + rcp
    tableSubset = "Withdrawals_ir45_MeanPctChg_2015_2070"
    joinFieldMPC = ["MEAN_" + pctField]
    arcpy.management.JoinField(
        in_data=fc_wdPctChg,
        in_field=joinFieldCounties,
        join_table=tableSubset,
        join_field=joinFieldWithdrawalsFips,
        fields=[joinFieldMPC])
    
    # arcpy.management.JoinField(
    #     "E:\\_Projects\\WaterDemand\\WaterDemandProject\\WaterDemandProject.gdb\\MeanPctChangeWithdrawals",
    #     'FIPS',
    #     "E:\\_Projects\\WaterDemand\\WaterDemandProject\\WaterDemandProject.gdb\\Withdrawals_ir45_MeanPctChg_2015_2070",
    #     'fips', 'MEAN_PctChange_ir45')
    
    print('Done!')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')
