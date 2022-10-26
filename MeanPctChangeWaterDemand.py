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
COORDS = arcpy.SpatialReference(
    "USA Contiguous Albers Equal Area Conic")

MAINFOLDER = \
    r'E:\_Projects\WaterDemand\WaterDemandProject'
# Location for final datasets
GDB = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')

# Data
# Raw CSV water demand data
inputTable = "E:\\_Projects\\WaterDemand\\WaterDemandProject\\DataWaterDemand" \
             "\\MeanPctChange\\cons_and_withdrawal.csv"
# Raw water demand data imported to the gdb
outputTable = "E:\\_Projects\\WaterDemand\\WaterDemandProject" \
              "\\WaterDemandProject.gdb\\ConsWithdrawal"

# Set the workspace.
env.workspace = GDB

try:
    print(
        'Starting analysis for calculating mean percent change '
        'in water demand:')
    
    # Import the CSV raw data table to the gdb.
    arcpy.conversion.ExportTable(
        inputTable,
        outputTable)
    
    print('Done!')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')
