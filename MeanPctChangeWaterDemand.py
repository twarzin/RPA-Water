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
                    1. Create pivot table of raw csv data.
Required Args:  Input data, format = ID is FIPS+Scenario, 2015 wd, 2070 wd.
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
    'Withdrawals')

# Sectors and scenarios of interest (domestic and thermoelectric;
#   RCP 4.5 and 8.5.)
sectorList = [
    'dp',
    'th']
scenarioList = [
    '45',
    '85']

# Set the workspace.
env.workspace = GDB

try:
    print(
        'Starting analysis for calculating mean percent change '
        'in water demand:')
    
    # Import the CSV raw data table to the gdb.
    print(
        '    Importing the raw data...')
    fieldMap = r'fips "fips" true true false 6 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,fips,-1,-1;sector "sector" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,sector,0,8000;scenario "scenario" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,scenario,0,8000;yearr "yearr" true true false 4 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,yearr,-1,-1;withdrawal "withdrawal" true true false 8 Double 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,withdrawal,-1,-1'
    arcpy.conversion.TableToTable(
        inputTable,
        outputTable,
        field_mapping=fieldMap)
    
    # Add a concatenated ID field.
    fullName = "!fips! + !scenario!"

    # Select the sectors.
    for sct in sectorList:
        for rcp in scenarioList:
            print(
                f"Selecting records for sector '{sct}' "
                f"and scenarios for 'RCP {rcp}'...")
            # Table name example: Withdrawals_dp45
            tableSubset = \
                'Withdrawals_' + sct + rcp
            # Select statement:
            #   sector = sct
            #   And scenario LIKE rcp
            #   And yearr = 2015
            #   Or sector = sct
            #   And scenario LIKE rcp
            #   And yearr = 2070
            sql = \
                "sector = '" + sct + \
                "' And scenario LIKE '%" + rcp + \
                "%' And yearr = 2015 Or sector = '" + sct + \
                "' And scenario LIKE '%" + rcp + "%' And yearr = 2070"
            # The correct sql prints out like this:
            # sector = 'dp' And scenario LIKE '%45%' And yearr =
            #   2015 Or sector = 'dp' And scenario LIKE '%45%' And yearr = 2070
            print(
                f"    The sql statement is: {sql}")
            arcpy.analysis.TableSelect(
                in_table=outputTable,
                out_table=tableSubset,
                where_clause=sql)
            # Transpose the table.
            
            # Add a field for calculating the percent change.
            #   ((2070wd-2015wd)/2015wd)*100
            arcpy.management.AddField(
                in_table=tableSubset,
                field_name="PctChange",
                field_type='DOUBLE',
                field_alias="Pct Change")
            # Calculate percent change from 2015 to 2070.
            # Calculate the mean percent change for all records in the table.
            statsFields = [["PctChange", "MEAN"]]
            tableFinal = tableSubset + "_mean"
            arcpy.analysis.Statistics(
                in_table=tableSubset,
                out_table=tableFinal,
                statistics_fields=statsFields,
                case_field='ID')
            
    # Clean up
    # Remove intermediate data tables?
    #   Could simply leave as is and use the four tables, or compile them into
    #   one table and then remove the four.

    print('Done!')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')