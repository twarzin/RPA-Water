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
cons_wdTable = \
    'ConsWithdrawals'
# Filtered table with OIDs added and only for years 2015.
wdTable = \
    'Withdrawals'
# Filtered table with OIDs added and only for years 2070. This will be joined to the 2015 data in the wdTable.
wdTable2070 = \
    'wd2070'

# Sectors and scenarios of interest (domestic and thermoelectric;
#   RCP 4.5 and 8.5.)
sectorList = [
    'ir',
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
    
    # # Import the CSV raw data table to the gdb and configure the imported table.
    # print(
    #     '    Importing the raw data...')
    # # Convert the raw data to a supported format that will contain an OID field.
    # arcpy.management.CopyRows(
    #     in_rows=inputTable,
    #     out_table=cons_wdTable)
    
    # Add a concatenated ID field. This will create a primary key for joining
    #   the 2015 and 2070 data together.
    print(
        '    Adding an ID field...')
    newID = 'ID'
    arcpy.management.AddField(
        in_table=cons_wdTable,
        field_name=newID,
        field_type='TEXT',
        field_length=50)
    # Add a field for calculating the percent change.
    print(
        '    Adding a PctChange field...')
    pctField = "PctChange"
    arcpy.management.AddField(
        in_table=cons_wdTable,
        field_name=pctField,
        field_type='DOUBLE',
        field_alias="Pct Change")
    # Create a table for the Withdrawals data.
    # Field map for the 2015 and 2070 export tables.
    print(
        '    Creating a table for the 2015 data...')
    fieldMap = r'fips "fips" true true false 4 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,fips,-1,-1;sector "sector" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,sector,0,8000;scenario "scenario" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,scenario,0,8000;yearr "yearr" true true false 4 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,yearr,-1,-1;withdrawal "withdrawal" true true false 8 Double 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\DataWaterDemand\MeanPctChange\cons_and_withdrawal.csv,withdrawal,-1,-1'
    expr2015 = "yearr = 2015"
    # Create the table with the 2015 data.
    arcpy.conversion.ExportTable(  # error here: invalid sql
        in_table=cons_wdTable,
        out_table=wdTable,
        where_clause=expr2015,
        field_mapping=fieldMap)
    # Calculate the concatenated ID field.
    #   Expression that concatenates the values of
    #   the fips and scenario fields.
    print(
        '    Calculating the ID field in the 2015 data...')
    fullName = "!fips! + !sector! + !scenario!"
    arcpy.management.CalculateField(
        in_table=wdTable,
        field=newID,
        expression=fullName,
        expression_type="PYTHON3")
    # Create a table for the 2070 data that will be joined to the 2015 data.
    print(
        '    Creating a table for the 2070 data...')
    expr2070 = "yearr = 2070"
    arcpy.conversion.ExportTable(
        in_table=cons_wdTable,
        out_table=wdTable2070,
        where_clause=expr2070,
        field_mapping=fieldMap)
    # Calculate the concatenated ID field.
    #   Expression that concatenates the values of
    #   the fips and scenario fields.
    print(
        '    Calculating the ID field in the 2070 data...')
    fullName = "!fips! + !sector! + !scenario!"
    arcpy.management.CalculateField(
        in_table=wdTable2070,
        field=newID,
        expression=fullName,
        expression_type="PYTHON3")
    
# works to this point - need to add the Pct Change field to the field map.


    # # Define output fields and field types.
    # #   This could be replaced with a FieldMap object and loop to determine
    # #   the output fields based on the input fields.
    # print('    Defining field types in the final import table...')
    # fieldMap = r'fips "fips" true true false 4 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConWithdrawals,fips,-1,-1;sector "sector" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConWithdrawals,sector,0,8000;scenario "scenario" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConWithdrawals,scenario,0,8000;yearr "yearr" true true false 4 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConWithdrawals,yearr,-1,-1;withdrawal "withdrawal" true true false 8 Double 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConWithdrawals,withdrawal,-1,-1'
    # # Select records for the years 2015 and 2070.
    # #   These are the years that the mean percent change is calculated from.
    # #   All of the sectors are in this table in case we want to
    # #   come back for more mean percent change calculations.
    # arcpy.conversion.TableToTable(
    #     in_rows=cons_wdTable,
    #     out_path=GDB,
    #     out_name=wdTable,
    #     field_mapping=fieldMap)
    #
    # exprYears = "yearr = 2015 Or yearr = 2070"
    #
    # # Transpose the table.
    #
    # # Select the sectors and the data for 2015 and 2070.
    # for sct in sectorList:
    #     for rcp in scenarioList:
    #         print(
    #             f"Selecting records for sector '{sct}' "
    #             f"and scenarios for 'RCP {rcp}'...")
    #         # Table name example: Withdrawals_dp45
    #         tableSubset = \
    #             'Withdrawals_' + sct + rcp
    #         # Select statement pseudocode:
    #         #   sector = sct
    #         #   And scenario LIKE rcp
    #         #   And yearr = 2015
    #         #   Or sector = sct
    #         #   And scenario LIKE rcp
    #         #   And yearr = 2070
    #         sql = \
    #             "sector = '" + sct + \
    #             "' And scenario LIKE '%" + rcp + \
    #             "%' And yearr = 2015 Or sector = '" + sct + \
    #             "' And scenario LIKE '%" + rcp + "%' And yearr = 2070"
    #         # The correct sql prints out like this:
    #         #   sector = 'dp' And scenario LIKE '%45%' And yearr = 2015
    #         #   Or sector = 'dp' And scenario LIKE '%45%' And yearr = 2070
    #         print(
    #             f"    The sql statement is: {sql}")
    #         arcpy.analysis.TableSelect(
    #             in_table=wdTable,
    #             out_table=tableSubset,
    #             where_clause=sql)
    #
    #         # Calculate percent change from 2015 to 2070.
    #         # ((2070wd-2015wd)/2015wd)*100
    #         yr2015 = '!2015!'
    #         yr2070 = '!2070!'
    #         exprPctChg = "((yr2070 - yr2015)-yr2015)*100"
    #         arcpy.management.CalculateField(
    #             in_table=tableSubset,
    #             field=pctField,
    #             expression=exprPctChg,
    #             expression_type="PYTHON3")
    #         # Calculate the mean percent change for all records in the table.
    #         statsFields = ["PctChange", "MEAN"]
    #         tableFinal = tableSubset + "_mean"
    #         arcpy.analysis.Statistics(
    #             in_table=tableSubset,
    #             out_table=tableFinal,
    #             statistics_fields=statsFields,
    #             case_field='ID')
            
    # Clean up
    # Remove intermediate data tables?
    #   Could simply leave as is and use the four tables, or compile them into
    #   one table and then remove the four.

    print('Done!')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')