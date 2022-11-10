""""
Source Name:    MeanPctChangeWaterDemand.py
Tool Name:
Version:        ArcGIS Pro 3.02
                Python 3.9
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2022 October 13
Updates:
Description:    Calculate mean percent change in water demand from 2015 to 2070
                across all scenarios.
                    1. Make a copy of the RPA counties feature class
                    for mapping.
                    2. Import a table of raw csv data.
                    3. Create a table with the 2015 and 2070 withdrawal data.
                    4. Calculate percent change from 2015 to 2070 for each record.
                    5. Calculate the mean percent change for all ag scenarios
                    and all thermo scenarios in RCP 4.5 and 8.5.
                    6. Compile the mean percent change data in a counties
                    feature class for mapping.
                    7. Remove intermediate data.
Required Args:  Input data, format = FIPS+Sector+Scenario concatenated ID,
                2015 wd, 2070 wd.
Optional Args:
Notes:          2022 Oct 13: Downloaded raw data 'cons_and_withdrawal.csv'
                from Box folder '2_Demand_Results'.
"""

# Import modules
print()
print(
    'Importing required modules...')
import arcpy
import os
from arcpy import env
env.overwriteOutput = True
arcpy.SpatialReference(
    "USA Contiguous Albers Equal Area Conic")

# Project folder
MAINFOLDER = \
    r'E:\_Projects\WaterDemand\WaterDemandProject'
# Project geodatabase
GDB = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')

# Counties feature class for RPA
fc_countiesRPA = \
    r'E:\0_Common\Counties\Counties.gdb\CountiesRPA'
# Copy of the RPA counties feature class for mapping the percent change data
fc_wdPctChg = \
    'WithdrawalsMeanPctChange'
# Counties join field (do not use FIPS or Recoded_FIPS)
joinFieldCounties = \
    'FIPS_NoZero'

# Raw input CSV water demand data
inputTable = os.path.join(
    MAINFOLDER,
    'DataWaterDemand',
    'MeanPctChange',
    'cons_and_withdrawal_2022-10-13.csv')
# Raw water demand data imported to the gdb
cons_wdTable = \
    'ConsWithdrawals'
# Join field for the withdrawals data
joinFieldWithdrawalsFips = \
    'fips'
# New field to act as a join field for the 2015 and 2070 data
primaryKeyWithdrawals = \
    'ID'

# Filtered table with OIDs added and only for years 2015.
wdTable = \
    'Withdrawals'
# Filtered table with OIDs added and only for years 2070.
#   This will be joined to the 2015 data in the wdTable.
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
    print()
    print(
        'Starting analysis for calculating mean percent change '
        'in water demand:')
    
    # Create a feature class for mapping the mean percent changes.
    print(
        '    Creating a feature class for mapping...')
    arcpy.management.CopyFeatures(
        in_features=fc_countiesRPA,
        out_feature_class=fc_wdPctChg)
    dropFields = [
        "STATEFP",
        "COUNTYFP",
        "COUNTYNS",
        "AFFGEOID",
        "LSAD",
        "ALAND",
        "AWATER"]
    arcpy.management.DeleteField(
        in_table=fc_wdPctChg,
        drop_field=dropFields)
    
    # Import the CSV raw data table to the gdb and configure
    #   the imported table.
    print()
    print(
        '    Importing the raw data...')
    # Convert the raw data to a supported format that will contain
    #   an OID field.
    arcpy.management.CopyRows(
        in_rows=inputTable,
        out_table=cons_wdTable)
    # Add a primary key field for joining the 2015 and 2070 data together.
    print(
        '        Adding a primary key to the imported table...')
    arcpy.management.AddField(
        in_table=cons_wdTable,
        field_name=primaryKeyWithdrawals,
        field_type='TEXT',
        field_length=50)

    # Create a table for the Withdrawals data.
    print()
    print(
        '    Defining field types for the new table...')
    # Field map for the 2015 and 2070 export tables.
    # Define output fields and field types.
    #   This could be replaced with a FieldMap object and loop to determine
    #   the output fields based on the input fields.
    fieldMap = r'fips "fips" true true false 5 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,fips,-1,-1;sector "sector" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,sector,0,8000;scenario "scenario" true true false 8000 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,scenario,0,8000;yearr "yearr" true true false 4 Long 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,yearr,-1,-1;withdrawal "withdrawal" true true false 8 Double 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,withdrawal,-1,-1;ID "ID" true true false 50 Text 0 0,First,#,E:\_Projects\WaterDemand\WaterDemandProject\WaterDemandProject.gdb\ConsWithdrawals,ID,0,50'
    # Create a table with the 2015 data.
    print(
        '    Creating a table for the 2015 data...')
    expr2015 = \
        "yearr = 2015"  # 'year' is misspelled in the csv data
    arcpy.conversion.ExportTable(
        in_table=cons_wdTable,
        out_table=wdTable,
        where_clause=expr2015,
        field_mapping=fieldMap)
    # Calculate the primary key ID field.
    #   Concatenate the values of the fips and scenario fields.
    print(
        '        Calculating the join field in the 2015 data...')
    fullName = \
        "!fips! + !sector! + !scenario!"
    arcpy.management.CalculateField(
        in_table=wdTable,
        field=primaryKeyWithdrawals,
        expression=fullName,
        expression_type="PYTHON3")
    # Create a table for the 2070 data that will be joined to the 2015 data.
    print(
        '    Creating a table for the 2070 data...')
    expr2070 = \
        "yearr = 2070"
    arcpy.conversion.ExportTable(
        in_table=cons_wdTable,
        out_table=wdTable2070,
        where_clause=expr2070,
        field_mapping=fieldMap)
    # Calculate the concatenated ID field (primary key field).
    #   Expression that concatenates the values of
    #   the fips and scenario fields.
    print(
        '        Calculating the join field in the 2070 data...')
    fullName = \
        "!fips! + !sector! + !scenario!"
    arcpy.management.CalculateField(
        in_table=wdTable2070,
        field=primaryKeyWithdrawals,
        expression=fullName,
        expression_type="PYTHON3")
    # Join the 2070 data to the 2015 data to complete the
    #   withdrawals data table.
    print(
        '    Joining the 2070 data to the 2015 data...')
    arcpy.management.JoinField(
        in_data=wdTable,
        in_field=primaryKeyWithdrawals,
        join_table=wdTable2070,
        join_field=primaryKeyWithdrawals)
    
    # Evaluate each sector and RCP to determin the mean percent change
    #   in withdrawals.
    print()
    print(
        '    Determine the mean percent changes...')
    for sct in sectorList:
        for rcp in scenarioList:
            print(
                f"        Selecting records for sector '{sct}' "
                f"and scenarios for 'RCP {rcp}'...")
            # Table name example: Withdrawals_ir45
            tableSubset = \
                wdTable + '_' + sct + rcp
            # Select statement pseudocode:
            #   "sector = sct AND scenario LIKE rcp"
            sql = \
                "sector = '" + sct + "' And scenario LIKE '%" + rcp + "%'"
            # The correct sql prints out like this:
            #   sector = 'ir' And scenario LIKE '%45%'
            arcpy.analysis.TableSelect(
                in_table=wdTable,
                out_table=tableSubset,
                where_clause=sql)

            # Add a field for calculating the percent change.
            print(
                '            Adding a PctChange field...')
            # Field to store the 2015-2070 percent change in withdrawals
            pctField = \
                "PctChange_" + sct + rcp
            arcpy.management.AddField(
                in_table=tableSubset,
                field_name=pctField,
                field_type='DOUBLE',
                field_alias="Pct Change " + sct + rcp)
            # Calculate percent change in withdrawals from 2015 to 2070.
            #   ((2070wd-2015wd)/2015wd)*100
            print(
                '            Caclulating the percent change '
                'in withdrawals...')
            yr2015 = \
                '!withdrawal!'
            yr2070 = \
                '!withdrawal_1!'
            exprPctChg = "((" + yr2070 + "-" + yr2015 + ")/" + yr2015 + ")*100"
            arcpy.management.CalculateField(
                in_table=tableSubset,
                field=pctField,
                expression=exprPctChg,
                expression_type="PYTHON3")

            # Calculate the mean percent change for all
            #   scenarios in each county.
            print(
                '            Calculating the mean percent '
                'change in withdrawals...')
            statsFields = [
                [pctField,
                 "MEAN"]]
            # Summarize by county.
            caseFields = joinFieldWithdrawalsFips
            tableFinal = tableSubset + "_MeanPctChg_2015_2070"
            arcpy.analysis.Statistics(
                in_table=tableSubset,
                out_table=tableFinal,
                statistics_fields=statsFields,
                case_field=caseFields)
            
            # Add the mean percent change field to the counties feature class.
            print('            Adding the mean percent change field to the '
                  'counties feature class...')
            
            joinFieldMPC = [
                "MEAN_" + pctField]
            arcpy.management.JoinField(
                in_data=fc_wdPctChg,
                in_field=joinFieldCounties,
                join_table=tableFinal,
                join_field=joinFieldWithdrawalsFips,
                fields=[joinFieldMPC])
            
            # Remove the intermediate data for this round.
            delIntData = [
                tableSubset,
                tableFinal]
            arcpy.management.Delete(delIntData)

    # Finish cleaning up by removing the intermediate data tables.
    print()
    print(
        'Cleaning up...')
    # Define the items to delete.
    deleteList = [
        wdTable2070,
        wdTable]
    # Delete the items in the list.
    arcpy.management.Delete(deleteList)

    print()
    print('Done!')
    print('Now go have some fun mapping...')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')