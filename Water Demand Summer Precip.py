"""
Source Name:    Water Demand Summer Precip.py
Tool Name:
Version:        ArcGIS Pro 2.7
                Python 3.9
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2021 May 13
Updates:
Description:    This script replaces sections of the R script that transposes
                data for sy15, sy16 etc., then calculates and saves change in
                precip data to _______________.
                Flow for this script is found in "Water Demand Summer Precip
                Script Design.drawio"

Required Args:  Inputs - pr_[climate model][rcp]_month.xlsx files
                    In ...\CountyPrecip\SummerPrecip\doneMonthlyCountyPrecip on
                    Pam's computer.
                    The CountyPrecipMonthlyCSV folder had the original CSV
                    data files. I manually converted these to Excel format.
                    5 climate models
                        CNRM_CM5
                        HadGEM2_ES365
                        IPSL_CM5A_MR
                        MRI_CGCM3
                        NorESM1_M
                    2 RCPs
                        rcp45
                        rcp85
                Outputs - pr_[climate model][rcp]_month_spFinal.xlsx files
                    In ...\CountyPrecip\SummerPrecip on Pam's computer. Originally
                    the inputs were here as well but I moved them to the 'done'
                    folder when the script was finished.

Optional Args:
Notes:          See related metadata file:
                    "0_Metadata for 1_Climate Folder.docx".
                'Ctrl-p' shows parameter info.
"""

# Import modules
print('Importing required modules...')
import arcpy
import os
from arcpy import env

# Check for licenses
print('Checking for licenses...')
arcpy.CheckOutExtension("Spatial")

env.overwriteOutput = True
COORDS = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")

# Locations on Pam's computer
# Root folder
MAINFOLDER = r'E:\\_Projects\\WaterDemand\\WaterDemandProject'
# Geodatabase for all intermediate and final datasets
GDB_WORKDIR = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')
# Geodatabase for the output tables - for uploading to AGOL
GDB_PPT = os.path.join(
    MAINFOLDER,
    'Precip.gdb')
# Excel input tables, converted by hand from R script CSV outputs
#   This needs to be revised as follows if you run this script again.
#   Uncomment the next few lines and modify the body of the script accordingly:
# New variable for input files.
# folderInputDataFiles = os.path.join(
#     MAINFOLDER,
#     'DataWaterDemand\\CountyPrecip\\SummerPrecip\\doneMonthlyCountyPrecip')
# Revised variable for final output data files.
# folderOutputDataFiles = os.path.join(
#     MAINFOLDER,
#     'DataWaterDemand\\CountyPrecip\\SummerPrecip')
folderExcelFiles = os.path.join(
    MAINFOLDER,
    'DataWaterDemand\\CountyPrecip\\SummerPrecip')

try:
    print(
        'Starting analysis for change in summer precipitation for the '
        'Water Demand project:')

    # Import the Excel Spreadsheets to the GDB --------------------------------
    env.workspace = folderExcelFiles
    listExcelFiles = arcpy.ListFiles('*.xlsx')
    # Use this to select the blank lines that carried over from the CSV files.
    sqlNoNulls = 'Date IS NOT NULL'
    for xlsx in listExcelFiles:
        print('  The current Excel file is {0}'.format(xlsx))
        # Get the Excel file name without the extension.
        nameExcelFile = os.path.splitext(xlsx)[0]
        # Define the name of the current sheet
        #   (they are all the same as the root of the file name).
        # Excel sheet names have a '$' at the end when viewed in ArcGIS apps.
        sheetName = nameExcelFile + '$'
        # Full path of the worksheet, the Excel file is the directory
        #   or workspace
        sheetPath = os.path.join(xlsx, sheetName)
        print('    The Excel sheet is {0}'.format(sheetPath))
        # Make a temporary table view without the NULL records.
        print('    Making a table view without NULL records...')
        tblViewNoNulls = arcpy.management.MakeTableView(
            sheetPath,
            'temp_NoNulls',
            sqlNoNulls)
        # Make the table view a permanent table in the WorkDir gdb
        #   (precip data with blank rows removed).
        print('    Saving {0} to the WorkDir GDB...'.format(nameExcelFile))
        # Use the same name as the Excel file for the new gdb table
        #   (example = 'pr_CNRM_CM5rcp45_month').
        tblPrecip = os.path.join(
            GDB_WORKDIR,
            nameExcelFile)
        # Import the Excel rows to the gdb.
        arcpy.management.CopyRows(
            tblViewNoNulls,
            tblPrecip)

        # Summarize the Annual Summer Precip ----------------------------------
        # Create a precip data subset of the summer months (April - September).
        print(
            '    Selecting the summer months from the precip data ("summer '
            'growing precip")...')
        # Select data for months of April through September.
        sqlSummerMonths = "Month >= 4 and Month <= 9"
        # Create a table view of the summer months' precip data.
        tblViewSummer = arcpy.management.MakeTableView(
            tblPrecip,
            'temp_SummerPrecip',
            sqlSummerMonths)
        # Build the list of fields on which to summarize annual precip stats.
        print('    Getting ready to summarize the summer growing precip:')
        print('      Generating a list of fields...')
        # We want to summarize the precip data in the FIPS fields, so generate
        #   a list of them.
        # The FIPS columns start at index 4 (the 5th field) and go to the end.
        fieldList = arcpy.ListFields(tblViewSummer)[4:]
        # Create an empty list that will store the formatted list of
        #   fields for statistics calculation
        statsFields = []
        # Do the following loop for each FIPS field to build the stats list.
        for field in fieldList:
            print('      Adding {0} to the fields list...'.format(field.name))
            # Add the current field name and "Sum" to the stats fields list.
            statsFields.append([field.name, "Sum"])
        # Name of the output summer precip table
        #   (example = 'pr_CNRM_CM5rcp45_month_S').
        nameSummerPrecip1 = nameExcelFile + '_S'
        outTableSummer1 = os.path.join(
            GDB_WORKDIR,
            nameSummerPrecip1)
        # Define the case field to include in the stats table.
        caseField = 'Year'
        # Calculate the annual summer precip data by year for each FIPS.
        #   ("growing precip" in the R script)
        # Fields in the output are: Year, Frequency, Sum_F1001, Sum_F1003,
        #   Sum_F1005, etc.
        print(
            '      Summarizing the list of fields for '
            'summer/growing precip...')
        arcpy.analysis.Statistics(
            tblViewSummer,
            outTableSummer1,
            statsFields,
            caseField)

        # Transpose the Precip Data -------------------------------------------
        # Generate the list of FIPS fields to transpose. We want the FIPS
        #   columns to be placed in rows.
        print('    Generating transpose fields list...')
        # The FIPS field names in the Statistics output all start with 'Sum_F'.
        fieldNamesTxpose = [f.name for f in arcpy.ListFields(
            outTableSummer1,
            'Sum_F*')]
        # Create a blank list for the FIPS fields info.
        fieldsList = []
        for f in fieldNamesTxpose:
            # Combine the FIPS field names to produce this: "Sum_F1001 1001,
            #   Sum_F1003 1003, Sum_F1005 1005, ...".
            # '[5:9]' trims off the 'Sum_F' prefix in front of the
            #   'FIPS' fieldnames.
            appendFields = f + ' ' + f[5:9]
            # Adds the current field info to the list.
            fieldsList.append(appendFields)
        # Now format the list so it has the correct syntax for the
        #   Transpose tool. Example - "Sum_F1001 1001;Sum_F1003 1003; ..."
        # Add the semi-colon that acts as the delimiter between field entries.
        fieldListTxpose = ';'.join(fieldsList)
        # Name of the transposed summer precip
        #   (example = 'pr_CNRM_CM5rcp45_month_S_Tx')
        nameSummerPrecip2 = nameSummerPrecip1 + '_Tx'
        # Full path of transposed summer precip
        outTableSummer1Txposed = os.path.join(
            GDB_WORKDIR,
            nameSummerPrecip2)
        # The name of the newly-transposed field.
        fieldTranspose = 'FIPS'
        # The name of the values field.
        # Also used for the Base Precip field name
        #   (see below when the alias is updated).
        fieldValue = 'PrecipSummer'
        # The field to maintain as an attribute.
        fieldAttribute = 'Year'
        print(
            '    {0} will be transposed using this list of '
            'field names: {1}'.format(
                nameSummerPrecip1,
                fieldListTxpose))
        # Transpose the annual precip data.
        arcpy.management.TransposeFields(
            outTableSummer1,
            fieldListTxpose,
            outTableSummer1Txposed,
            fieldTranspose,
            fieldValue,
            fieldAttribute)

        # Create 'Base' Data Table from the 2015 Precip Data ------------------
        # Same as 's_precip0' in the R script.
        print(
            '    Creating a data table of the 2015 precip '
            'data as a base dataset...')
        # Name of the 2015 base precip data
        #   (example = 'pr_CNRM_CM5rcp45_month_S_Tx_Base2015')
        nameBasePrecip = nameSummerPrecip2 + '_Base2015'
        # Full path of the 2015 base precip data
        tblBasePrecip = os.path.join(
            GDB_WORKDIR,
            nameBasePrecip)
        # sql statement is 'Year = 2015'.
        sqlSelectBase = fieldAttribute + ' = 2015'
        print(
            '    The current sql select statement is: {0}'.format(
                sqlSelectBase))
        # Select the 'base' data for 2015 from the transposed summer precip
        #   data and copy to a new gdb table. The base precip field will be
        #   joined back to the transposed summer precip.
        arcpy.analysis.TableSelect(
            outTableSummer1Txposed,
            tblBasePrecip,
            sqlSelectBase)

        # Add the Base Precip Field to the Summer Precip Data. ----------------
        # Format the 'Precip' field for the JoinField process.
        fieldAddPrecipBase = [fieldValue]
        # Join by FIPS to add the base precip to the transposed summer
        #   precip table.
        #   After the base precip field joins to the summer precip,
        #   the field name has '_1' as a suffix.
        arcpy.JoinField_management(
            outTableSummer1Txposed,
            fieldTranspose,
            tblBasePrecip,
            fieldTranspose,
            fieldAddPrecipBase)

        # Correct the Field Types for the Final Precip Data Fields. -----------
        # At this point the two data fields, 'Precip' and
        #   'Summer Precip Base', are Text fields.
        #   They generate an error when you try to calculate the difference
        #       between them (change in precip), so they need to be changed
        #       to Double.
        print(
            '    Correcting the precip field data types for the final '
            'summer precip data table...')
        # Output feature class location
        outloc = GDB_WORKDIR
        # Name of output feature class
        #   (example = 'pr_CNRM_CM5rcp45_month_Summer')
        #   This will be the name of the final data table for summer precip.
        outTableFinal = nameExcelFile + '_Summer'
        print('      Defining new data type and field names to change...')
        # # List of fields for which to change data type (FIPS and precip data)
        # #  Re-write code to use a list instead of 2 hard-coded field names.
        # fieldNames = [fieldTranspose, fieldValue]
        # New field type
        fieldtype = 'Double'
        # Set the empty 'FieldMappings' object.
        fms = arcpy.FieldMappings()
        print(
            '      Creating a list of input field info from {0}...'.format(
                outTableSummer1Txposed))
        # Collect info for all fields in the input fc and store them
        #   in 'fieldList'.
        fieldList = arcpy.ListFields(outTableSummer1Txposed)
        # List of fields to exclude from the output feature class
        skipfields = ['OBJECTID']
        # Loop through the list of fields in the 'fieldList' variable.
        for field in fieldList:
            print('      Going through the input field list...')
            # If the current field is in the 'skipfields' list...
            if field.name in skipfields:
                # ...pass on it and go to the next field in the
                #   'fieldList' variable.
                print('      Skipping a field...')
                pass
            else:  # The field is one you want to keep.
                # Define an empty FieldMap to store this field's info for the
                #   output FieldMap.
                fm = arcpy.FieldMap()
                print(
                    '      Adding input field info for {0} to the '
                    'field map...'.format(
                        field.name))
                # Load the input table's field info (length, type, etc.)
                #   to the 'FieldMap'.
                fm.addInputField(outTableSummer1Txposed,
                                 field.name)
                # Probably a better way to write this section than
                #   having 2 'if-loops'.
                # Check to see if the current field name from the input
                #   table equals 'fieldname1' or 'fieldname2'.
                print(
                    '      Checking to see if field type needs to '
                    'be changed...')
                # If field name = 'PrecipSummer'
                if field.name == fieldValue:
                    print("      We will change this field's type...")
                    # If it matches then set 'newfield' to the
                    #   current field map.
                    newfield = fm.outputField
                    # Change the field type property to that
                    #   defined by 'fieldtype'.
                    newfield.type = fieldtype
                    # Reset the current field map to the 'newfield' setting.
                    fm.outputField = newfield
                # Repeat for the second field to match.
                # If field name = 'PrecipSummer_1'
                if field.name == fieldValue + '_1':
                    print("      We will change this field's type...")
                    newfield = fm.outputField
                    newfield.type = fieldtype
                    fm.outputField = newfield
                print(
                    '      Adding "{0}" (type = {1}) to the '
                    'field map...'.format(
                        field.name,
                        fieldtype))
                fms.addFieldMap(fm)
        # Print the output feature class's field map.
        print('      Done creating the field mappings.')
        print('      The new field map is: {0}'.format(fms))
        # Create a table using the new field mappings 'fms'.
        print(
            '      Creating the new "{0}" table with the updated '
            'field mappings...'.format(
                outTableFinal))
        arcpy.conversion.TableToTable(
            outTableSummer1Txposed,
            outloc,
            outTableFinal,
            field_mapping=fms)

        # Change the field alias for 'Precip_1' to 'Summer Precip Base' -------
        # Base precip field name (the '_1' gets added by the JoinField process)
        fieldPrecipBaseName = fieldValue + '_1'
        # New field alias for 'Precip' in the base data
        fieldPrecipBaseAlias = 'Summer Precip Base'
        print('      Updating the base precip field alias to "{0}"...'.format(
            fieldPrecipBaseAlias))
        arcpy.AlterField_management(
            outTableFinal,
            fieldPrecipBaseName,
            new_field_alias=fieldPrecipBaseAlias)

        # Add and calculate a field for change in precip ----------------------
        #   ('PrecipSummer' - 'PrecipBase')
        # Add field
        # Change in precip field
        fieldPrecipChange = 'ChangeSummerPrecip'
        # Field alias
        fieldPrecipChangeAlias = 'Change in Summer Precip'
        print('    Adding a field for precipitation change...')
        arcpy.management.AddField(
            outTableFinal,
            fieldPrecipChange,
            'DOUBLE',
            field_alias=fieldPrecipChangeAlias)
        # Calculate field
        print('    Calculating the field for precipitation change...')
        # Change in Summer Precip equation
        #   (the field with '_1' at the end is the base precip data)
        #   ChangeSummerPrecip = PrecipSummer - PrecipBase
        sqlPrecipChange = '!'+fieldValue+'!'+'-'+'!'+fieldValue+'_1!'
        arcpy.management.CalculateField(
            outTableFinal,
            fieldPrecipChange,
            sqlPrecipChange,
            'PYTHON')

        # Export the Final Data Tables. ---------------------------------------
        #   This section can be added to the above loop when the script
        #       is finished and running.
        #   The Excel files are for input to Water Demand R scripts,
        #       and the geodatabase tables are for AGOL.
        #   The Excel field aliases get removed when saving to a CSV file.
        #   Fields are converted to tblBasePrecip field names:
        #       FIPS, Year, PrecipSummer, PrecipSummer_1, ChangeSummerPrecip.
        # List the gdb final summer precip tables, then loop through the
        #   list and export each table to an Excel file.
        env.workspace = GDB_WORKDIR
        # Excel file name - strip off the '_Summer' suffix
        outFileNameFinalExcel = nameExcelFile + '_spFinal.xlsx'
        # Excel file full path
        xlsPath = 'DataWaterDemand\\CountyPrecip\\SummerPrecip'
        outFinalExcel = os.path.join(
            MAINFOLDER,
            xlsPath,
            outFileNameFinalExcel)
        # Export gdb table to Excel.
        arcpy.conversion.TableToExcel(
            outTableFinal,
            outFinalExcel)
        # Add a section here that exports the table to a Precip gdb for
        #   use in AGOL and ArcGIS Pro. #######################################
        env.workspace = GDB_PPT
        # Import the excel file to the precip gdb
        arcpy.conversion.ExcelToTable()

    # Cleanup, remove intermediate data ---------------------------------------
    #   May not ever use this section.
    #   I am keeping all the intermediate outputs for now.
    print('    Removing intermediate data...')
    deleteList = transposeSummerT + ';' + tblBasePrecip
    arcpy.management.Delete(deleteList)

    print('Done!')
    print('You can remove any "temp_" layers from your map if they exist.')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')
