"""
Source Name:    Water Demand Summer PET.py
Tool Name:
Version:        ArcGIS Pro 3.0
                Python 3.9
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2022 July 21
Updates:

Description:    This script is mostly identical to "Water Demand Summer
                PET.py", except for minor revisions to accomodate
                differences in the PET input data.
                The flow chart for this script is found in "Water Demand Summer
                PET Script Design.drawio"

Required Args:  Inputs - PET_[climate model][rcp]_Monthly.csv files
                    In "...\DataWaterDemand\CountyPET\CountyPET_inputFiles" on
                    Pam's computer. These are monthly HUC 8 PET in mm for the
                    years 1950 to 2099.
                    5 climate models
                        CNRM_CM5
                        HadGEM2_ES365
                        IPSL_CM5A_MR
                        MRI_CGCM3
                        NorESM1_M
                    2 RCPs
                        rcp45
                        rcp85
                Outputs - PET_[climate model][rcp]_petFinal.xlsx files
                    In ...\DataWaterDemand\CountyPET\CountyPET_outputFiles
                    on Pam's computer.

Optional Args:
Notes:          See related metadata file:
                    "0_Metadata for 1_Climate Folder.docx".
                'Ctrl-p' in PyCharm shows parameter info.
"""

# Import modules
print('Importing required modules...')
import arcpy
import os
from arcpy import env

env.overwriteOutput = True
# COORDS = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")

# Locations on Pam's computer
# Root folder
MAINFOLDER = 'E:\\_Projects\\WaterDemand\\WaterDemandProject'

# Geodatabase for all intermediate and final datasets
GDB_WORKDIR = os.path.join(
    MAINFOLDER,
    'WaterDemandProject.gdb')

# # Geodatabase for PET outputs, use for uploading to AGOL
# GDB_PET = os.path.join(
#     MAINFOLDER,
#     'PET.gdb')

# Location of input files.
folderInputDataFiles = os.path.join(
    MAINFOLDER,
    'DataWaterDemand\\CountyPET\\CountyPET_inputFiles')
# Location of output files.
folderOutputDataFiles = os.path.join(
    MAINFOLDER,
    'DataWaterDemand\\CountyPET\\CountyPET_outputFiles')

# Other global variables
#   Define the index of the first FIPS data field.
#   In the 2022 PET raw data files, it was column 5, so the index is 4.
#   See the 'fieldList = arcpy.ListFields' bookmark.
firstColumnHUC = 4
#   Define the case field by which the PET data are summarized.
#   See the 'arcpy.analysis.Statistics' bookmark.
caseField = 'year'

# Start the analysis.
try:
    print(
        'Starting analysis for change in summer potential evapotranspiration '
        '(PET) for the Water Demand project:')

    # Make a list of the raw data files. --------------------------------------
    
    arcpy.env.workspace = folderInputDataFiles
    listDataFiles = arcpy.ListFiles("*.xlsx")
    for file in listDataFiles:
        # Print the current file name.
        print('  The current input raw data file is {0}'.format(file))
        
        # Define the name of the Excel worksheet.
        #   Get the Excel file name without the extension.
        nameExcelFile = os.path.splitext(file)[0]
        #   Define the name of the current sheet
        #   (they are all the same as the root of the file name).
        #   Excel sheet names have a '$' at the end when viewed in ArcGIS Pro,
        #   so append this character to the Excel file name.
        sheetName = nameExcelFile + '$'
        #   Full path of the worksheet, the Excel file is the directory
        #   or workspace
        sheetPath = os.path.join(file, sheetName)
        print('    The Excel sheet is {0}'.format(sheetPath))
        
        # Import the 2015-2070 summer PET data tables to the GDB. -------------
        # Filter the raw data and import to the WorkDir gdb.
        print('    Filtering the data...')
        #   Define the data filters.
        #   Select the summer months from 2015 to 2070 (the same years and
        #   months as the summer precip data).
        sqlFilterData = "year >= 2015 And year <= 2070 " \
                        "And month >= 4 And month <= 9"
        
        # Make a temporary table view with the applied filters.
        tblviewFilteredData = arcpy.MakeTableView_management(
            sheetPath,
            "tblViewTemp",
            sqlFilterData)
        
        # Define the name of the gdb table, based on the input filename.
        #   (example input filename = 'PET_CNRM_C5_45_Monthly', output is
        #   'PET_CNRM_C5_45_MonthlySummer')
        #   Get the Excel file name without the extension, then....
        #   add a suffix to indicate the data are for April through September.
        nameSummerPET = (nameExcelFile + "Summer")
        print('    Saving {0} to the WorkDir GDB...'.format(nameSummerPET))
        tblPET = os.path.join(
            GDB_WORKDIR,
            nameSummerPET)
        arcpy.CopyRows_management(
            tblviewFilteredData,
            tblPET)
        
        # Summarize the HUC 8 Summer PET data by year -------------------------
        # Build the list of fields on which to summarize annual PET stats.
        print('    Getting ready to summarize the summer PET:')
        print('      Generating a list of fields...')
        #   We want to summarize the PET data in the HUC fields, so generate
        #   a list of them. The HUC columns start at index 4 (the 5th field)
        #   and go to the end. The index variable is defined above in the
        #   global variables section. See the link for firstColumnHUC in the
        #   Structure pane in PyCharm.
        fieldList = arcpy.ListFields(tblPET)[firstColumnHUC:]
        # Create an empty list that will store the formatted list of
        #   fields for statistics calculation.
        statsFields = []
        # Do the following loop for each HUC field to build the input stats
        #   list for the Statistics process.
        for field in fieldList:
            print('      Adding {0} to the fields list...'.format(field.name))
            # Add the current field name and "Sum" to the stats fields list.
            statsFields.append([field.name, "Sum"])
            
        # Calculate the annual summer PET for each HUC.
        #   Define the name of the output annual summer PET table, eliminating
        #   the 'Monthly' from the file name and adding 'AnnualSummerRaw' to
        #   indicate the HUC fields have not been transposed yet.
        #   (example = 'PET_CNRM_C5_45_AnnualSummerRaw').
        nameSummerPET1 = nameExcelFile[:-8] + '_AnnualSummerRaw'
        outTableSummer1 = os.path.join(
            GDB_WORKDIR,
            nameSummerPET1)
        # The case field is defined in global variables at the beginning
        #   of this script.
        print('      Summarizing the data for annual summer PET.')
        print('      The output file is {0}.'.format(nameSummerPET1))
        #   Fields in this output are: year, Frequency, Sum_F10010001,
        #   Sum_F10010002, Sum_F10020001, etc.
        arcpy.analysis.Statistics(
            tblPET,
            outTableSummer1,
            statsFields,
            caseField)

        # Transpose the PET Data ----------------------------------------------
        
        # Generate the list of HUC fields to transpose. We want the HUC
        #   columns to be placed in rows.
        print('    Generating transpose fields list...')
        #   The HUC field names in the Statistics output all start with 'Sum_F'.
        fieldNamesTxpose = [f.name for f in arcpy.ListFields(
            outTableSummer1,
            'Sum_F*')]
        #   Create a blank list for the HUC fields info.
        fieldsList = []
        for f in fieldNamesTxpose:
            # Combine the HUC field names to produce this: "Sum_F1001 1001,
            #   Sum_F1003 1003, Sum_F1005 1005, ...".
            # '[5:13]' trims off the 'Sum_F' prefix in front of the
            #   'HUC' fieldnames.
            appendFields = f + ' ' + f[5:13]
            # Adds the current field info to the list.
            fieldsList.append(appendFields)
        #   Now format the list so it has the correct syntax for the
        #   Transpose tool. Example - "Sum_F1001 1001;Sum_F1003 1003; ..."
        #   Add the semi-colon that acts as the delimiter between field entries.
        fieldListTxpose = ';'.join(fieldsList)
        # Name of the transposed output table
        #   (example = 'PET_CNRM_C5_45_AnnualSummerTx')
        nameSummerPET2 = nameSummerPET1 + 'Tx'
        # Full path of transposed summer PET table
        outTableSummer1Txposed = os.path.join(
            GDB_WORKDIR,
            nameSummerPET2)
        # The name of the newly-transposed field.
        fieldTranspose = 'HUC'
        # The name of the values field.
        #   Also used for the Base PET field name
        #   (see below when the alias is updated in the 'Add the Base
        #   PET Field' section).
        fieldValue = 'PETSummer'
        # The field to maintain as an attribute.
        fieldAttribute = caseField
        print(
            '    {0} will be transposed using this list of '
            'field names: {1}'.format(
                nameSummerPET1,
                fieldListTxpose))
        # Transpose the annual PET data.
        arcpy.management.TransposeFields(
            outTableSummer1,
            fieldListTxpose,
            outTableSummer1Txposed,
            fieldTranspose,
            fieldValue,
            fieldAttribute)

        # Add a 2015 base data field to the summer PET table. -----------------
        print(
            '    Creating a data table of the 2015 PET '
            'data as a base dataset...')
        # Name of the 2015 base PET data
        #   (example = 'PET_CNRM_C5_45_AnnualSummerBase2015')
        nameBasePET = nameSummerPET2[:-2] + 'Base2015'
        # Full path of the 2015 base PET data
        tblBasePET = os.path.join(
            GDB_WORKDIR,
            nameBasePET)
        # sql statement translates to 'year = 2015'.
        sqlSelectBase = fieldAttribute + ' = 2015'
        print(
            '    The current sql select statement is: {0}'.format(
                sqlSelectBase))
        # Select the 'base' data for 2015 from the transposed summer PET
        #   data and copy to a new gdb table. The base PET field will be
        #   joined back to the transposed summer PET.
        arcpy.analysis.TableSelect(
            outTableSummer1Txposed,
            tblBasePET,
            sqlSelectBase)

        # Format the 'PET' field for the JoinField process.
        fieldAddPETBase = [fieldValue]
        # Join by HUC to add the base PET to the transposed summer
        #   PET table.
        #   After the base PET field joins to the summer PET,
        #   the field name has '_1' as a suffix.
        arcpy.JoinField_management(
            outTableSummer1Txposed,
            fieldTranspose,
            tblBasePET,
            fieldTranspose,
            fieldAddPETBase)
        
        # Correct the field types in the final PET data table. ----------------
        
        # At this point the two data fields, 'PET' and
        #   'Summer PET Base', are TEXT fields.
        #   They generate an error when you try to calculate the difference
        #       between them (change in PET), so they need to be changed
        #       to DOUBLE.
        print(
            '    Correcting the PET field data types for the final '
            'summer PET data table...')
        # Name of output feature class
        #   (example = 'PET_CNRM_C5_45_AnnualSummer')
        #   This will be the name of the final data table for summer PET.
        outTableFinal = nameExcelFile[:-8] + '_AnnualSummer'
        print('      Defining new data type and field names to change...')
        # List of fields for which to change data type (HUC and PET data)
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
                # If field name = 'PETSummer'
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
                # If field name = 'PETSummer_1'
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
            GDB_WORKDIR,
            outTableFinal,
            field_mapping=fms)

        # Change the field name and alias for the summer and summer base
        #   PET fields to avoid confusion later.

        # Change the workspace to the location of the final output tables.
        env.workspace = GDB_WORKDIR

        # Change the summer PET alias.
        # Define the new alias.
        fieldPETNewAlias = 'Summer PET'
        print('      Updating the Summer PET field alias '
              'to "{0}".'.format(fieldPETNewAlias))
        arcpy.management.AlterField(
            outTableFinal,
            fieldValue,
            new_field_alias=fieldPETNewAlias)
        
        # Change the Summer PET Base field name and alias.
        # Define the field name (the '_1' gets added by the
        #   JoinField process earlier in this script).
        fieldPETBaseName = fieldValue + '_1'
        # Define the new field name.
        fieldPETBaseNewName = fieldValue + 'Base'
        # Define the new field alias
        fieldPETBaseAlias = 'Summer PET Base'
        print('      Input table is "{0}"'.format(outTableFinal))
        print('      Updating the base PET field name to {0} and the alias '
              'to "{1}"...'.format(fieldPETBaseNewName, fieldPETBaseAlias))
        arcpy.management.AlterField(
            outTableFinal,
            fieldPETBaseName,
            new_field_name=fieldPETBaseNewName,
            new_field_alias=fieldPETBaseAlias)

        # Add and calculate a field for change in PET. ------------------------
        #   Generally, the formula is: '[PETSummer]-[PETBase]'.
        
        # Add a field for the change in PET values.
        # Define the new field name.
        fieldPETChange = 'ChangeSummerPET'
        # Define the new field alias.
        fieldPETChangeAlias = 'Change in Summer PET'
        print('    Adding a field for PET change...')
        arcpy.management.AddField(
            outTableFinal,
            fieldPETChange,
            'DOUBLE',
            field_alias=fieldPETChangeAlias)
        
        # Calculate field
        print('    Calculating the field for PET change...')
        # Change in Summer PET equation
        #   ChangeSummerPET = PETSummer - PETBase
        sqlPETChange = '!'+fieldValue+'!'+'-'+'!'+fieldPETBaseNewName+'!'
        arcpy.management.CalculateField(
            outTableFinal,
            fieldPETChange,
            sqlPETChange,
            'PYTHON')

        # Export the final data tables and clean up. --------------------------
        
        #   The Excel files are for input to Water Demand R scripts,
        #       and the geodatabase tables are for AGOL.
        #   If you save these Excel files as CSVs, the field aliases
        #       are removed.
        #   Fields are converted to tblBasePET field names:
        #       HUC, Year, PETSummer, PETSummer_1, ChangeSummerPET.
        
        # List the gdb final summer PET tables, then loop through the
        #   list and export each table to an Excel file.
        
        # Define the output Excel file name - strip off the '_Summer' suffix
        outFileExcelFinal = outTableFinal[:-13] + '_petFinal.xlsx'
        outFinalExcel = os.path.join(
            folderOutputDataFiles,
            outFileExcelFinal)
        
        # Export gdb table to Excel.
        print('     Exporting the data to an Excel file...')
        arcpy.conversion.TableToExcel(
            outTableFinal,
            outFinalExcel)
        
        # # Add a section here that exports the table to a PET gdb for
        # #   uploading to AGOL. ##############################################
        # env.workspace = GDB_PET
        # # Copy the table to the PET gdb.

        # Cleanup, remove intermediate data.
        #   May not ever use this section.
        #   I am keeping all the intermediate outputs for now.
        print('Removing intermediate data...')
        # Define the items to delete.
        deleteList = [
            outTableSummer1,
            tblBasePET,
            outTableSummer1Txposed,
            tblPET]
        arcpy.management.Delete(deleteList)
        
        # Reset the workspace and go get the next file in the list.
        arcpy.env.workspace = folderInputDataFiles
        print('  Go get the next input file....')
        print()

    print('Done!')
    print('You can remove any "temp_" layers from your map if they exist.')

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')
