"""
Source Name:    Water Shortage.py
Tool Name:
Version:        ArcGIS Pro 2.8
                Python 3.6
Author:         Pam Froemke, Rocky Mountain Research Station
Date:           2021 August

Updates:

Description:    Analyzes 'coverage' data for the 2020 RPA assessment (output data tables from WEAP).
                Counts the number of months in a given year and HUC 4 ID where there was a shortage.
                Shortages are indicated by a '1'.
                Creates a new CSV file for each climate model to store the annual shortage month number.
                Output file names have an '_sm' suffix indicating 'Shortage Months'.
                
Required Args:  CSV output files from WEAP.
                1 = shortage
                0 = no shortage

Optional Args:  None

Notes:          Values in the inputs are the HUC IDs with a prefix. Some rows have 2 HUC IDs. If you want to map
                these data, the HUC IDs will have to be isolated in a separate column.
"""

# Import modules
print('Importing required modules...')
import arcpy
import os
import csv  # Enables creating and editing CSV files.
from arcpy import env

# # Check for licenses
# print('Checking for licenses...')
# arcpy.CheckOutExtension("Spatial")

# Environments
env.overwriteOutput = True

# Locations
MAINFOLDER = r'E:\WaterDemand\WaterDemandProject\DataWaterDemand\WaterShortage'
InputDataFolder = os.path.join(MAINFOLDER, 'InputData')  # CSV files downloaded from Box folder (Travis)
OutputDataFolder = os.path.join(MAINFOLDER, 'OutputData')  # Location for final datasets
# env.workspace = InputDataFolder

# Data
# Input CSV files for water shortage months
inputCSVFiles = arcpy.ListFiles('*.csv')
# Lists the 30-years from 1986 to 2015
listPast = list(range(1986, 2016))
# Lists the 30-years from 2041 to 2070
listFuture = list(range(2041, 2071))
# Fields in the output CSV files: number of shortage months for each HUC and year
outputFields = ['HUC', 'Year', 'Months']

try:
    print('Starting analysis for Water Shortage:')
    
    ############ This section goes below #############################################################################
    # # Create a new CSV file to store one climate model's output data.
    # outputFilename = 'testCSV.csv'
    # outputCSV = os.path.join(OutputDataFolder, outputFilename)
    # testRows = [['c1001', '1950', 3],
    #             ['c1001', '1951', 5]]
    # with open(outputCSV, 'w', newline='') as csvFile:  # w is the write mode, newline eliminates blank line between rows
    #     csvwriter = csv.writer(csvFile)
    #     csvwriter.writerow(outputFields)
    #     csvwriter.writerows(testRows)
    ##################################################################################################################
    
    # Loop through the input CSV files in the water shortage folder.
    for file in inputCSVFiles:
        # Create a new CSV file to store one climate model's output data.
        # Store the base name of the file in a variable. This will be part of the name of the output file.
        outputFileName = os.path.splitext(file)[0] + '_sm.csv'  # sm = Shortage Months.
        # File location
        outputCSV = os.path.join(OutputDataFolder, outputFileName)
        outputData = [[]]
        with open(outputCSV,
                  'w',
                  newline='') as csvFile:  # w is the write mode, newline eliminates blank line between rows
            csvwriter = csv.writer(csvFile)  # Creates the CSV file.
            csvwriter.writerow(outputFields)  # Writes the column names.
            csvwriter.writerows(outputData)  # Writes data in the outputData variable.
        
        # Loop through the list variables that define the 30-year subsets for past and future.
        # Make a list of the lists!
        listVariables = [listPast, listFuture]
        for item in listVariables:
            # Loop through the years in the current list variable.
            print('    The list of years is:')
            for i in listPast:
                # Loop through the column names for each row
                # Turn the numbers in the list to text strings.
                textString = '"' + str(i) + '"'
                print('       {0}'.format(textString))
        
    print('Done!')
    print('Final CSV output tables are in: {}.'.format(OutputDataFolder))

except arcpy.ExecuteError as e:
    print(e)
    print('Drat! Curses!!')
