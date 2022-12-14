## Flight Path Analysis on ungulate winter ranges
# date: Feb. 27, 2020
# author: adali
# Main script: Given a folder with gpx flight files, create:
# - a line feature class with all flight paths,
# - a point feature class with all the flight points labelled with their flight name and corresponding
# uwr unit number, buffer distance, and incursion severity label. Note that a point in the gpx file 
# may be duplicated if it is within the uwr boundaries of different uwr's. It is also labelled with 
# the time interval the point represents and the total time of the flight path
# - a duplicate of the above point feature class with reduced points due to terrain masking (LOS analysis)
# - gdb table and excel table with time stats of all the points
# - gdb table and excel table with time stats of points after terrain masking (LOS analysis)
# update: March 2, 2020 - added additional field for uwr number 
# udpate: June 15, 2020 - added new variable bufferDistList

import arcpy
import os
import datetime
import time
#from statistics import median
import tempfile
import pandas as pd
from arcpy.sa import *

import flightPathAnalysis_Functions

def getFlightLinePoints(gpxFolder, outputGDB, finalFlightLineName, finalFlightPointName, DEM, unit_no, unit_no_id, uwrBuffered, IncursionSeverity, generalFolder):
    """
    (string, string, string, string, string, string, dictionary, string) -> None

    Inputs:
    gpxFolder: Full path to folder with the gpx files input
    outputGDB: Full path to GDB to put all the output feature classes
    finalFlightLineName: Name of output feature class for all flight lines
    finalFlightPointName: Name of output feature class for allflight points
    DEM: Full path to raster DEM input
    unit_no: field for unit number (eg. u-2-002)
    unit_no_id: field for unit number id (eg. TO 32)
    uwrBuffered: Full path to feature class of buffered uwr. output of flightPathAnalysis_Functions.createUWRBuffer()
    IncursionSeverity: Dictionary of buffer distance and incursion severity category ie. {500: "High", 1000: "Moderate}
    generalFolder: Full path to folder for error handling documents ie. excel/text files

    Output:
    - feature class with all flight paths
    - feature class with all flight points with corresponding uwr, incursion severity, time in seconds it represents, and total time for that flight
    - (potential) text file with list of gpx files that have 0 or 1 flight points

    Purpose: 
    > Make a single feature class with all flight paths in the folder of gpx flight path files.
    > Make a single feature class with all points from the gpx files that are below 500m. Each point is labelled with their flight name and corresponding
    uwr unit number, buffer distance, and incursion severity label. Note that a point in the gpx file may be duplicated if it is within the 
    uwr boundaries of different uwr's. It is also labelled with the time interval the point represents and the total time of the flight path.

    """
    print("Script starting...Start time: ", datetime.datetime.now())
    starttime = datetime.datetime.now()

    gdbNameUnformatted = "Temp" + gpxFolder[(gpxFolder.rfind("\\"))+1:] #name of temp gdb

    #get gdb name from folder name. Make sure they are all alphanumeric
    gdbName = flightPathAnalysis_Functions.replaceNonAlphaNum(gdbNameUnformatted, "_")

    with tempfile.TemporaryDirectory() as temp_location:
        #create temp gdb
        tempgdbName = str(gdbName) + ".gdb"
        tempgdbUnprojectedPath = temp_location + "\\" + tempgdbName
        print(tempgdbUnprojectedPath)
        arcpy.CreateFileGDB_management (temp_location, tempgdbName)

        # tmpdrive = 'T:'
        tempgdbRawPointsName = str(gdbName) + "_Raw.gdb"
        tempgdbRawPointsPath = os.path.join(temp_location, tempgdbRawPointsName)
        if not arcpy.Exists(tempgdbRawPointsPath):
            arcpy.CreateFileGDB_management (temp_location, tempgdbRawPointsName)

        # make tempgdb for individual flight lines
        tempgdbFlightLineName = str(gdbName) + "_FlightLine.gdb"
        tempgdbFlightLinePath = os.path.join(temp_location, tempgdbFlightLineName)

        if not arcpy.Exists(tempgdbFlightLinePath):
            arcpy.CreateFileGDB_management (temp_location, tempgdbFlightLineName)

        #dictionary of different time intervals and the flight points in each
        timeIntervalDict = {}

        #list of flight lines
        flightLines = []

        #count of flight lines
        flightCount = 0

        #total flight time in the season
        totalSeasonTime = 0

        #list of fc to check
        checkFilesList = []

        #converts all gpx files into fc
        for gpx in os.listdir(gpxFolder):
            if gpx.endswith(".gpx"):
                flightCount += 1
                #getting rid of non alphanumeric
                gpxFormattedName = flightPathAnalysis_Functions.replaceNonAlphaNum(gpx, "_")
                fcRawName = gpxFormattedName[:gpxFormattedName.find("_gpx")]

                fc = tempgdbRawPointsPath + "\\fc" + fcRawName
                print(fc)
            
                arcpy.env.workspace = tempgdbRawPointsPath
                arcpy.env.overwriteOutput = True

                arcpy.GPXtoFeatures_conversion(gpxFolder + "\\" + gpx, fc)

                #checks for files with consistent time intervals. All fc with same time intervals are grouped together in the dictionary
                # if there are more than 2 rows, find time between point recorded halfway through the flight and the point recorded right before it. else,time for objectid 3 - objectid 2 because there was an instance where there were 4 minutes between id 1 and 2.
                # assume only seconds between them the two points
                rowCount = arcpy.GetCount_management (fc)

                if int(rowCount[0]) > 2:

                    half = round(int(rowCount[0])/2)
                    sel = (half-1, half)

                    query = "OBJECTID in " + str(sel)
                    values = [f[0] for f in arcpy.da.SearchCursor(fc, "DateTime", query)]
                    timeInterval = flightPathAnalysis_Functions.convert_timedelta(values[1] - values[0])

                elif int(rowCount[0]) == 2:
                    query = "OBJECTID in (1,2)"
                    values = [f[0] for f in arcpy.da.SearchCursor(fc, "DateTime", query)]
                    timeInterval = flightPathAnalysis_Functions.convert_timedelta(values[1] - values[0])
                
                else:
                    checkFilesList.append(gpx)
                    continue

                #count number of points
                rowCount = arcpy.GetCount_management (fc)
                totalFlightTime = (float(rowCount[0])-1) * timeInterval

                #add unique flight ID and total time
                arcpy.AddField_management (fc, "FlightName", "TEXT")
                arcpy.AddField_management (fc, "TotalTime", "DOUBLE")
                with arcpy.da.UpdateCursor(fc, ["FlightName", "TotalTime"]) as cursor:
                    for row in cursor:
                        row[0] = fcRawName
                        row[1] = totalFlightTime
                        cursor.updateRow(row)
                del cursor

                arcpy.env.workspace = tempgdbFlightLinePath
                arcpy.env.overwriteOutput = True

                #make flight line
                indFlightLinePath = tempgdbFlightLinePath + "\\Line_" + fcRawName
                arcpy.PointsToLine_management (fc, indFlightLinePath)
                flightLines.append(indFlightLinePath)

                #get season time sum
                totalSeasonTime += totalFlightTime

                timeIntervalStr = "T" + flightPathAnalysis_Functions.replaceNonAlphaNum(str(timeInterval), "_")

                if timeIntervalStr not in timeIntervalDict:
                    timeIntervalDict[timeIntervalStr] = [timeInterval, [fc]]
                else:
                    timeIntervalDict[timeIntervalStr][1].append(fc)

        if len(checkFilesList) > 0:
            problemGPXText = open(os.path.join(generalFolder, "problemGPXFiles.txt"), "w")
            for i in checkFilesList:
                problemGPXText.write(i + "\n")
            problemGPXText.close()

        print("Runtime to convert gpx to features: ", datetime.datetime.now() - starttime)
        print("total season time in seconds:", totalSeasonTime)
        print("Total flight lines:", flightCount)

        print(flightLines)
        print(timeIntervalDict)
        print("number of different time intervals:" , len(timeIntervalDict))

        tempgdbUnprojectedName = str(gdbName) + "_Unprojected.gdb"
        tempgdbUnprojectedPath = temp_location + "\\" + tempgdbUnprojectedName
        if not arcpy.Exists(tempgdbUnprojectedPath):
            arcpy.CreateFileGDB_management (temp_location, tempgdbUnprojectedName)

        unprojectedFC = []

        currenttime = datetime.datetime.now()
        for interval in timeIntervalDict:
            arcpy.env.workspace = tempgdbUnprojectedPath
            arcpy.env.overwriteOutput = True

            #merge all points with same time interval together - WGS 84
            fcAllUnprojected = tempgdbUnprojectedPath + "\\" + interval + "_All_Unprojected"
            arcpy.Merge_management(timeIntervalDict[interval][1], fcAllUnprojected)
            print("merged all fc for", interval)
            
            #add fields
            arcpy.AddField_management(fcAllUnprojected, "AGL", "LONG")

            #turn on spatial analyst extension
            arcpy.CheckOutExtension("Spatial")

            #extract DEM
            arcpy.sa.ExtractMultiValuesToPoints(fcAllUnprojected, [[DEM, "DEMElev"]])

            #turn off spatial analyst extension
            arcpy.CheckInExtension("Spatial")

            #calculate AGL
            with arcpy.da.UpdateCursor(fcAllUnprojected, ["Elevation", "DEMElev", "AGL"]) as cursor:
                for row in cursor:
                    row[2] = row[0] - row[1]
                    if row[2] < 0:
                        row[2] = 0
                    cursor.updateRow(row)
            del cursor

            #made fc with only points less than 500m
            Points_lessthan500Height_Unprojected = "Points_lessthan500Height_Unprojected_" + interval
            arcpy.FeatureClassToFeatureClass_conversion (fcAllUnprojected, tempgdbUnprojectedPath, Points_lessthan500Height_Unprojected, "AGL < 500")

            #if table is empty ie. no points less than 500m elevation, move to the next flight
            rowCount = arcpy.GetCount_management(os.path.join(tempgdbUnprojectedPath, Points_lessthan500Height_Unprojected))
            if int(rowCount[0]) == 0:
                continue

            #add time interval field
            arcpy.AddField_management (os.path.join(tempgdbUnprojectedPath, Points_lessthan500Height_Unprojected), "TimeInterval", "DOUBLE")
            with arcpy.da.UpdateCursor(os.path.join(tempgdbUnprojectedPath, Points_lessthan500Height_Unprojected), "TimeInterval") as cursor:
                for row in cursor:
                    row[0] = timeIntervalDict[interval][0]
                    cursor.updateRow(row)
            del cursor

            #add to list of unprojected flight points
            unprojectedFC.append(os.path.join(tempgdbUnprojectedPath, Points_lessthan500Height_Unprojected))

        print("Runtime find points lower than 500m for each flight line: ", datetime.datetime.now() - currenttime)
        print(unprojectedFC)

        #merge unprojected flight points <500m AGL
        currenttime = datetime.datetime.now()
        Points_lessthan500Height_Unprojected_Final = os.path.join(tempgdbUnprojectedPath, "Points_lessthan500Height_Unprojected_Final")
        arcpy.Merge_management(unprojectedFC, Points_lessthan500Height_Unprojected_Final)
        print("Runtime to merge all points: ", datetime.datetime.now() - currenttime)

        #merge all unprojected flight lines
        allFlightLinesUnprojected = os.path.join(tempgdbUnprojectedPath, "allFlightLinesUnprojected")
        arcpy.Merge_management(flightLines, allFlightLinesUnprojected)

        tempgdbProjectedName = str(gdbName) + "_Projected.gdb"
        tempgdbProjectedPath = temp_location + "\\" + tempgdbProjectedName
        if not arcpy.Exists(tempgdbProjectedPath):
            arcpy.CreateFileGDB_management (temp_location, tempgdbProjectedName)

        arcpy.env.workspace = tempgdbProjectedPath
        arcpy.env.overwriteOutput = True

        points_lessthan500Projected = os.path.join(tempgdbProjectedPath, "Points_lessthan500heightProjected")

        currenttime = datetime.datetime.now()
        # if projected to bc albers projection, it will be an invalid extent for extracting DEM
        #merged points to right projection
        sr = arcpy.SpatialReference(3005)

        #project flight points
        arcpy.Project_management(
            Points_lessthan500Height_Unprojected_Final,
            points_lessthan500Projected,
            sr,
            "WGS_1984_(ITRF00)_To_NAD_1983"
        )
        print("Runtime to project flight points to bc albers: ", datetime.datetime.now() - currenttime)

        # field to distinguish height range
        currenttime = datetime.datetime.now()
        arcpy.AddField_management (points_lessthan500Projected, "HeightRange", "TEXT")
        with arcpy.da.UpdateCursor(points_lessthan500Projected, ["AGL", "HeightRange"]) as cursor:
            for row in cursor:
                if row[0] <=400:
                    row[1] = "0 to 400m"
                else:
                    row[1] = "400 to 500m"
                cursor.updateRow(row)
        del cursor
        print("Runtime to classify height range: ", datetime.datetime.now() - currenttime)

        currenttime = datetime.datetime.now()

        #fieldmapping to get only certain fields from uwr into Points_lessthan500Height

        #create field mapping and add flight point table
        fieldmappings = arcpy.FieldMappings()
        fieldmappings.addTable(points_lessthan500Projected)

        #create temp field mapping for uwr table
        uwrfms = arcpy.FieldMappings()
        uwrfms.addTable(uwrBuffered)

        #fields in uwr to keep
        uwrKeepFields = ["BUFF_DIST", unit_no, unit_no_id, "TUWR_TAG"] 

        #delete all other uwr fields
        fieldsDelete = [f.name for f in uwrfms.fields if f.name not in uwrKeepFields]
        for field in fieldsDelete:
            uwrfms.removeFieldMap(uwrfms.findFieldMapIndex(field))

        #add the kept uwr fields to field mapping
        for field in uwrKeepFields:
            field_map = uwrfms.fieldMappings[uwrfms.findFieldMapIndex(field)]
            fieldmappings.addFieldMap(field_map)

        Points_lessthan500Height_uwrbuffer = points_lessthan500Projected + "_height_UWR"
        #distinguish which uwr buffer zone it is in. Only points in zones will be in output
        arcpy.SpatialJoin_analysis (points_lessthan500Projected, uwrBuffered, Points_lessthan500Height_uwrbuffer, "JOIN_ONE_TO_MANY", "KEEP_COMMON", fieldmappings)
        print("Runtime to find uwr zones for points: ", datetime.datetime.now() - currenttime)

        arcpy.env.workspace = outputGDB
        arcpy.env.overwriteOutput = True

        currenttime = datetime.datetime.now()
        #project flight lines to outputgdb
        allFlightLinesProjected = os.path.join(outputGDB, finalFlightLineName)
        arcpy.Project_management(
            allFlightLinesUnprojected,
            allFlightLinesProjected,
            sr,
            "WGS_1984_(ITRF00)_To_NAD_1983"
        )
        print("Runtime to project flight lines to bc albers: ", datetime.datetime.now() - currenttime)

        #if table is empty ie. no points within uwr buffer zones, no need to get a table
        print(Points_lessthan500Height_uwrbuffer)
        rowCount = arcpy.GetCount_management(Points_lessthan500Height_uwrbuffer)
        if int(rowCount[0]) == 0:
            raise SystemExit("No flight lines intersect with uwr buffers")

        #add incursion severity according to buffer range
        arcpy.AddField_management (Points_lessthan500Height_uwrbuffer, "IncursionSeverity", "TEXT")
        with arcpy.da.UpdateCursor(Points_lessthan500Height_uwrbuffer, ["BUFF_DIST", "IncursionSeverity"]) as cursor:
            for row in cursor:
                row[1] = IncursionSeverity[row[0]]
                cursor.updateRow(row)
        del cursor

        # split points of each incursion severity into different feature classes
        for severity in IncursionSeverity:
            name = "Below500m_" + str(severity)
            arcpy.FeatureClassToFeatureClass_conversion(Points_lessthan500Height_uwrbuffer, outputGDB, name, "IncursionSeverity = '" + str(IncursionSeverity[severity]) + "'")
            print("Made separate feature class for", IncursionSeverity[severity], "incursion points")

        #copy final fc to outputgdb
        currenttime = datetime.datetime.now()
        arcpy.FeatureClassToFeatureClass_conversion(Points_lessthan500Height_uwrbuffer, outputGDB, finalFlightPointName)
        print("Runtime to copy Points_lessthan500Height_uwrbuffer: ", datetime.datetime.now() - currenttime)

        print("Final point file saved at", Points_lessthan500Height_uwrbuffer)
        print("Runtime to get all points: ", datetime.datetime.now() - starttime)
        time.sleep(10)


def main():
    #folder with the gpx files input
    #gpxFolder = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\test_20200225\sampleGPXFlights"
    gpxFolder = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\data\work\gpxSample"

    #raster DEM input
    DEM = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\test_May15\Input.gdb\bc_elevation_25m_bcalb_Clip2"

    #gdb containing original ungulate winter range layer
    origUWRGDB = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\testAllSkeena\testInputAll.gdb"
    
    #name of original ungulate winter range layer
    origUWRName = "tuwra_u6002_BufferFinal"

    #unit number field eg. u-2-002
    unit_no = "TUWR_TAG"

    #unit number id field eg. TO 60
    unit_no_id = "UNIT_NO"
            
    #uwr uniqud id field - field that combines uwr number and uwr unit number
    uwr_unique_Field = "uwr_unique_id"

    #list of buffer distances (in meters)
    bufferDistList = [500, 1000, 1500]

    #GDB to put all the output feature classes
    outputGDB = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\20200915\Output_20200915.gdb"

    #feature class of buffered uwr. output of flightPathAnalysis_Functions.createUWRBuffer() and input used for getFlightLinePoints() and flightPathAnalysis_Functions.LOS_analysis()
    #uwrBuffered = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\test_May15\Input.gdb\tuwra_u6002_BufferFinal"
    uwrBuffered = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\testAllSkeena\testInputAll.gdb\tuwra_u6002_BufferFinal"

    #buffer distance and incursion severity category used in getFlightLinePoints()
    IncursionSeverity = {0: "In UWR", 500: "High", 1000: "Moderate", 1500: "Low"}

    #folder to put excel/text files for error handling documents or stats
    generalFolder = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\20200915"

    ### terrain masking

    #viewshed feature class
    viewshed = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\20200915\Output_20200915.gdb\viewshed20200914"

    #agl viewshed feature class
    minElevViewshed = r"W:\srm\sry\Local\projlib\StewBase\Mountain_Goat\FlightLine_UWR_Analysis\20200915\Output_20200915.gdb\minElevViewshed20200515"

    #count of points in direct viewshed excel file
    ViewshedPointCount = "ViewshedPointCount_20200915"

    #name of feature class for the output flight line of getFlightLinePoints()
    outputFlightLineName = "FlightLineSkeenaAll_20200915"

    #name of feature class for the output flight points of getFlightLinePoints() (before terrain masking)
    allFlightPoint = "allFlightPoint"

    # max buffer range. See IncursionSeverity dictionary
    maxRange = 1500

    #stats for output flight points of getFlightLinePoints() (before terrain masking)
    allPointsStats_Name = "PointsGeneralStatSkeenaAll_Stats_20200915"

    #name of feature class for the output final flight points (after terrain masking)
    LOS_uwrFlightPoints = "FlightPointSkeenaAll_TerrainMasked_20200915"
    #gdb for the output final flight points (after terrain masking)
    LOS_uwrFlightPointsGDB = outputGDB

    # stats for final ouput flight points (after terrain masking)
    finalPointsStats_Name = "PointsGeneralStatSkeenaAll_TerrainMasked_Stats_20200915"

    #########don't change the stuff here:
    allPointsStats_Excel = os.path.join(generalFolder, allPointsStats_Name) + ".xlsx" #don't need to change
    finalPointsStats_Excel = os.path.join(generalFolder, finalPointsStats_Name) + ".xlsx" #don't need to change

    ######################################

    # Create the buffered uwr feature class. If the final fc exists, only uwr units that aren't in the final fc will be made and appended to it
    flightPathAnalysis_Functions.createUWRBuffer(origUWRGDB, origUWRName, outputGDB, unit_no, unit_no_id, uwr_unique_Field, uwrBuffered, bufferDistList)

    #no need to filter uwrBuffered with required uwr
    getFlightLinePoints(gpxFolder, outputGDB, outputFlightLineName, allFlightPoint, DEM, unit_no, unit_no_id, uwrBuffered, IncursionSeverity, generalFolder)

    #summary stats
    #Note:frequency table cannot be made in datasets
    arcpy.env.workspace = outputGDB
    arcpy.env.overwriteOutput = True

    allPointsStats_FullPath = os.path.join(outputGDB, allPointsStats_Name) #don't change!!

    currenttime = datetime.datetime.now()
    arcpy.Statistics_analysis (os.path.join(outputGDB, allFlightPoint), allPointsStats_FullPath, [["TimeInterval", "SUM"]], ["FlightName", "HeightRange", "BUFF_DIST", unit_no, unit_no_id, "TotalTime", "IncursionSeverity"])
    try:
        os.remove(allPointsStats_Excel)
    except:
        pass
    arcpy.TableToExcel_conversion(allPointsStats_FullPath, allPointsStats_Excel)
    print("Runtime to calculate general stats: ", datetime.datetime.now() - currenttime)

    flightPathAnalysis_Functions.LOS_Analysis(uwrBuffered, maxRange, DEM, viewshed, minElevViewshed, unit_no, unit_no_id, uwr_unique_Field, os.path.join(outputGDB, allFlightPoint), LOS_uwrFlightPointsGDB, LOS_uwrFlightPoints, generalFolder, ViewshedPointCount)

    # name of feature class for the output final flight points (after terrain masking)
    finalPoints_Masked = os.path.join(LOS_uwrFlightPointsGDB, LOS_uwrFlightPoints) ##don't change!!
    # full path to gdb table of stats of output final flight points (after terrain masking)
    finalPointsStats_FullPath = os.path.join(outputGDB, finalPointsStats_Name) ##don't change!!

    #stats for final points after terrain masking
    currenttime = datetime.datetime.now()
    arcpy.Statistics_analysis (finalPoints_Masked, finalPointsStats_FullPath, [["TimeInterval", "SUM"]], ["FlightName", "HeightRange", "BUFF_DIST", unit_no, unit_no_id, "TotalTime", "IncursionSeverity"])
    try:
        os.remove(finalPointsStats_Excel)
    except:
        pass
    arcpy.TableToExcel_conversion(finalPointsStats_FullPath, finalPointsStats_Excel)
    print("Runtime to calculate stats for terrain masked pts: ", datetime.datetime.now() - currenttime)
    
    print("Script completed!!")

if __name__ == "__main__":
    main()
