### functions used in flight path analysis on ungulate winter ranges
# date: Feb. 27, 2020
# author: adali
# Function: makeViewshed - requires 3D analyst/spatial analyst extensions to be TURNED ON/CHECKED OUT
# Function: LOS_Analysis - requires the user to HAVE ACCESS to 3D analyst/spatial analyst extensions
# update: March 3, 2020 - added uwr number field and unique uwr id (combo of uwr number and uwr unit number)
# update: April 29, 2020 - fixed replaceNonAlphaNum len(myText, not len(myText)-1. Deleted +1 in rawBuffer function in origFCName[:origFCName.find(".shp")+1])
# update: May 15, 2020 - fixed rawBuffer() replacing non alpha/num for origName 
# update: May 19, 2020 - fixed bug that makes the script crash when removing the temp directory in LOS_analysis and findBufferRange
# update: June 8, 2020 - createUWRBuffers looks at original uwr to see if buffers have already been made. if so, it won't make any
# update: June 12, 2020 - createUWRBuffers has a new argument for a list of buffer distances instead of it being hard coded to 500, 1000, 1500
# update: Aug 18, 2020 - for uwr_number + uwr number id to get unique id: converts both into str first.
# update: Sept. 15, 2020 - makeViewshed() not only uses uwr vertices of uwr as observation points to make the viewshed, 
# but also uses any points in the uwr that have elevations higher than the lowest vertex.
# makeViewshed() does not create skyline barriers anymore. it makes a viewshed layer with the minimum elevation required 
# from the ground before objects can be seen from the observation points. In LOS_Analysis(), the flight points are compared to the direct viewshed and minElevViewshed.
# If the AGL of the flight points are higher than the elevation in the minElevViewshed, they are not terrain masked.
# update: Dec. 14, 2020 - instead of merging after all viewsheds are made, it will add newly created viewsheds to the viewshed layer as they are made

import arcpy
import datetime
import time
import os
from math import sqrt
import tempfile
import pandas as pd
import subprocess


def replaceNonAlphaNum(myText, newXter):
    '''
    Purpose:  to check for non-alpha numeric xters
    replace them with user defined new character
    '''
    # go thru each xter. Check to see if it's alphanumeric.
    # If not, then replace it with the new character (newXter)
    for x in range(0, len(myText)):
        if not myText[x].isalnum():
            myText = myText.replace(myText[x], newXter)
    return myText

def convert_timedelta(duration):
    #convert timedelta to seconds (type: float)
    seconds = duration.total_seconds()
    seconds = seconds % 60
    return seconds

def appendMergeFeatures(featuresList, finalPath):
    """
    Purpose:
    When the final feature class does not exist, the list of features will be merged.
    If the final feature class exists, the list of features will be appended.

    Note: if the final feature class exists, it assumes the list of features follow
    the same schema as the final feature class.

    Input:
    featuresList: List of feature classes
    finalPath: Full path of final feature class
    """
    if not arcpy.Exists(finalPath): #if feature class not exist
        print('need to make', finalPath)
        path = os.path.normpath(finalPath)
        pathSplit = path.split(os.sep)

        for s in pathSplit:
            if '.gdb' in s:
                indexGDB = pathSplit.index(s)
                gdbPathList = pathSplit[:indexGDB+1]
                gdbPath = os.path.join(*gdbPathList)
                if ':' in pathSplit[0]:
                    gdbPath = gdbPath.replace(":", ":\\")
                else:
                    gdbPath = r"\\" + gdbPath
                break

        #make gdb if not exist
        if not arcpy.Exists(gdbPath): #if gdb of finalPath not exist
            gdbFolderList = pathSplit[:indexGDB]
            gdbFolder = r"\\" + os.path.join(*gdbFolderList)
            arcpy.CreateFileGDB_management(gdbFolder, pathSplit[indexGDB])
            print("created", pathSplit[indexGDB], "in", gdbFolder)
            print('made gdb', gdbPath)

        #make dataset if not exist
        datasetPathList = pathSplit[:indexGDB + 2]
        datasetPath = r"\\" + os.path.join(*datasetPathList)
        if '.gdb' not in pathSplit[-2]:
            arcpy.CreateFeatureDataset_management (gdbPath, r"\\" + pathSplit[indexGDB + 1])
            print('made', datasetPath)

        #merge all features to final layer
        arcpy.Merge_management(featuresList, finalPath) 
        print('made final layer', finalPath)

    else:
        #append all new features into old layer
        arcpy.Append_management(featuresList, finalPath)
        print('appended all features to the final layer', finalPath)

## functions to create uwr buffer

def rawBuffer(origFCLoc, origFCName, bufferDistanceInput, bufferNumber, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field):
    """
    (string, string, string, int, string) -> string, string
    origFCLoc: Folder of feature class to be buffered
    origFCName: name of feature class or shapefile to be buffered
    bufferDistanceInput: buffer value and unit. example: "1500 Meters"
    bufferNumber: buffer value. Use for naming output feature class. Assume units in meters. example: 1500
    unit_no_Field: field of unit number
    unit_no_id_Field: field of unit number id
    uwr_unique_Field: field for unique uwr id made from combining unit number and unit number id

    Purpose:
    Buffers given input feature class or dissolved feature class.
    Returns names of output GDB and buffer fc name.

    Output: buffered feature class

    """
    if origFCName.find(".shp") >= 0:
        origName = replaceNonAlphaNum(origFCName[:origFCName.find(".shp")], "_") ###deleted +1 in  origFCName.find(".shp")+1
    else:
        origName = replaceNonAlphaNum(origFCName, "_")

    rawBuffer = "rawBuffer_" + origName + "_" + str(bufferNumber)

    arcpy.Buffer_analysis (os.path.join(origFCLoc, origFCName), os.path.join(outputGDB, rawBuffer), bufferDistanceInput)
    print("buffered", origName, "by", bufferDistanceInput)
    arcpy.DeleteField_management(os.path.join(outputGDB, rawBuffer), ["ORIG_FID"])
    
    #Add field that combines unit_no and unit_no_id
    arcpy.AddField_management(os.path.join(outputGDB, rawBuffer), uwr_unique_Field, "TEXT")
    with arcpy.da.UpdateCursor(os.path.join(outputGDB, rawBuffer), [uwr_unique_Field, unit_no_Field, unit_no_id_Field]) as cursor:
        for row in cursor:
            row[0] = str(row[1]) + "__" + str(row[2])
            cursor.updateRow(row)
    del cursor

    return outputGDB, rawBuffer

def findBufferRange(ToEraseLoc, ToEraseName, UsetoErasePath, uniqueUWR_IDFields, outputGDB):
    """
    (string, string, string, string, string) -> None
    ToEraseLoc: Folder of feature class to be erased
    ToEraseName: Name of feature class to be erased
    UsetoErasePath: Full path location of feature class used to erase the incoming feature class
    uniqueUWR_IDFields: List of unique ID fields of the incoming feature class used to identify for erasing. Has to be a list!!
    outputGDB: Gdb for the output

    Given two feature classes with the same unique field, erase area in each feature in the first
    feature class according to the area of the matching id in the other feature class.

    Returns name of full path to feature class of results of erase analysis
    Output: feature class of erase analysis

    Note: Time used to erase ~3 seconds/record, ~10 min to merge a 1500 record uwr

    """

    print("Erasing starting...Start time: ", datetime.datetime.now())
    starttime = datetime.datetime.now()

    with tempfile.TemporaryDirectory() as temp_location:
        #create temp gdb to store each fc output from erasing with each feature
        tempgdbName = str(ToEraseName) + ".gdb"
        tempgdbPath = temp_location + "\\" + tempgdbName
        arcpy.CreateFileGDB_management (temp_location, tempgdbName)

        arcpy.env.workspace = tempgdbPath
        arcpy.env.overwriteOutput = True

        arcpy.MakeFeatureLayer_management(UsetoErasePath, "UsetoEraseFL") 
        arcpy.MakeFeatureLayer_management(os.path.join(ToEraseLoc, ToEraseName), "ToEraseFL")
        
        #count of features
        out_count = 0

        #list of erased fc to merge together
        bufferedFeatures = []

        #select unique id in each fc and erase area of second fc from first fc. Creates a fc output

        with arcpy.da.SearchCursor("UsetoEraseFL", uniqueUWR_IDFields) as cursor:
            for row in cursor:
                #for field in uniqueUWR_IDFields or i in index

                if type(row[0]) == int: #if id field is integer
                    query = '(\"' + uniqueUWR_IDFields[0] + '\" = ' + str(row[0]) + ')'
                else:
                    query = '(\"' + uniqueUWR_IDFields[0] + '\" = \'' + str(row[0]) + "')"

                countField = len(uniqueUWR_IDFields)
                if countField > 1:
                    for i in range(1, countField):
                        if type(row[i]) == int: #if id field is integer
                            query += 'AND (\"' + uniqueUWR_IDFields[i] + '\" = ' + str(row[i]) + ')'
                        else:
                            query += 'AND (\"' + uniqueUWR_IDFields[i] + '\" = \'' + str(row[i]) + "')"

                out_count += 1
                out_features = arcpy.env.workspace + "\\outfeature" + str(out_count)
                arcpy.SelectLayerByAttribute_management("UsetoEraseFL", "NEW_SELECTION", query)
                arcpy.SelectLayerByAttribute_management("ToEraseFL", "NEW_SELECTION", query)
                arcpy.Erase_analysis("ToEraseFL", "UsetoEraseFL", out_features)
                bufferedFeatures.append(out_features)
                if (out_count % 250) == 0:
                    print(out_count, "features done")
        del cursor

        arcpy.Delete_management("UsetoEraseFL")
        arcpy.Delete_management("ToEraseFL")

        print("Runtime to erase: ", datetime.datetime.now() - starttime, ". Merging them now")

        currenttime = datetime.datetime.now()

        arcpy.env.workspace = outputGDB
        arcpy.env.overwriteOutput = True

        #merge all outputs
        outputPath = ToEraseLoc + "\\" + ToEraseName + "Only"
        arcpy.Merge_management(bufferedFeatures, outputPath)

        print("Runtime to merge: ", datetime.datetime.now() - currenttime)

        arcpy.ClearWorkspaceCache_management() #bug in arcpro 2.6.1. Creating gdb will create lock. This clears it
        print("Total runtime to find buffer range for", ToEraseName, ":", datetime.datetime.now()-starttime)
    return outputPath

##just use this function to create uwr buffer
def createUWRBuffer(origUWRGDB, origUWRName, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field, finalFC, bufferDistList):
    # 
    # Create 'donut' buffers of uwr polygons with buffer numbers in bufferDistList from the original uwr feature class
    # If the final buffer feature class exists, only uwr that's in the original uwr layer but not in the final layer 
    # will be buffered and added to the final feature class. If the final feature class does not exist, a new one will be made.
    #
    # eg. bufferDistList = [500,1000,1500]
    # Outputs:
    # 500m buffer polygon = the 500m donut shape outside of the uwr polygon. (0-500m buffer area)
    # 1000m buffer polygon = the 500m donut shape outside of the 500m buffer polygon. (500-1000m buffer area)
    # 1500m buffer polygon = the 500m donut shape outside of the 1000m buffer polygon. (1000-1500m buffer area)

    arcpy.env.workspace = outputGDB
    arcpy.env.overwriteOutput = True

    scriptstarttime = datetime.datetime.now()

    origUWRPath = os.path.join(origUWRGDB, origUWRName)

    #get list of relevant UWR
    uwrSet = set()
    with arcpy.da.SearchCursor(origUWRPath, [unit_no_Field, unit_no_id_Field] ) as cursor:
        for row in cursor:
            uwrSet.add(str(row[0]) + "__" + str(row[1]))
    del cursor

    print(uwrSet)

    #get list of uwr that have buffers created
    CreatedUWRSet = set()
    if arcpy.Exists(finalFC): #if final buffer fc exists
        finalFCExist = 'Yes'
        with arcpy.da.SearchCursor(finalFC, [uwr_unique_Field]) as cursor:
            for row in cursor:
                CreatedUWRSet.add(row[0])
        del cursor
        print("uwr already buffered:", CreatedUWRSet)

        #get list of uwr that do not have buffers created
        UWRRequireSet = uwrSet - CreatedUWRSet
        print("need to make buffers for uwr:", UWRRequireSet)
    else:
        finalFCExist = 'No'
        UWRRequireSet = uwrSet

    #create viewsheds for uwr that don't have any. Either makes a new final viewshed layer or appends to the old viewshed
    if len(UWRRequireSet) > 0:
        if finalFCExist == 'Yes':
            
            # make new field in copy of orig UWR fc for unique uwr id. DIFFERENT FROM uniqueuwrid. make it so that there's no way this field existed before
            arcpy.FeatureClassToFeatureClass_conversion(origUWRPath, outputGDB, 'tempFC')
            
            tempOrigFCCopy = os.path.join(outputGDB, 'tempFC')

            fieldnames = [field.name for field in arcpy.ListFields(tempOrigFCCopy)]
            tempUniqueUWRField = "tempUniqueUWRField"
            if tempUniqueUWRField not in fieldnames:
                arcpy.AddField_management(tempOrigFCCopy, tempUniqueUWRField, "TEXT")
            # populate unique uwr id
            with arcpy.da.UpdateCursor(tempOrigFCCopy, [unit_no_Field, unit_no_id_Field, tempUniqueUWRField] ) as cursor:
                for row in cursor:
                    row[2] = str(row[0]) + "__" + str(row[1])
                    cursor.updateRow(row)
            del(cursor)
            
            # make query with UWRRequireSet
            uwrList_string = "','".join(UWRRequireSet)
            query = tempUniqueUWRField + " in ('" + uwrList_string + "')" 
            print(query)

            UnbufferedFL = "UnbufferedUWR"
            arcpy.MakeFeatureLayer_management(tempOrigFCCopy, UnbufferedFL, query)

            RequireUWRLayer = UnbufferedFL
        else:
            RequireUWRLayer = origUWRPath


        uniqueIDFields = [unit_no_Field, unit_no_id_Field]

        #Dissolves input feature class by dissolveFields list if list is given. This is to avoid errors for multi part uwr
        # that have been split into separate features in the original uwr feature class.
        dissolvedOrigLoc =  r"memory"
        dissolvedOrigName = "dissolve"
        dissolvedOrigPath = os.path.join(dissolvedOrigLoc, dissolvedOrigName)
        arcpy.Dissolve_management(RequireUWRLayer, dissolvedOrigPath, uniqueIDFields)
        arcpy.DeleteField_management(dissolvedOrigPath, ["ORIG_FID"])

        #start list of intermediate features to be deleted
        UWROnly = "BufferUWROnly"
        delFC = [os.path.join(outputGDB, UWROnly)]

        #Create raw buffers
        #sort buffer distances. does not include 0 = uwr
        bufferDistList.sort()
        rawBufferDict = {}
        for bufferDist in bufferDistList:
            rawBufferLoc, rawBufferName = rawBuffer (dissolvedOrigLoc, dissolvedOrigName, str(bufferDist) + " Meters", bufferDist, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field)
            rawBufferDict[bufferDist] = [rawBufferLoc, rawBufferName]
            delFC.append(os.path.join(rawBufferLoc, rawBufferName))

        #Get donut shaped buffers to only get 
        #list of buffered donut polygons that will be merged together to get the final fc

        requireMergeBufferList = [os.path.join(outputGDB, UWROnly)]

        ##add to requireMergeBufferList

        #list of keys in rawBufferDict ie. buffer distance in ascending order
        sortBuffDistList = list(sorted(rawBufferDict))

        #goes through each buffer distance to get the 'donut' shapes of only the area for each buffer distance
        for bufferDist in sortBuffDistList:
            if sortBuffDistList.index(bufferDist) == 0: #smallest buffer that is not the orig uwr
                onlyBufferDist = findBufferRange(rawBufferDict[bufferDist][0], rawBufferDict[bufferDist][1], os.path.join(dissolvedOrigLoc, dissolvedOrigName), uniqueIDFields, outputGDB)
            else:
                prevIndex = sortBuffDistList.index(bufferDist) - 1
                prevBufferDist = sortBuffDistList[prevIndex]
                onlyBufferDist = findBufferRange(rawBufferDict[bufferDist][0], rawBufferDict[bufferDist][1], os.path.join(rawBufferDict[prevBufferDist][0], rawBufferDict[prevBufferDist][1]), uniqueIDFields, outputGDB)

            requireMergeBufferList.append(onlyBufferDist)
            delFC.append(onlyBufferDist)

        #dissolve original uwr by the dissolveFields
        origUWRPathMemory = r"memory\dissolveuwrOnly" + UWROnly
        arcpy.Dissolve_management(RequireUWRLayer, origUWRPathMemory, uniqueIDFields)
        arcpy.DeleteField_management(origUWRPathMemory, ["ORIG_FID"])

        #Create copy of original uwr and add BUFF_DIST field and add unique uwr id field that combines unit_no and unit_no_id
        arcpy.FeatureClassToFeatureClass_conversion (origUWRPathMemory, outputGDB, UWROnly)
        arcpy.AddFields_management(os.path.join(outputGDB, UWROnly), [["BUFF_DIST", "DOUBLE"], [uwr_unique_Field, "TEXT"]])
        with arcpy.da.UpdateCursor(os.path.join(outputGDB, UWROnly), ["BUFF_DIST", uwr_unique_Field, unit_no_Field, unit_no_id_Field]) as cursor:
            for row in cursor:
                row[0] = 0
                row[1] = str(row[2]) + "__" + str(row[3])
                cursor.updateRow(row)
        del cursor
        arcpy.Delete_management(origUWRPathMemory)

        #Append uwr into the final feature class or create a new feature class
        appendMergeFeatures(requireMergeBufferList, finalFC)

        if finalFCExist == 'Yes':
            arcpy.Delete_management(UnbufferedFL)
            arcpy.Delete_management(tempOrigFCCopy)
            arcpy.DeleteField_management(origUWRPath, tempUniqueUWRField)

        print("Runtime to make final uwr buffer:", finalFC, ":", datetime.datetime.now() - scriptstarttime)

        #clean up files
        for fc in delFC:
            arcpy.Delete_management(fc)

    else:
        print('no need to make new uwr buffers')
##functions for terrain masking


##just use this function to create viewshed
def makeViewshed(uwrList, uwr_bufferFC, buffDistance, unit_no_Field, unit_no_id_Field, uwr_unique_Field, tempGDBPath, DEM, viewshed, minElevViewshed):
    """
    (list, string, integer, string, string, string, optional: string) -> None
    uwrList: List of uwr to make viewsheds for each uwr
    uwr_bufferFC: Feature class of uwr with buffers
    buffDistance: Maximum buffer distance
    unit_no_Field: field for unit number (eg. u-2-002)
    unit_no_id_Field: field for unit number id (eg. TO 32)
    uwr_unique_Field: field made in the buffered_uwr layer and viewshed layer that is a combo of unit_no and unit_no_id
    tempGDBPath: Gdb to store intermediate files
    DEM: Raster DEM
    viewshed: Existing viewshed layer or path to a new viewshed layer
    minElevViewshed: Existing min Elevation viewshed layer or path to a new layer. This contains the minimum elevation required for current ground level areas not visible to be visible

    Purpose: For each uwr in the list of uwr, create a viewshed layer and another viewshed layer 
    with minimum height required for objects in currently non visible areas to be visible.
    If the viewshed feature class exists, the new viewsheds created will be appended to them.

    Note: requires 3D and spatial analyst extension to be turned on

    """
    
    arcpy.env.workspace = tempGDBPath
    arcpy.env.overwriteOutput = True

    UWR_noBuffer = "UWR_noBuffer"
    UWRVertices = "UWRVertices"
    UWR_Buffer = "UWR_Buffer"

    #name of feature layers
    UWR_noBuffer_FL = "UWR_noBuffer_FL"
    UWRVertices_FL = "UWRVertices_FL"
    UWR_Buffer_FL = "UWR_Buffer_FL"
    UWR_DEMPoints_FL = "UWR_DEMPoints_FL"
    polygonViewshed_FL = "polygonViewshed_FL"
    polygon_aglViewshed_FL = "polygon_aglViewshed_FL"

    starttime = datetime.datetime.now()
    #make feature class with relevant UWR - 0m buffer
    uwrSet_str = "','".join(uwrList)

    arcpy.FeatureClassToFeatureClass_conversion(uwr_bufferFC, tempGDBPath, UWR_noBuffer, uwr_unique_Field + r" in ('" + uwrSet_str + r"') and BUFF_DIST = 0")

    ##subprocess.run(["cscript", r""])

    arcpy.MakeFeatureLayer_management(UWR_noBuffer, UWR_noBuffer_FL)

    #generalize uwr - 0m buffer
    arcpy.Generalize_edit(UWR_noBuffer)

    #convert uwr polygons to vertices
    arcpy.FeatureVerticesToPoints_management(UWR_noBuffer, UWRVertices)
    
    # get DEM of vertices
    arcpy.sa.ExtractMultiValuesToPoints(UWRVertices, [[DEM, "DEMElev"]])
    arcpy.MakeFeatureLayer_management(UWRVertices, UWRVertices_FL)

    #make feature class with relevant UWR buffered. includes all buffer distances
    arcpy.FeatureClassToFeatureClass_conversion(uwr_bufferFC, tempGDBPath, UWR_Buffer, uwr_unique_Field + " in ('" + uwrSet_str + "')") #  and BUFF_DIST = " + str(buffDistance)
    arcpy.MakeFeatureLayer_management(UWR_Buffer, UWR_Buffer_FL)
    print("Runtime to make feature layer of uwr - buffer: ", datetime.datetime.now() - starttime)

    # uwrViewshedList = []
    # agl_uwrViewshedList = []

    for uwr in uwrList:
        uwrstarttime = datetime.datetime.now()
        name_uwr = replaceNonAlphaNum(uwr, "_")
        rasterViewshed = "rasterViewshed_" + name_uwr
        agl_rasterViewshed = "agl_rasterViewshed" + name_uwr
        polygonViewshed = "polygonViewshed_" + name_uwr
        totalViewshed = "totalViewshed_" + name_uwr
        totalViewshed_dis = totalViewshed + "dis"
        int_aglViewshed = "int_aglViewshed" + name_uwr
        polygon_aglViewshed = "polygon_aglViewshed" + name_uwr
        dissolved_aglViewshed = "dissolved_aglViewshed" + name_uwr

        UWR_DEMClip = "DEMClip" + name_uwr
        BufferUWR_DEMClip = "Buffer_DEMClip_" + name_uwr
        UWR_DEMPoints = "UWR_DEMPoints" + name_uwr
        UWRBuffer_DEMPoints = "UWRBuffer_DEMPoints" + name_uwr
        UWR_ViewshedObsPoints = "UWR_ViewshedObsPoints" + name_uwr

        uwr_no = uwr[:uwr.find("__")]
        uwr_no_id = uwr[uwr.find("__")+2:]

        ##check to find the right query depending on if uwr fields are integer or text
        with arcpy.da.SearchCursor(UWR_Buffer_FL, [unit_no_Field, unit_no_id_Field]) as cursor:
            for row in cursor:
                if type(row[0]) == int: #if unit_no_Field field is integer
                    uwrQuery = '(\"' + unit_no_Field + '\" = ' + uwr_no + ')'
                else:
                    uwrQuery = '(\"' + unit_no_Field + '\" = \'' + uwr_no + "')"

                if type(row[1]) == int: #if unit_no_id_Field field is integer
                    uwrQuery += ' AND (\"' + unit_no_id_Field + '\" = ' + uwr_no_id + ')'
                else:
                    uwrQuery += ' AND (\"' + unit_no_id_Field + '\" = \'' + uwr_no_id + "')"
                break
        print("working on", uwr)

        viewshedstarttime = datetime.datetime.now()

        #select uwr
        arcpy.SelectLayerByAttribute_management(UWR_Buffer_FL, "NEW_SELECTION", uwrQuery)

        #get extent of biggest uwr buffer to clip raster and get all incursion raster area (uwr+buffer)
        with arcpy.da.SearchCursor(UWR_Buffer_FL, ['SHAPE@'], "BUFF_DIST = " + str(buffDistance)) as cursor:
            for row in cursor:
                extent = row[0].extent
                UWR_Buffer_ExtentList = [str(extent.XMin), str(extent.YMin), str(extent.XMax), str(extent.YMax)]    
                UWR_Buffer_Extent = " ".join(UWR_Buffer_ExtentList)
                break
        del cursor

        #clip DEM to uwr + biggest buffer size
        arcpy.Clip_management(DEM, UWR_Buffer_Extent, BufferUWR_DEMClip, in_template_dataset= UWR_Buffer_FL, clipping_geometry='ClippingGeometry', maintain_clipping_extent='NO_MAINTAIN_EXTENT')

        #get extent of original uwr to clip raster and get uwr raster area
        arcpy.SelectLayerByAttribute_management(UWR_Buffer_FL, "NEW_SELECTION", uwrQuery + " and (BUFF_DIST = 0)")
        with arcpy.da.SearchCursor(UWR_Buffer_FL, ['SHAPE@']) as cursor:
            for row in cursor:
                extent = row[0].extent
                UWR_Buffer_ExtentList = [str(extent.XMin), str(extent.YMin), str(extent.XMax), str(extent.YMax)]    
                UWR_Buffer_Extent = " ".join(UWR_Buffer_ExtentList)
                break
        del cursor

        #clip dem to buffer - 0m
        arcpy.Clip_management(DEM, UWR_Buffer_Extent, UWR_DEMClip, in_template_dataset= UWR_Buffer_FL, clipping_geometry='ClippingGeometry', maintain_clipping_extent='NO_MAINTAIN_EXTENT')

        #convert raster to points
        arcpy.RasterToPoint_conversion(UWR_DEMClip, UWR_DEMPoints) #uwr DEM
        arcpy.AlterField_management(UWR_DEMPoints, "grid_code", "DEMElev", "DEMElev")

        arcpy.SelectLayerByAttribute_management(UWRVertices_FL, "NEW_SELECTION", uwrQuery)
        
        #get list of DEM values for uwr vertices
        DEMvalues = [row[0] for row in arcpy.da.SearchCursor(UWRVertices_FL, "DEMElev") if row[0] is not None]

        # get min value of vertices DEM list
        minValue = min(DEMvalues)

        # get raster DEM values in uwr that are higher than the min DEM value of vertices
        arcpy.MakeFeatureLayer_management(UWR_DEMPoints, UWR_DEMPoints_FL)
        arcpy.SelectLayerByAttribute_management(UWR_DEMPoints_FL, "NEW_SELECTION", "DEMElev > " + str(minValue))
        
        #add all higher than min DEM value points to vertices layer
        arcpy.Merge_management([UWR_DEMPoints_FL, UWRVertices_FL], UWR_ViewshedObsPoints )

        # make raster viewshed    
        arcpy.Viewshed_3d(BufferUWR_DEMClip, UWR_ViewshedObsPoints, rasterViewshed, out_agl_raster = agl_rasterViewshed)

        #make raster viewshed to polygon and includes actual uwr area into the viewshed
        arcpy.RasterToPolygon_conversion(rasterViewshed, polygonViewshed)
        arcpy.MakeFeatureLayer_management(polygonViewshed, polygonViewshed_FL)
        arcpy.SelectLayerByAttribute_management(polygonViewshed_FL, "NEW_SELECTION", "gridcode <> 0") #select all direct viewshed area
        arcpy.SelectLayerByAttribute_management(UWR_noBuffer_FL, "NEW_SELECTION", uwrQuery) #note: generalized polygon. if want actual area, will need original UWR
        arcpy.Merge_management([polygonViewshed_FL, UWR_noBuffer_FL], totalViewshed)
        arcpy.Dissolve_management(totalViewshed, totalViewshed_dis)
        
        #label viewshed with uwr name
        arcpy.AddFields_management(totalViewshed_dis, [[unit_no_Field, "TEXT"], [unit_no_id_Field, "TEXT"], [uwr_unique_Field, "TEXT"]])
        with arcpy.da.UpdateCursor(totalViewshed_dis, [unit_no_Field, unit_no_id_Field, uwr_unique_Field]) as cursor:
            for row in cursor:
                row[0] = uwr_no
                row[1] = uwr_no_id
                row[2] = uwr
                cursor.updateRow(row)
        del cursor

        #convert float agl viewshed raster to integer raster. Convert it to a polygon 
        arcpy.ddd.Int(agl_rasterViewshed, int_aglViewshed)
        arcpy.conversion.RasterToPolygon(int_aglViewshed, polygon_aglViewshed, "SIMPLIFY", "Value", "MULTIPLE_OUTER_PART", None)
        
        #all areas in direct viewshed are dissolved
        arcpy.MakeFeatureLayer_management(polygon_aglViewshed, polygon_aglViewshed_FL)
        arcpy.management.SelectLayerByAttribute(polygon_aglViewshed_FL, "NEW_SELECTION", "gridcode <= 0", None)
        arcpy.management.CalculateField(polygon_aglViewshed_FL, "gridcode", "0", "PYTHON3", '', "TEXT")
        arcpy.SelectLayerByAttribute_management(polygon_aglViewshed_FL, "CLEAR_SELECTION")
        arcpy.management.Dissolve(polygon_aglViewshed_FL, dissolved_aglViewshed, "gridcode", None, "MULTI_PART", "DISSOLVE_LINES")

        #label agl viewshed with uwr name
        arcpy.AddFields_management(dissolved_aglViewshed, [[unit_no_Field, "TEXT"], [unit_no_id_Field, "TEXT"], [uwr_unique_Field, "TEXT"]])
        with arcpy.da.UpdateCursor(dissolved_aglViewshed, [unit_no_Field, unit_no_id_Field, uwr_unique_Field]) as cursor:
            for row in cursor:
                row[0] = uwr_no
                row[1] = uwr_no_id
                row[2] = uwr
                cursor.updateRow(row)
        del cursor

        # #list of dissolved viewshed areas
        # uwrViewshedList.append(os.path.join(tempGDBPath, totalViewshed_dis))

        # #list of raster agl viewsheds
        # agl_uwrViewshedList.append(os.path.join(tempGDBPath, dissolved_aglViewshed))

        #print(uwrViewshedList)

        print("Runtime to make viewshed:", uwr, ":", datetime.datetime.now() - viewshedstarttime) 

        arcpy.Delete_management(polygon_aglViewshed_FL)
        arcpy.Delete_management(polygonViewshed_FL)
        arcpy.Delete_management(UWR_DEMPoints_FL)

        # append or merge recently made viewsheds together
        appendMergeFeatures([os.path.join(tempGDBPath, totalViewshed_dis)], viewshed)
        appendMergeFeatures([os.path.join(tempGDBPath, dissolved_aglViewshed)], minElevViewshed)

    # delete the feature layers or else there will be locking issues or the temp directory can't be removed
    arcpy.Delete_management(UWR_noBuffer_FL)
    arcpy.Delete_management(UWRVertices_FL)
    arcpy.Delete_management(UWR_Buffer_FL)


##just use this function to create viewshed and skyline AND conduct the analysis
def LOS_Analysis(uwrBuffered, maxRange, DEM, viewshed, minElevViewshed, unit_no_Field, unit_no_id_Field, uwr_unique_Field, allFlightPoints, LOS_uwrFlightPointsGDB, LOS_uwrFlightPoints, generalFolder, ViewshedPointCount):
    """
    (string, integer, string, string, string, string, string, string, string, string, string) -> None
    
    Inputs:
    uwrBuffered: Feature class of uwr with buffers
    maxRange: Maximum buffer distance
    DEM: Raster DEM
    viewshed: Existing viewshed layer or path to a new viewshed layer
    minElevViewshed: Existing min Elevation viewshed layer or path to a new laywer. This contains the minimum elevation required for current ground level areas not visible to be visible
    unit_no_Field: field for unit number (eg. u-2-002)
    unit_no_id_Field: field for unit number id (eg. TO 32)
    uwr_unique_Field: field for unique uwr id that combines unit_no_Field and unit_no_id_Field. Present in the viewshed/skyline/buffer layer
    allFlightPoints: Feature class of all flight points with uwr distinguished in column unit_no_Field
    LOS_uwrFlightPointsGDB: Output gdb to store final point feature class after LOS analysis
    LOS_uwrFlightPoints: Name of final output point feature class after LOS analysis
    generalFolder: Folder path to store excel/text files
    ViewshedPointCount: Excel file with count of points.
        -'points found in viewshed': number of pts found in the direct viewshed

    Output: feature class of flight points that have been terrain masked by line of sight

    Note 1 : If an uwr's viewshed has already been made in the viewshed layer, no viewshed will be made for it

    Note 2: Requires user to have permission to use 3D analyst and spatial analyst extensions
    """

    with tempfile.TemporaryDirectory() as temp_location:

        #create temp gdb
        tempgdbName = str('testing') + ".gdb"
        tempGDBPath = temp_location + "\\" + tempgdbName
        print(tempGDBPath)
        arcpy.CreateFileGDB_management (temp_location, tempgdbName)

        arcpy.env.workspace = tempGDBPath
        arcpy.env.overwriteOutput = True

        class LicenseError(Exception):
            pass

        #feature layers
        uwrFlightPoints_FL = "uwrFlightPoints_FL"
        viewshed_FL = "viewshed_FL"
        minElevViewshed_FL = "minElevViewshed_FL"

        #check out licenses
        try:
            if arcpy.CheckExtension("3D") == "Available":
                arcpy.CheckOutExtension("3D")
            else:
                raise LicenseError
            
            if arcpy.CheckExtension("Spatial") == "Available":
                arcpy.CheckOutExtension("Spatial")
            else:
                raise LicenseError

            scriptstarttime = datetime.datetime.now()

            #get list of relevant UWR
            starttime = datetime.datetime.now()
            uwrSet = set()
            with arcpy.da.SearchCursor(allFlightPoints, [unit_no_Field, unit_no_id_Field] ) as cursor:
                for row in cursor:
                    uwrSet.add(str(row[0]) + "__" + str(row[1]))
            del cursor

            #uwrSet = {"M-204", "M-216", "M-268", "M-314", "M-338"} #####################test. delete after , "M-337", "M-250"
            print(uwrSet)

            print("Runtime to get unique uwr: ", datetime.datetime.now() - starttime)

            #get list of uwr that have viewsheds created
            viewshedUWRSet = set()
            if arcpy.Exists(viewshed): #if viewshed exists
                with arcpy.da.SearchCursor(viewshed, [uwr_unique_Field]) as cursor:
                    for row in cursor:
                        viewshedUWRSet.add(row[0])
                del cursor
                print(viewshedUWRSet)

                #get list of uwr that do not have viewsheds created
                UWRRequireViewshedSet = uwrSet - viewshedUWRSet
                print(UWRRequireViewshedSet)
            else:
                UWRRequireViewshedSet = uwrSet

            # UWRRequireViewshedSet = ["M-273", "M-337", "M-250"] #####test

            #create viewsheds for uwr that don't have any. Either makes a new final viewshed layer or appends to the old viewshed
            if len(UWRRequireViewshedSet) > 0:
                makeViewshed(UWRRequireViewshedSet, uwrBuffered, maxRange, unit_no_Field, unit_no_id_Field, uwr_unique_Field, tempGDBPath, DEM, viewshed, minElevViewshed)
            else:
                print('no need to make viewsheds')

            finalPointsSet = set()

            arcpy.MakeFeatureLayer_management(allFlightPoints, uwrFlightPoints_FL)
            arcpy.MakeFeatureLayer_management(viewshed, viewshed_FL)
            arcpy.MakeFeatureLayer_management(minElevViewshed, minElevViewshed_FL)

            arcpy.env.workspace = tempGDBPath
            arcpy.env.overwriteOutput = True

            #count of points intersecting with the viewshed for each uwr
            viewshedPointCount = {}

            #list of feature classes with points that are not terrain masked
            uwr_notmasked_List = []

            for uwr in uwrSet:
                nameUWR = replaceNonAlphaNum(uwr, "_")
                points_aglViewshed = "points_aglViewshed" + nameUWR
                uwr_notmasked = "notMasked" + nameUWR
                print("finding LOS flight points for uwr ", uwr)
                LOS_uwrFlightPoints_FL = "LOS_uwrFlightPoints_FL"
                uwrPointsstarttime = datetime.datetime.now()
                uwrFlightPointsSet = set()
                NotLOSSet = set()

                uwr_no = uwr[:uwr.find("__")]
                uwr_no_id = uwr[uwr.find("__")+2:]

                ##check to find the right query depending on if uwr fields are integer or text
                with arcpy.da.SearchCursor(minElevViewshed_FL, [unit_no_Field, unit_no_id_Field]) as cursor:
                    for row in cursor:
                        if type(row[0]) == int: #if unit_no_Field field is integer
                            uwrQuery = '(\"' + unit_no_Field + '\" = ' + uwr_no + ')'
                        else:
                            uwrQuery = '(\"' + unit_no_Field + '\" = \'' + uwr_no + "')"

                        if type(row[1]) == int: #if unit_no_id_Field field is integer
                            uwrQuery += ' AND (\"' + unit_no_id_Field + '\" = ' + uwr_no_id + ')'
                        else:
                            uwrQuery += ' AND (\"' + unit_no_id_Field + '\" = \'' + uwr_no_id + "')"
                        break
                del cursor

                arcpy.SelectLayerByAttribute_management(minElevViewshed_FL, "NEW_SELECTION", uwrQuery +" AND (gridcode <> 0)")
                arcpy.SelectLayerByAttribute_management(uwrFlightPoints_FL, "NEW_SELECTION", uwrQuery)

                # all flight points associated with the uwr
                with arcpy.da.SearchCursor(uwrFlightPoints_FL, ["OBJECTID"]) as cursor:
                    for row in cursor:
                        uwrFlightPointsSet.add(row[0])
                del cursor

                # spatial join points with points that aren't in the direct viewshed but within the buffer zone
                arcpy.analysis.SpatialJoin(uwrFlightPoints_FL, minElevViewshed_FL, points_aglViewshed, "JOIN_ONE_TO_ONE", "KEEP_ALL")
                
                # getting the points that are terrain masked
                points_aglViewshedNumberSet = set()
                with arcpy.da.SearchCursor(points_aglViewshed, ["OBJECTID", "AGL", "gridcode"]) as cursor:
                    for row in cursor:
                        if row[2] is not None and (row[1] < row[2]):
                            points_aglViewshedNumberSet.add(str(row[0]))
                del cursor

                if len(points_aglViewshedNumberSet) == 0:
                    finalSQL = None
                else:
                    terrainMaskedPoints = ','.join(points_aglViewshedNumberSet)
                    finalSQL = "OBJECTID NOT IN (" + terrainMaskedPoints + ")"
                arcpy.FeatureClassToFeatureClass_conversion(points_aglViewshed, tempGDBPath, uwr_notmasked, finalSQL)

                #put into a list of all the feature classes of points that are terrain masked
                uwr_notmasked_List.append(os.path.join(tempGDBPath, uwr_notmasked))
                
                print("Runtime to find points not in LOS for ", uwr, ":", datetime.datetime.now() - uwrPointsstarttime)

            arcpy.env.workspace = LOS_uwrFlightPointsGDB
            arcpy.env.overwriteOutput = True

            #create final feature class of all points that aren't terrain masked
            arcpy.Merge_management(uwr_notmasked_List, os.path.join(LOS_uwrFlightPointsGDB, LOS_uwrFlightPoints))
            print("merged all non terrain masked points together")

            #get count of points that are in direct viewshed
            arcpy.MakeFeatureLayer_management(LOS_uwrFlightPoints, LOS_uwrFlightPoints_FL)
            arcpy.SelectLayerByAttribute_management(LOS_uwrFlightPoints_FL, "NEW_SELECTION", "gridcode is NULL")
            
            result = arcpy.GetCount_management(LOS_uwrFlightPoints_FL)
            uwrFlightSelectedCount = int(result[0])
            viewshedPointCount[uwr] = uwrFlightSelectedCount

            #getting count found in direct viewshed into excel
            dfViewshed = pd.DataFrame.from_dict(viewshedPointCount, orient = 'index')
            dfViewshed.columns = ['points found in viewshed']
            ViewshedPointCountPath = os.path.join(generalFolder, ViewshedPointCount) + ".xlsx"
            dfViewshed.to_excel(ViewshedPointCountPath)

            #delete feature layers
            arcpy.Delete_management(LOS_uwrFlightPoints_FL)
            arcpy.Delete_management(uwrFlightPoints_FL)
            arcpy.Delete_management(viewshed_FL)
            arcpy.Delete_management(minElevViewshed_FL)

            #delete unwanted fields
            arcpy.DeleteField_management(LOS_uwrFlightPoints, ["Join_Count_1", "TARGET_FID_1", "gridcode", "TUWR_TAG_1", "UNIT_NO_1", "uwr_unique_id"])

            if arcpy.CheckExtension("3D") == "Available":
                arcpy.CheckInExtension("3D")
            if arcpy.CheckExtension("Spatial") == "Available":
                arcpy.CheckInExtension("Spatial")

            print("Created a new feature class of flight points that have been terrain masked using line of sight analysis:", datetime.datetime.now() - scriptstarttime)

        except LicenseError:
            print("required licenses not available")