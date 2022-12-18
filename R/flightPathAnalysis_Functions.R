# File flightPathAnalysis_Functions.py
# def replaceNonAlphaNum(myText, newXter):
#   '''
#     Purpose:  to check for non-alpha numeric xters
#     replace them with user defined new character
#     '''
# # go thru each xter. Check to see if it's alphanumeric.
# # If not, then replace it with the new character (newXter)
# for x in range(0, len(myText)):
#   if not myText[x].isalnum():
#     myText = myText.replace(myText[x], newXter)
#     return myText
# --- Notes :
# Just a gsub, no need for separate function, already in base R
replaceNonAlphaNum <- function(myText, newXter) {
  gsub("[:alnum:]", newXter, myText)
}

# def convert_timedelta(duration):
#   #convert timedelta to seconds (type: float)
#   seconds = duration.total_seconds()
# seconds = seconds % 60
# return seconds
# assume only seconds between them the two points??
# --- Notes :
# Just a difftime, no need for separate function, already in base R
convert_timedelta <- function(time1, time2) {
  difftime(time1, time2, units = "secs") |> as.integer()
}

# def appendMergeFeatures(featuresList, finalPath):
#     """
#     Purpose:
#     When the final feature class does not exist, the list of features will be merged.
#     If the final feature class exists, the list of features will be appended.
#
#     Note: if the final feature class exists, it assumes the list of features follow
#     the same schema as the final feature class.
#
#     Input:
#     featuresList: List of feature classes
#     finalPath: Full path of final feature class
#     """
#     if not arcpy.Exists(finalPath): #if feature class not exist
#         print('need to make', finalPath)
#         path = os.path.normpath(finalPath)
#         pathSplit = path.split(os.sep)
#
#         for s in pathSplit:
#             if '.gdb' in s:
#                 indexGDB = pathSplit.index(s)
#                 gdbPathList = pathSplit[:indexGDB+1]
#                 gdbPath = os.path.join(*gdbPathList)
#                 if ':' in pathSplit[0]:
#                     gdbPath = gdbPath.replace(":", ":\\")
#                 else:
#                     gdbPath = r"\\" + gdbPath
#                 break
#
#         #make gdb if not exist
#         if not arcpy.Exists(gdbPath): #if gdb of finalPath not exist
#             gdbFolderList = pathSplit[:indexGDB]
#             gdbFolder = r"\\" + os.path.join(*gdbFolderList)
#             arcpy.CreateFileGDB_management(gdbFolder, pathSplit[indexGDB])
#             print("created", pathSplit[indexGDB], "in", gdbFolder)
#             print('made gdb', gdbPath)
#
#         #make dataset if not exist
#         datasetPathList = pathSplit[:indexGDB + 2]
#         datasetPath = r"\\" + os.path.join(*datasetPathList)
#         if '.gdb' not in pathSplit[-2]:
#             arcpy.CreateFeatureDataset_management (gdbPath, r"\\" + pathSplit[indexGDB + 1])
#             print('made', datasetPath)
#
#         #merge all features to final layer
#         arcpy.Merge_management(featuresList, finalPath)
#         print('made final layer', finalPath)
#
#     else:
#         #append all new features into old layer
#         arcpy.Append_management(featuresList, finalPath)
#         print('appended all features to the final layer', finalPath)
# --- Notes :
# ordinary merge on sf object since it's already a data.frame, no need for it to be a function either

# def rawBuffer(origFCLoc, origFCName, bufferDistanceInput, bufferNumber, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field):
#     """
#     (string, string, string, int, string) -> string, string
#     origFCLoc: Folder of feature class to be buffered
#     origFCName: name of feature class or shapefile to be buffered
#     bufferDistanceInput: buffer value and unit. example: "1500 Meters"
#     bufferNumber: buffer value. Use for naming output feature class. Assume units in meters. example: 1500
#     unit_no_Field: field of unit number
#     unit_no_id_Field: field of unit number id
#     uwr_unique_Field: field for unique uwr id made from combining unit number and unit number id
#
#     Purpose:
#     Buffers given input feature class or dissolved feature class.
#     Returns names of output GDB and buffer fc name.
#
#     Output: buffered feature class
#
#     """
#     if origFCName.find(".shp") >= 0:
#         origName = replaceNonAlphaNum(origFCName[:origFCName.find(".shp")], "_") ###deleted +1 in  origFCName.find(".shp")+1
#     else:
#         origName = replaceNonAlphaNum(origFCName, "_")
#
#     rawBuffer = "rawBuffer_" + origName + "_" + str(bufferNumber)
#
#     arcpy.Buffer_analysis (os.path.join(origFCLoc, origFCName), os.path.join(outputGDB, rawBuffer), bufferDistanceInput)
#     print("buffered", origName, "by", bufferDistanceInput)
#     arcpy.DeleteField_management(os.path.join(outputGDB, rawBuffer), ["ORIG_FID"])
#
#     #Add field that combines unit_no and unit_no_id
#     arcpy.AddField_management(os.path.join(outputGDB, rawBuffer), uwr_unique_Field, "TEXT")
#     with arcpy.da.UpdateCursor(os.path.join(outputGDB, rawBuffer), [uwr_unique_Field, unit_no_Field, unit_no_id_Field]) as cursor:
#         for row in cursor:
#             row[0] = str(row[1]) + "__" + str(row[2])
#             cursor.updateRow(row)
#     del cursor
#
#     return outputGDB, rawBuffer
# --- Notes :
# Also no need for a function here, simple
sf::st_buffer(x = , dist = units::as_units(bufferDistanceInput), nQuadSegs = 2)
# lower nQuadSegs to minimal requirement for faster processing, do you need a perfect circle^
# is an octogone ok?
# with a
outputGDB$uwr_unique_Field <- sprintf("%s__%s", outputGDB$unit_no_Field, outputGDB$unit_no_id_Field)
# or similar

# def findBufferRange(ToEraseLoc, ToEraseName, UsetoErasePath, uniqueUWR_IDFields, outputGDB):
#     """
#     (string, string, string, string, string) -> None
#     ToEraseLoc: Folder of feature class to be erased
#     ToEraseName: Name of feature class to be erased
#     UsetoErasePath: Full path location of feature class used to erase the incoming feature class
#     uniqueUWR_IDFields: List of unique ID fields of the incoming feature class used to identify for erasing. Has to be a list!!
#     outputGDB: Gdb for the output
#
#     Given two feature classes with the same unique field, erase area in each feature in the first
#     feature class according to the area of the matching id in the other feature class.
#
#     Returns name of full path to feature class of results of erase analysis
#     Output: feature class of erase analysis
#
#     Note: Time used to erase ~3 seconds/record, ~10 min to merge a 1500 record uwr
#
#     """
#
#     print("Erasing starting...Start time: ", datetime.datetime.now())
#     starttime = datetime.datetime.now()
#
#     with tempfile.TemporaryDirectory() as temp_location:
#         #create temp gdb to store each fc output from erasing with each feature
#         tempgdbName = str(ToEraseName) + ".gdb"
#         tempgdbPath = temp_location + "\\" + tempgdbName
#         arcpy.CreateFileGDB_management (temp_location, tempgdbName)
#
#         arcpy.env.workspace = tempgdbPath
#         arcpy.env.overwriteOutput = True
#
#         arcpy.MakeFeatureLayer_management(UsetoErasePath, "UsetoEraseFL")
#         arcpy.MakeFeatureLayer_management(os.path.join(ToEraseLoc, ToEraseName), "ToEraseFL")
#
#         #count of features
#         out_count = 0
#
#         #list of erased fc to merge together
#         bufferedFeatures = []
#
#         #select unique id in each fc and erase area of second fc from first fc. Creates a fc output
#
#         with arcpy.da.SearchCursor("UsetoEraseFL", uniqueUWR_IDFields) as cursor:
#             for row in cursor:
#                 #for field in uniqueUWR_IDFields or i in index
#
#                 if type(row[0]) == int: #if id field is integer
#                     query = '(\"' + uniqueUWR_IDFields[0] + '\" = ' + str(row[0]) + ')'
#                 else:
#                     query = '(\"' + uniqueUWR_IDFields[0] + '\" = \'' + str(row[0]) + "')"
#
#                 countField = len(uniqueUWR_IDFields)
#                 if countField > 1:
#                     for i in range(1, countField):
#                         if type(row[i]) == int: #if id field is integer
#                             query += 'AND (\"' + uniqueUWR_IDFields[i] + '\" = ' + str(row[i]) + ')'
#                         else:
#                             query += 'AND (\"' + uniqueUWR_IDFields[i] + '\" = \'' + str(row[i]) + "')"
#
#                 out_count += 1
#                 out_features = arcpy.env.workspace + "\\outfeature" + str(out_count)
#                 arcpy.SelectLayerByAttribute_management("UsetoEraseFL", "NEW_SELECTION", query)
#                 arcpy.SelectLayerByAttribute_management("ToEraseFL", "NEW_SELECTION", query)
#                 arcpy.Erase_analysis("ToEraseFL", "UsetoEraseFL", out_features)
#                 bufferedFeatures.append(out_features)
#                 if (out_count % 250) == 0:
#                     print(out_count, "features done")
#         del cursor
#
#         arcpy.Delete_management("UsetoEraseFL")
#         arcpy.Delete_management("ToEraseFL")
#
#         print("Runtime to erase: ", datetime.datetime.now() - starttime, ". Merging them now")
#
#         currenttime = datetime.datetime.now()
#
#         arcpy.env.workspace = outputGDB
#         arcpy.env.overwriteOutput = True
#
#         #merge all outputs
#         outputPath = ToEraseLoc + "\\" + ToEraseName + "Only"
#         arcpy.Merge_management(bufferedFeatures, outputPath)
#
#         print("Runtime to merge: ", datetime.datetime.now() - currenttime)
#
#         arcpy.ClearWorkspaceCache_management() #bug in arcpro 2.6.1. Creating gdb will create lock. This clears it
#         print("Total runtime to find buffer range for", ToEraseName, ":", datetime.datetime.now()-starttime)
#     return outputPath
# --- Notes :
# Unless I'm mistaken, this is a simple
sf::st_difference()
# By matching unique ID field which can be found with a match/index call
# Depending on how fast it needs to run, we could jump directly to the C Call
# sf:::CPL_geos_op2("difference", ...

# ##just use this function to create uwr buffer
# def createUWRBuffer(origUWRGDB, origUWRName, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field, finalFC, bufferDistList):
#     #
#     # Create 'donut' buffers of uwr polygons with buffer numbers in bufferDistList from the original uwr feature class
#     # If the final buffer feature class exists, only uwr that's in the original uwr layer but not in the final layer
#     # will be buffered and added to the final feature class. If the final feature class does not exist, a new one will be made.
#     #
#     # eg. bufferDistList = [500,1000,1500]
#     # Outputs:
#     # 500m buffer polygon = the 500m donut shape outside of the uwr polygon. (0-500m buffer area)
#     # 1000m buffer polygon = the 500m donut shape outside of the 500m buffer polygon. (500-1000m buffer area)
#     # 1500m buffer polygon = the 500m donut shape outside of the 1000m buffer polygon. (1000-1500m buffer area)
#
#     arcpy.env.workspace = outputGDB
#     arcpy.env.overwriteOutput = True
#
#     scriptstarttime = datetime.datetime.now()
#
#     origUWRPath = os.path.join(origUWRGDB, origUWRName)
#
#     #get list of relevant UWR
#     uwrSet = set()
#     with arcpy.da.SearchCursor(origUWRPath, [unit_no_Field, unit_no_id_Field] ) as cursor:
#         for row in cursor:
#             uwrSet.add(str(row[0]) + "__" + str(row[1]))
#     del cursor
#
#     print(uwrSet)
#
#     #get list of uwr that have buffers created
#     CreatedUWRSet = set()
#     if arcpy.Exists(finalFC): #if final buffer fc exists
#         finalFCExist = 'Yes'
#         with arcpy.da.SearchCursor(finalFC, [uwr_unique_Field]) as cursor:
#             for row in cursor:
#                 CreatedUWRSet.add(row[0])
#         del cursor
#         print("uwr already buffered:", CreatedUWRSet)
#
#         #get list of uwr that do not have buffers created
#         UWRRequireSet = uwrSet - CreatedUWRSet
#         print("need to make buffers for uwr:", UWRRequireSet)
#     else:
#         finalFCExist = 'No'
#         UWRRequireSet = uwrSet
#
#     #create viewsheds for uwr that don't have any. Either makes a new final viewshed layer or appends to the old viewshed
#     if len(UWRRequireSet) > 0:
#         if finalFCExist == 'Yes':
#
#             # make new field in copy of orig UWR fc for unique uwr id. DIFFERENT FROM uniqueuwrid. make it so that there's no way this field existed before
#             arcpy.FeatureClassToFeatureClass_conversion(origUWRPath, outputGDB, 'tempFC')
#
#             tempOrigFCCopy = os.path.join(outputGDB, 'tempFC')
#
#             fieldnames = [field.name for field in arcpy.ListFields(tempOrigFCCopy)]
#             tempUniqueUWRField = "tempUniqueUWRField"
#             if tempUniqueUWRField not in fieldnames:
#                 arcpy.AddField_management(tempOrigFCCopy, tempUniqueUWRField, "TEXT")
#             # populate unique uwr id
#             with arcpy.da.UpdateCursor(tempOrigFCCopy, [unit_no_Field, unit_no_id_Field, tempUniqueUWRField] ) as cursor:
#                 for row in cursor:
#                     row[2] = str(row[0]) + "__" + str(row[1])
#                     cursor.updateRow(row)
#             del(cursor)
#
#             # make query with UWRRequireSet
#             uwrList_string = "','".join(UWRRequireSet)
#             query = tempUniqueUWRField + " in ('" + uwrList_string + "')"
#             print(query)
#
#             UnbufferedFL = "UnbufferedUWR"
#             arcpy.MakeFeatureLayer_management(tempOrigFCCopy, UnbufferedFL, query)
#
#             RequireUWRLayer = UnbufferedFL
#         else:
#             RequireUWRLayer = origUWRPath
#
#
#         uniqueIDFields = [unit_no_Field, unit_no_id_Field]
#
#         #Dissolves input feature class by dissolveFields list if list is given. This is to avoid errors for multi part uwr
#         # that have been split into separate features in the original uwr feature class.
#         dissolvedOrigLoc =  r"memory"
#         dissolvedOrigName = "dissolve"
#         dissolvedOrigPath = os.path.join(dissolvedOrigLoc, dissolvedOrigName)
#         arcpy.Dissolve_management(RequireUWRLayer, dissolvedOrigPath, uniqueIDFields)
#         arcpy.DeleteField_management(dissolvedOrigPath, ["ORIG_FID"])
#
#         #start list of intermediate features to be deleted
#         UWROnly = "BufferUWROnly"
#         delFC = [os.path.join(outputGDB, UWROnly)]
#
#         #Create raw buffers
#         #sort buffer distances. does not include 0 = uwr
#         bufferDistList.sort()
#         rawBufferDict = {}
#         for bufferDist in bufferDistList:
#             rawBufferLoc, rawBufferName = rawBuffer (dissolvedOrigLoc, dissolvedOrigName, str(bufferDist) + " Meters", bufferDist, outputGDB, unit_no_Field, unit_no_id_Field, uwr_unique_Field)
#             rawBufferDict[bufferDist] = [rawBufferLoc, rawBufferName]
#             delFC.append(os.path.join(rawBufferLoc, rawBufferName))
#
#         #Get donut shaped buffers to only get
#         #list of buffered donut polygons that will be merged together to get the final fc
#
#         requireMergeBufferList = [os.path.join(outputGDB, UWROnly)]
#
#         ##add to requireMergeBufferList
#
#         #list of keys in rawBufferDict ie. buffer distance in ascending order
#         sortBuffDistList = list(sorted(rawBufferDict))
#
#         #goes through each buffer distance to get the 'donut' shapes of only the area for each buffer distance
#         for bufferDist in sortBuffDistList:
#             if sortBuffDistList.index(bufferDist) == 0: #smallest buffer that is not the orig uwr
#                 onlyBufferDist = findBufferRange(rawBufferDict[bufferDist][0], rawBufferDict[bufferDist][1], os.path.join(dissolvedOrigLoc, dissolvedOrigName), uniqueIDFields, outputGDB)
#             else:
#                 prevIndex = sortBuffDistList.index(bufferDist) - 1
#                 prevBufferDist = sortBuffDistList[prevIndex]
#                 onlyBufferDist = findBufferRange(rawBufferDict[bufferDist][0], rawBufferDict[bufferDist][1], os.path.join(rawBufferDict[prevBufferDist][0], rawBufferDict[prevBufferDist][1]), uniqueIDFields, outputGDB)
#
#             requireMergeBufferList.append(onlyBufferDist)
#             delFC.append(onlyBufferDist)
#
#         #dissolve original uwr by the dissolveFields
#         origUWRPathMemory = r"memory\dissolveuwrOnly" + UWROnly
#         arcpy.Dissolve_management(RequireUWRLayer, origUWRPathMemory, uniqueIDFields)
#         arcpy.DeleteField_management(origUWRPathMemory, ["ORIG_FID"])
#
#         #Create copy of original uwr and add BUFF_DIST field and add unique uwr id field that combines unit_no and unit_no_id
#         arcpy.FeatureClassToFeatureClass_conversion (origUWRPathMemory, outputGDB, UWROnly)
#         arcpy.AddFields_management(os.path.join(outputGDB, UWROnly), [["BUFF_DIST", "DOUBLE"], [uwr_unique_Field, "TEXT"]])
#         with arcpy.da.UpdateCursor(os.path.join(outputGDB, UWROnly), ["BUFF_DIST", uwr_unique_Field, unit_no_Field, unit_no_id_Field]) as cursor:
#             for row in cursor:
#                 row[0] = 0
#                 row[1] = str(row[2]) + "__" + str(row[3])
#                 cursor.updateRow(row)
#         del cursor
#         arcpy.Delete_management(origUWRPathMemory)
#
#         #Append uwr into the final feature class or create a new feature class
#         appendMergeFeatures(requireMergeBufferList, finalFC)
#
#         if finalFCExist == 'Yes':
#             arcpy.Delete_management(UnbufferedFL)
#             arcpy.Delete_management(tempOrigFCCopy)
#             arcpy.DeleteField_management(origUWRPath, tempUniqueUWRField)
#
#         print("Runtime to make final uwr buffer:", finalFC, ":", datetime.datetime.now() - scriptstarttime)
#
#         #clean up files
#         for fc in delFC:
#             arcpy.Delete_management(fc)
#
#     else:
#         print('no need to make new uwr buffers')
# ##functions for terrain masking
# --- Notes :
# Combine st_buffer and st_difference

