# _______________________________________
# 1 Load libraries
# _______________________________________
library(rgdal) # 4 read shapefiles
library(raster) # 4 read raster
library(rgeos)# 4 calculate distance from point to expert polygon
library(RMySQL) # 4 connect Catalogue of Life
library(mongolite) # 4 connect MongoDB
library(httr) # 4 test the URL connection
library(lubridate) # 4 dates parsing

# _______________________________________
# 2. Load script parameters
# _______________________________________
# Those objects are used by updateMongoDB directly from global
# enviroment and aren't given as a function parameters

# Load 'iucnShapes' table with the species name with available expert maps
iucnShapes <- read.csv('D:/dbDownload/nwDB/spp&TaxonomyCol2015.csv', as.is = TRUE)

# Load 'iucnShapesPath' with location of available expert maps
iucnShapesPath <- 'D:/dbDownload/nwDB/bySppClipCol_UTM'

# Load 'cit', 'end', 'inv', and 'cit' with species attributes
# of CITES appendix, endemic, invasive and threatened status
load('D:/dbDownload/CIT_IUCN_END_INV_tables.RData')

# Load 'layNE' raster stack with environmental values
# for 'biogeo' package outliers
load('D:/dbDownload/LayNE_noCorr.RData')

# Load 'projUTM' with projected reference usefull for calculate planar distances
prjUTM <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Load GIS layers for geographical verifications. Counties and DEM
# 'ALT', 'casco', 'mar', 'mpios1964', 'mpios1973', 'mpios1985',
# 'mpios1993', 'mpios2003', 'mpios2011', 'mpios2014' and 'paises' 
load('D:/dbDownload/verifGeoMongo.RData')

# Load tables with species names registered in Colombia from
# diferent sources compilations: 'TAX', 'TAXaccepNames', 'TAXNames', 'TAXsciNames'
load('D:/dbDownload/nwDB/TAXnwDb.RData')

# Load 'id' raster
id <<- raster('D:/dbDownload/nwDB/aoiId.tif')

# Load shapefile with region of interest
aoi <- readOGR('D:/dbDownload/nwDB', 'aoi', verbose = FALSE)

source('D:/dbDownload/nwDB/updateMongoDB.R')
source('C:/GitHub/workFlow/updateMongoDB.R')

# _______________________________________
## 3. DB connections
# _______________________________________
# These objects represents data bases connections and are used by updateMongoDB 
# directly from global enviroment and aren't given as a function parameters

conMSQ <<- dbConnect(dbDriver("MySQL"), user = "root", password = "root", dbname = "col2015ac", host = "localhost")

## Original MongoDB conections
# mongoConectRec <<- mongo(db = "produccion", collection = "records", url ="mongodb://192.168.11.81:27017", verbose = FALSE)
# mongoConectSpe <<- mongo(db = "produccion", collection = "species", url ="mongodb://192.168.11.81:27017", verbose = FALSE)

# Test conections
mongoConectRec <<- mongo(db = "records", collection = "recordsTest", url ="mongodb://192.168.11.81:27017", verbose = FALSE) ## modify this argument
mongoConectSpe <<- mongo(db = "records", collection = "speciesTest", url ="mongodb://192.168.11.81:27017", verbose = FALSE) ## modify this argument


mongoConectRec$drop() # Reset temporal test collections. Caution adviced
mongoConectSpe$drop() # Reset temporal test collections. Caution adviced

mongoConectSpe$count() # Check collection size
mongoConectRec$count() # Check collection size


# _______________________________________
## 4. Set and  create paths
# _______________________________________
# Create paths when duplicated, non taxonomic recognized and error 
# records are saved

root <- 'D:/dbDownload/nwDB/uploadsPath'          ## | modify this argument |
dirName <- 'testMongoV2'                          ## | modify this argument |
tempTaxValidation <- paste0(root, '/', dirName,'/manualTaxValidation') # Folder for non accepted species
dupDirRecords <- paste0(root, '/', dirName,'/dupDirRecords') # Folder for duplicated records
errorPath <- paste0(root, '/', dirName,'/errorPath') # Folder for errors
sapply(c(errorPath, dupDirRecords, tempTaxValidation), function(x) dir.create(x, recursive = TRUE))

# _______________________________________
## load table with records
# _______________________________________
# Load table with information. Final object must be assigned as 'upData'
tstdat <- read.csv('D:/dbDownload/nwDB/uploadsPath/anfibios_wAgudelo/Atelopus_muisca.csv')  ## modify this argument
upData <- tstdat

# _______________________________________
## 6. Upload records
# _______________________________________
# Run a loop for each row

i <- 1 # run once before start loop
ij <- 1
for(i in ij:nrow(upData)){ # Change 4 vars
  cat(dirName, 'record ', i, '-', nrow(upData), '|', round(i/nrow(upData)*100, 2), '%\n') # 
  
  upLdError <- tryCatch(updateMongoDB(record = upData[i, ], # Record
                                      aoi = aoi, # Region of interest
                                      .PrivateData = 0, # Private data flag
                                      .update = TRUE, # Really update mongoDB? 
                                      onlySp = FALSE, # Run only 'species' collection flags
                                      .tempTaxValidation = tempTaxValidation, # Temporal path
                                      .dupDirRecords = dupDirRecords, # Duplicated path
                                      .tempDirRecords = errorPath), # Error paths
                        error = function (e) {e})
  cat('\n')
  if (any(class(upLdError) == 'error')){
    cat('\n Error found \n')
    write.csv(cbind(x = i, error = paste0(as.character(upLdError))),
              paste0(errorPath, '/index', i, '.csv'), row.names = FALSE)
    write.csv(upData[i, ], paste0(errorPath, '/record', i, '.csv'), row.names = FALSE) #
    unErr <- unlist(as.character(upLdError))
    msqErr <- grep('MySQL', unErr)
    if (any(msqErr)){
      conMSQ <<- dbConnect(dbDriver("MySQL"), user = "root", password = "root", dbname = "col2015ac", host = "localhost")
    }
  }
  ij <- i
}

# _______________________________________
# 7. Update outlier flags 
# _______________________________________
uniqueSpp <- unique(upData$species) # Assuming upData have same names as uplodaded
for(s in 1:length(uniqueSpp)){
  updateFlags(.acceptedNameUsage = uniqueSpp[s], 
              .flags = c('altitudinalOutlier', 'consistentAltitude', 'diferenceInAltitude', 'environmentalOutlier'))
}


# _______________________________________
# 8. Check errors
# _______________________________________
checkErrors(c(errorPath, dupDirRecords, errorPath))

#Compile records if exist errrors
newDataFrameErrors <- compileErrors(tempDirRecordsPath = errorPath, .pattern = 'record')
write.csv(newDataFrameErrors, 'myNewPath/mynewDataset.csv', na = '', row.names = FALSE) ## modify this argument


##
## END


# __________________________________________
## Check upload succes
# __________________________________________

## Extract records from current v2 mongo database
mongoProdRec <<- mongo(db = "produccion", collection = "records", url ="mongodb://192.168.11.81:27017", verbose = FALSE)
mongoProdSpe <<- mongo(db = "produccion", collection = "species", url ="mongodb://192.168.11.81:27017", verbose = FALSE)

mongoProdSpe <- mongoProdSpe$find('{"species":"Atelopus muisca"}')
mongoProdRec <- mongoProdRec$find('{"species":"Atelopus muisca"}')

# Extract records from recently updated database
mongoTestSpe <- mongoConectSpe$find('{"species":"Atelopus muisca"}')
mongoTestRec <- mongoConectRec$find('{"species":"Atelopus muisca"}')

# Compare tables
head(mongoProdRec, 1)
head(mongoTestRec, 1)

setdiff(colnames(mongoProdRec), colnames(mongoTestRec)) # Missing fields from script

head(mongoProdSpe)
head(mongoTestSpe)
setdiff(colnames(mongoProdSpe), colnames(mongoTestSpe)) # Missing fields from script