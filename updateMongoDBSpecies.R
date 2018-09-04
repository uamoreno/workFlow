#library(rgdal); library(raster)
#library(RMySQL);library(rgeos);
#library(mongolite);library(httr);library(lubridate)

# iucnShapes <- read.csv('D:/dbDownload/nwDB/spp&TaxonomyCol2015.csv', as.is = TRUE)
# iucnShapesPath <- 'D:/dbDownload/nwDB/bySppClipCol'
# load('D:/dbDownload/CIT_IUCN_END_INV_tables.RData')
# load('D:/dbDownload/LayNE_noCorr.RData')
# demAoi <- raster('D:/dbDownload/nwDB/demAoi.tif')
# prjUTM <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# load('D:/dbDownload/verifGeoMongo.RData')
# load('D:/dbDownload/nwDB/TAXnwDb.RData')
#load('D:/dbDownload/nwDB/mpios.RData')
# id <<- raster('D:/dbDownload/nwDB/aoiId.tif')
# aoi <- readOGR('D:/dbDownload/nwDB', 'aoi', verbose = FALSE)
# mongoConectRec <<- mongo(db = "produccion", collection = "records", url ="mongodb://biomodelos:#F#W4ff4uV-7Kjmd@192.168.11.81:27017/prueba", verbose = FALSE) ## modify this argument
# mongoConectSpe <<- mongo(db = "prueba", collection = "pruebaMultiSpSpe", url ="mongodb://biomodelos:#F#W4ff4uV-7Kjmd@192.168.11.81:27017/prueba", verbose = FALSE) ## modify this argument
# mongoConectRec$count()
# mongoConectSpe$count()

# testDat <- read.csv('D:/dbDownload/nwDB/uploadsPath/fauna_exotica/SubidaCamposFormato Documentacion Exoticas _RRBB Fauna vJul2017_editIG.csv', as.is = TRUE)
# summary(testDat[,c('lat', 'lon')])
# testDat$lon[testDat$lon > 0] <- testDat$lon[testDat$lon > 0] * -1
# plot(testDat$lon, testDat$lat)
# uniqueSp <- base:::split(testDat, testDat$species)
# length(unique(testDat$species))
# which(unique(testDat$species) %in% iucnShapes$iucnShapes)
# length(uniqueSp)
# record <- uniqueSp[[22]]

<<<<<<< HEAD
library(rgdal)
library(raster)
library(RMySQL)
library(rgeos)
library(mongolite)
library(httr)
library(lubridate)

## Functions -----
#MSQ <- list(user = "root", password = "root", dbname = "col2015ac", host = "localhost")
loadEnvironment <- function(.iucnShapes = 'D:/dbDownload/nwDB/spp&TaxonomyCol2015.csv',
                            .iucnShapesPath = 'D:/dbDownload/nwDB/bySppClipCol',
                            .citEndTables = 'D:/dbDownload/CIT_IUCN_END_INV_tables.RData',
                            .noCor = 'D:/dbDownload/LayNE_noCorr.RData',
                            .demAoi = 'D:/dbDownload/nwDB/demAoi.tif',
                            .mpios = 'D:/dbDownload/nwDB/mpios.RData',
                            .aoiPath = 'D:/dbDownload/nwDB',
                            .aoiLayName = 'aoi',
                            .tax = 'D:/dbDownload/nwDB/TAXnwDb.RData',
                            .mongoUrl = "mongodb://biomodelos:#F#W4ff4uV-7Kjmd@192.168.11.105:27018/produccion",
                            .mongoDb = "produccion",
                            .mongoRec = "records", .mongoSpe = "species",
                            .sqlUser = "root", .sqlpwd = "root", .sqlDb = "col2015ac",
                            useProductionMDBConnection = FALSE){
  
  
  # Load 'iucnShapes' table with the species name with available expert maps
  iucnShapes <<- read.csv(.iucnShapes, as.is = TRUE)
  
  # Load 'iucnShapesPath' with location of available expert maps
  iucnShapesPath <<- .iucnShapesPath
  
  # Load 'cit', 'end', 'inv', and 'cit' with species attributes
  # of CITES appendix, endemic, invasive and threatened status
  load(.citEndTables, envir = .GlobalEnv)
  cit <<- cit; inv <<- inv; iuc <<- iuc #end <<- end;
  
  # Load 'layNE' raster stack with environmental values
  # for 'biogeo' package outliers
  load(.noCor)
  layNE <<- layNE
  demAoi <<- raster(.demAoi)
  
  # Load 'projUTM' with projected reference usefull for calculate planar distances
  prjUTM <<- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # Load GIS layers for geographical verifications. Counties and DEM
  # 'ALT', 'id', 'casco', 'mar', 'mpios1964', 'mpios1973', 'mpios1985',
  # 'mpios1993', 'mpios2003', 'mpios2011', 'mpios2014' and 'paises' 
  load(.mpios, envir = .GlobalEnv)
  #ALT <<- ALT; casco <<- casco; id <<-id; mar <<- ma; mpios <<- mpios; proj <<- proj
  
  # Load tables with species names registered in Colombia from
  # diferent sources compilations: 'TAX', 'TAXaccepNames', 'TAXNames', 'TAXsciNames'
  load(.tax, envir = .GlobalEnv)
  
  # Load shapefile with region of interest
  aoi <<- readOGR(.aoiPath, .aoiLayName, verbose = FALSE)
  
  ## Generate the database conection ----
  conMSQ <<- dbConnect(dbDriver("MySQL"), user = .sqlUser, password = .sqlpwd, dbname = .sqlDb, host = "localhost")
  

  ## Generate mongo conections
  if(useProductionMDBConnection){
    mongoConectSpe <<- mongo(db = .mongoDb, collection = .mongoSpe, url = .mongoUrl, verbose = FALSE)
    mongoConectRec <<- mongo(db = .mongoDb, collection = .mongoRec, url = .mongoUrl, verbose = FALSE)
  }
}

=======
## Functions -----

#MSQ <- list(user = "root", password = "root", dbname = "col2015ac", host = "localhost")
>>>>>>> origin/master
estimateUploadTime <- function(speciesField){
  tab.sp <- table(speciesField)
  #4.646x(^-0.488)
}

<<<<<<< HEAD
uploadTable <- function(upData, DirName, aoi, .PrivateData = 0, .update = TRUE, OnlySp = FALSE,
                        tempTaxValidation, dupDirRecords, errorPath, .Use = TRUE, .RecordsVisualizationPrivileges = 0,
                        .sqlUser = "root", .sqlpwd = "root", .sqlDb = "col2015ac", UploadUser = NA, delMissCoords = FALSE, delOutCoords = FALSE){
  # " - DEBUG 1";  DirName=dirName; .PrivateData = 0; .update = TRUE; OnlySp = FALSE; UploadUser='ig'; delMissCoords=TRUE;delOutCoords=TRUE
  # " - DEBUG 2";  record = upData[[1]]; aoi = aoi; .PrivateData = .PrivateData; .update = .update; onlySp = OnlySp; .tempTaxValidation = tempTaxValidation;
  # " - DEBUG 2";  .RecordsVisualizationPrivileges = 0;
  # " - DEBUG 2";  .dupDirRecords = dupDirRecords; .tempDirRecords = errorPath; .Use = TRUE; .uploadUser = UploadUser; .delMissCoords = delMissCoords; .delOutCoords = delOutCoords
  # " - DEBUG 3";  rec = record; .privateData = .PrivateData; .privateDataset = 0; fr.onlySp = onlySp; fr.use =.Use; .contributedRecord=contRecord = 'NULL'; fr.delMissCoords = .delMissCoords; fr.delOutCoords = .delOutCoords; .recordsVisualizationPrivileges = 0;  .tmpTax = 1; .tmpGeo = 1;


  t0 <- Sys.time()
  
  # if(!OnlySp){
  #   upNames <- names(upData)
  #   .colNames <- c('ID', 'speciesName', 'origRecords', 'uploadedRecords', 'NewInSppTable', 'validName', 'seconds')
  #   logTable <- data.frame(matrix(ncol = length(.colNames), nrow = length(upData)))
  #   colnames(logTable) <- .colNames
  # }

  # i <- 1
=======
uploadTable <- function(upData, DirName, aoi, .PrivateData = 0, .update = TRUE, onlySp = FALSE,
                        tempTaxValidation, dupDirRecords, errorPath, .Use = TRUE, MSQ){
  t0 <- Sys.time()
  
  if(!.OnlySp){
    upNames <- names(upData)
    .colNames <- c('ID', 'speciesName', 'origRecords', 'uploadedRecords', 'NewInSppTable', 'validName', 'seconds')
    logTable <- data.frame(matrix(ncol = length(.colNames), nrow = length(upData)))
    colnames(logTable) <- .colNames
  }

>>>>>>> origin/master
  for(i in 1:length(upData)){
    cat('\n', DirName, 'record ', i, '-', nrow(upData), '|', round(i/length(upData)*100, 2), '%\n') # 
    t00 <- Sys.time()
    upLdError <- tryCatch(updateMongoDB(record = upData[[i]], # Record
                                        aoi = aoi, # Region of interest
                                        .PrivateData = .PrivateData, # Private data flag
<<<<<<< HEAD
                                        .recordsVisualizationPrivileges = .RecordsVisualizationPrivileges,
                                        .update = .update, # Really update mongoDB? 
                                        onlySp = OnlySp, # Run only 'species' collection flags
                                        .tempTaxValidation = tempTaxValidation, # Temporal path
                                        .dupDirRecords = dupDirRecords, # Duplicated path
                                        .tempDirRecords = errorPath, # Error paths
                                        .Use = .Use,
                                        .uploadUser = UploadUser,
                                        .delMissCoords = delMissCoords,
                                        .delOutCoords = delOutCoords), #Use record?
=======
                                        .update = .update, # Really update mongoDB? 
                                        onlySp = onlySp, # Run only 'species' collection flags
                                        .tempTaxValidation = tempTaxValidation, # Temporal path
                                        .dupDirRecords = dupDirRecords, # Duplicated path
                                        .tempDirRecords = errorPath, # Error paths
                                        .Use = .Use), #Use record?
>>>>>>> origin/master
                          error = function (e) {e})
    if (any(class(upLdError) == 'error')){
      cat(paste0('\n Error found \n', upLdError, '\n'))
      write.csv(cbind(x = i, error = paste0(as.character(upLdError))),
                paste0(errorPath, '/index', i, '.csv'), row.names = FALSE)
      write.csv(upData[[i]], paste0(errorPath, '/recordSet_', i, '.csv'), row.names = FALSE) #
      unErr <- unlist(as.character(upLdError))
      msqErr <- grep('MySQL', unErr)
      if (any(msqErr)){
<<<<<<< HEAD
        conMSQ <<- dbConnect(dbDriver("MySQL"), user = .sqlUser, password = .sqlpwd, dbname = .sqlDb, host = "localhost")
        
      }
    }
    # if(!OnlySp){
    #   nRec <- nrow(upData[[i]])
    #   delayTime <- Sys.time() - t00
    #   cat(nRec, delayTime, '\n')
    #   logTable[i, c(i, upNames[i], nRec, upLdError$nRecUp, upLdError$newInsppTable, upLdError$isValidName, delayTime)] #c('ID', 'speciesName', 'origRecords', 'uploadedRecords', 'NewInSppTable', 'validName', 'seconds')
    # }
=======
        conMSQ <<- dbConnect(dbDriver("MySQL"), user = MSQ$user, password = MSQ$password, dbname = MSQ$dbname, host = MSQ$host)
        
      }
    }
    if(!.OnlySp){
      nRec <- nrow(upData[[i]])
      delayTime <- Sys.time() - t00
      cat(nRec, delayTime, '\n')
      logTable[i, c(i, upNames[i], nRec, upLdError$nRecUp, upLdError$newInsppTable, upLdError$isValidName, delayTime)] #c('ID', 'speciesName', 'origRecords', 'uploadedRecords', 'NewInSppTable', 'validName', 'seconds')
    }
>>>>>>> origin/master
  }
  t1 <- Sys.time()
}

<<<<<<< HEAD
updateMongoDB <- function(record, aoi, .PrivateData = 0, onlySp = FALSE, .Use = TRUE, .recordsVisualizationPrivileges = 0,
                          .tempTaxValidation, .tempDirRecords, .dupDirRecords, .update = TRUE, contRecord = 'NULL', 
                          .uploadUser = NA, .delMissCoords = FALSE, .delOutCoords = FALSE){
  
  # .PrivateData = 0; .Use = TRUE; .scriptMaxchar = .2
  # .update = F
  # contRecord = 'NULL'
  # OnlySp = onlySp = fr.onlySp = FALSE
  # .tempTaxValidation = tempTaxValidation; .dupDirRecords = dupDirRecords; .tempDirRecords = errorPath
  # .privateData = 0; .override = 0; .url = ''; .recordsVisualizationPrivileges = 0; .privateDataset = 0; .tmpTax = 1; .tmpGeo = 1; .contributedRecord = FALSE

  if (is.na(.uploadUser))  stop('provide upload user')
  
  ## Take the original record and structure it using some parameters
  formRecord0 <- formatRecord(rec = record, aoi, .privateData = .PrivateData, fr.onlySp = onlySp, fr.use = .Use, 
                             .contributedRecord = contRecord, fr.delMissCoords = .delMissCoords, fr.delOutCoords = .delOutCoords)
  
  onlySp <- formRecord0$fr.onlySp
  formRecord <- formRecord0[[1]]
  formRecord$recordsVisualizationPrivileges <- .recordsVisualizationPrivileges
  formRecord$uploadUser <- .uploadUser
  currentDateTime <- Sys.time()
  formRecord$uploadDate <- ISOdate(year = year(currentDateTime), month = month(currentDateTime), day = day(currentDateTime),
                                   hour = hour(currentDateTime), min = minute(currentDateTime), sec = second(currentDateTime),
                                   tz = 'GMT')
  
  ## Check if species is not accepted and save it and the record in a temporal folder
  isNotValidName <- is.na(formRecord$acceptedNameUsage[1]) | formRecord$acceptedNameUsage[1] == ''
  if (isNotValidName){
=======
updateMongoDB <- function(record, aoi, .PrivateData = 0, onlySp = FALSE, .Use = TRUE, 
                          .tempTaxValidation, .tempDirRecords, .dupDirRecords, .update = TRUE, contRecord = 'NULL'){
  
  # .PrivateData = 0;   .Use = TRUE; .scriptMaxchar = .2
  # .update = F
  # contRecord = 'NULL'
  # onlySp = fr.onlySp = FALSE
  # .tempTaxValidation = tempTaxValidation; .dupDirRecords = dupDirRecords; .tempDirRecords = errorPath
  # .privateData = 0; .override = 0; .url = ''; .visualizationPrivileges = 0; .privateDataset = 0; .tmpTax = 1; .tmpGeo = 1; .contributedRecord = FALSE

  
  ## Take the original record and structure it using some parameters
  formRecord <- formatRecord(rec = record, aoi, .privateData = .PrivateData, fr.onlySp = onlySp, fr.use = .Use, .contributedRecord = contRecord)
  
  ## Check if species is not accepted and save it and the record in a temporal folder
  isValidName <- is.na(formRecord$acceptedNameUsage[1]) | formRecord$acceptedNameUsage[1] == ''
  if (isValidName){
>>>>>>> origin/master
    currSpe2Validate <- list.files(path = .tempTaxValidation, pattern = '.csv')
    csvFiles <- gsub('.csv', '', currSpe2Validate)
    if(!formRecord$species[1] %in% csvFiles){
      taxRec <- formRecord[1, c('species', 'speciesOriginal', 'kingdom', 'phylum', 
                                'class', 'order', 'family', 'acceptedNameUsage')]
      write.csv(taxRec, paste0(.tempTaxValidation, '/', formRecord$species[1],'.csv'), row.names = FALSE, na = '')
    }
    write.csv(formRecord, paste0(.tempDirRecords, '/', formRecord$species[1],'-', basename(tempfile(formRecord$species[1])),'.csv'), row.names = FALSE, na = '')
    cat(formRecord$species[1], ' saved in temporal records and taxonimc folder ')
    
    ## If species exist then the record is uploaded into mongoDB
  } else {
    existsID <- specAlreadyInDb(rec = formRecord[1, ])

    ## Is the species new in mongoDB? If not then add it
    if (existsID$exist == FALSE){
<<<<<<< HEAD
      formRecord$taxID <- existsID$taxID[1]
      if (.update == TRUE){
        #formRecord$consecutivoID <- (mongoConectSpe$count() + 1)
        updateSpeciesTable(rec = formRecord[1, ])
=======
      formRecord$taxID <- existsID$taxID
      if (.update == TRUE){
        formRecord$consecutivoID <- (mongoConectSpe$count() + 1)
        updateSpeciesTable2(rec = formRecord[1, ])
>>>>>>> origin/master
        cat(formRecord$species[1], 'upload in species table')
      } else{
        cat(formRecord$species[1], 'upload in species table (FAKE - change to .update = TRUE)')
      }
    } else {
<<<<<<< HEAD
      formRecord$taxID <- existsID$taxID[1]
=======
      formRecord$taxID <- existsID$taxID
>>>>>>> origin/master
      cat(formRecord$species[1], 'already in species table')
    }
    if (is.data.frame(formRecord)){
      if(.update == TRUE & onlySp == FALSE){
        formRecord$consecutivoID <- (mongoConectRec$count() + 1):(mongoConectRec$count() + nrow(formRecord)) 
<<<<<<< HEAD
        updateRecordsTable(rec = formRecord[!formRecord$dbDuplicate, ])
=======
        updateRecordsTable2(rec = formRecord[!formRecord$dbDuplicate, ])
>>>>>>> origin/master
        cat(' - ', formRecord$species[1], 'upload in records (',  nrow(formRecord[!formRecord$dbDuplicate, ]),') table ')
        if (any(formRecord$dbDuplicate)) {
          write.csv(formRecord[formRecord$dbDuplicate, ], paste0(.dupDirRecords, '/', formRecord$species[1],'-', basename(tempfile(formRecord$species[1])),'.csv'), row.names = FALSE, na = '')
          cat(formRecord$species[1], ' saved in temporal records and taxonimc folder ')
        }
      }
    }
  }
  cat(' - Success! \n')
  #return(list(nRecUp = nrow(formRecord[!formRecord$dbDuplicate, ]),
  #            newInsppTable = !existsID$exist))
    
<<<<<<< HEAD
=======
  
>>>>>>> origin/master
}


## Insert record in species table
<<<<<<< HEAD
# updateSpeciesTable <- function(rec){
#   taxCols <- c('species', 'originalGenus', 'originalSpecificEpithet',
#                'taxonomicStatus', 'originalScientificNameID',
#                'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship',
#                'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet',
#                'nameAccordingTo', 'taxVerifSource', 'sppInCol', 'speciesInCountry', 'validName',
#                'bmClass', 'invasive', 'endemic', 'iucn', 'cites', 'taxID', 'consecutivoID')
#   
#   speciesTable <- rec[, taxCols]
#   speciesTable[1, is.na(speciesTable[1, ]) | speciesTable[1, ] == ''] <- 'NULL'
#   
#   mongoConectSpe$insert(speciesTable)
# }
# 
# ## Insert record in records table
# updateRecordsTable <- function(rec){
#   taxCols <- c('species', 'originalGenus', 'originalSpecificEpithet',
#                'taxonomicStatus', 'originalScientificNameID',
#                'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship',
#                'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet',
#                'nameAccordingTo', 'taxVerifSource', 'sppInCol', 'speciesInCountry', 'validName', 'taxID', 'consecutivoID')
#   
#   recordsTable <- rec[, c('species', 'acceptedNameUsage', 'taxID', 'consecutivoID', colnames(rec)[!(colnames(rec) %in% taxCols)])]
#   recordsTable[1, is.na(recordsTable[1, ]) | recordsTable[1, ] == ''] <- 'NULL'
#   colnames(recordsTable)[grep('collection', colnames(recordsTable))] <- 'colection'
#   #mongoConectRec <- mongo(db = "records", collection = "records", url = "mongodb://192.168.11.81:27017", verbose = FALSE)
#   mongoConectRec$insert(recordsTable)
# }

## Insert record in species table v2
updateSpeciesTable <- function(rec){
  # taxCols[taxCols %in% colnames(rec)]
  # taxCols[!taxCols %in% sort(colnames(rec))]
  speciesTable <- rec[, taxCols] # str(speciesTable[, sort(colnames(speciesTable))])
  #speciesTable[1, is.na(speciesTable[1, ]) | speciesTable[1, ] == ''] <- 'NULL'
  # mongoConectSpe$insert(speciesTable)
  # insertObject <- jsonlite::toJSON(as.list(speciesTable), auto_unbox = TRUE)
  # mongoConectSpe$insert(insertObject)
=======
updateSpeciesTable <- function(rec){
  taxCols <- c('species', 'originalGenus', 'originalSpecificEpithet',
               'taxonomicStatus', 'originalScientificNameID',
               'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship',
               'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet',
               'nameAccordingTo', 'TaxVerifSource', 'sppInCol', 'speciesInCountry', 'validName',
               'bmClass', 'invasive', 'endemic', 'iucn', 'cites', 'taxID', 'consecutivoID')
  
  speciesTable <- rec[, taxCols]
  speciesTable[1, is.na(speciesTable[1, ]) | speciesTable[1, ] == ''] <- 'NULL'
  
  #mongoConectSpe <- mongo(db = 'records', collection = 'species', url = 'mongodb://192.168.11.81:27017', verbose = FALSE)
  mongoConectSpe$insert(speciesTable)
}

## Insert record in records table
updateRecordsTable <- function(rec){
  taxCols <- c('species', 'originalGenus', 'originalSpecificEpithet',
               'taxonomicStatus', 'originalScientificNameID',
               'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship',
               'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet',
               'nameAccordingTo', 'TaxVerifSource', 'sppInCol', 'speciesInCountry', 'validName', 'taxID', 'consecutivoID')
  
  recordsTable <- rec[, c('species', 'acceptedNameUsage', 'taxID', 'consecutivoID', colnames(rec)[!(colnames(rec) %in% taxCols)])]
  recordsTable[1, is.na(recordsTable[1, ]) | recordsTable[1, ] == ''] <- 'NULL'
  colnames(recordsTable)[grep('collection', colnames(recordsTable))] <- 'colection'
  #mongoConectRec <- mongo(db = "records", collection = "records", url = "mongodb://192.168.11.81:27017", verbose = FALSE)
  mongoConectRec$insert(recordsTable)
}

## Insert record in species table v2
updateSpeciesTable2 <- function(rec){
  taxCols <- c("species" ,"taxonomicStatus", "acceptedNameUsage", "scientificNameAuthorship",
               "kingdom", "phylum", "class", "order", 
               "family", "genus", "specificEpithet", "nameAccordingTo",
               "TaxVerifSource", "sppInCol", "speciesInCountry", "validName", 
               "bmClass", "invasive", "endemic", "iucn",
               "cites", "taxID", "migratoryType")
  
  speciesTable <- rec[, taxCols]
  speciesTable[1, is.na(speciesTable[1, ]) | speciesTable[1, ] == ''] <- 'NULL'
>>>>>>> origin/master
  mongoConectSpe$insert(speciesTable)
}

## Insert record in records table v2
<<<<<<< HEAD
updateRecordsTable <- function(rec){
  recordsTable <- rec[,  setdiff(colnames(rec), excludeColsFromRecords)]
  # recordsTable[] <- lapply(recordsTable, function(x) {
  #   bad <- which(is.na(x) | x == '')
  #   if(any(bad)){
  #     x[bad] <- 'NULL'
  #   }
  #   return(x)
  # })
  #mongoConectRec$insert(recordsTable)
  
  colnames(recordsTable)[grep('collection', colnames(recordsTable))] <- 'colection'
  insertObject <- jsonlite::toJSON(as.list(recordsTable), auto_unbox = TRUE)
  mongoConectSpe$insert(insertObject)
}


## Chek if the species alredy exists in mongoDB
specAlreadyInDb <- function(rec){
=======
updateRecordsTable2 <- function(rec){
  excCols <- c("taxonomicStatus", "scientificNameAuthorship",
               "specificEpithet", "nameAccordingTo", "TaxVerifSource",
               "kingdom", "phylum", "class", "order", "family", "genus", 
               "sppInCol", "speciesInCountry", "validName", 
               "bmClass", "invasive", "endemic", "iucn", "cites", "migratoryType",
               "consecutivoID", "otherID", "withLat", "withLon", "recSizeBytes", "bigSizeRecord",
               "hasTaxDoubt", "privateDataset", "tmpTax", "tmpGeo")
  
  recordsTable <- rec[,  setdiff(colnames(rec), excCols)]
  recordsTable[] <- lapply(recordsTable, function(x) {
    bad <- which(is.na(x) | x == '')
    if(any(bad)){
      x[bad] <- 'NULL'
    }
    return(x)
  })
  
  colnames(recordsTable)[grep('collection', colnames(recordsTable))] <- 'colection'
  mongoConectRec$insert(recordsTable)
}

## Chek if the species alredy exists in mongoDB
specAlreadyInDb <- function(rec){
  dbTaxFields <- c('species', 'originalGenus', 'originalSpecificEpithet',
                   'taxonomicStatus', 'originalScientificNameID',
                   'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship',
                   'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet',
                   'nameAccordingTo', 'TaxVerifSource', 'sppInCol', 'speciesInCountry', 'validName')
  
>>>>>>> origin/master
  mongoQ <- paste0('{"$or":[{"species":"', rec$species[1], '"}, {"acceptedNameUsage":"', rec$species[1],
                   '"}, {"species":"', rec$acceptedNameUsage[1], '", "acceptedNameUsage":"', rec$acceptedNameUsage[1], '"}]}')
  
  
  mongoFind <- mongoConectSpe$find(mongoQ)
  
  if (nrow(mongoFind) == 0){
    rowCount <- mongoConectSpe$count()
    temp_taxID <- rowCount + 1
    idQ <- mongoConectSpe$find(paste0('{"taxID":', temp_taxID, '}'))
    if(nrow(idQ) >= 1) {
      temp_taxID <- temp_taxID + 1
      return(list(taxID = temp_taxID, exist = FALSE))
    } else {
      return(list(taxID = temp_taxID, exist = FALSE))
    }
    
  } else if (nrow(mongoFind) != 0){
    return(list(taxID = mongoConectSpe$find(mongoQ)$taxID,
                exist = TRUE))
  }
}

<<<<<<< HEAD

## Define columns

excludeColsFromRecords <<- c("taxonomicStatus", "scientificNameAuthorship",
             "specificEpithet", "nameAccordingTo", "taxVerifSource",
             "kingdom", "phylum", "class", "order", "family", "genus", 
             "sppInCol", "speciesInCountry", "validName", 
             "bmClass", "invasive", "endemic", "iucn", "iucnSpeciesID", "amenazaNacional", "cites", "migratoryType",
             "consecutivoID", "otherID", "withLat", "withLon", "recSizeBytes", "bigSizeRecord",
             "hasTaxDoubt", "privateDataset", "tmpTax", "tmpGeo",  'recordsVisualizationPrivileges',
             'uploadUser', 'uploadDate')

# Taxonomic and requierd Cols for script. NA is given if absent

reqCols <<- c("source", "occurrenceID", "species", "speciesOriginal", "continent", 
             "country", "stateProvince", "county", "verbatimLocality","lat",
             "lon","coordinateUncertaintyInMeters", "verbatimElevation","institutionCode", "collectionCode",
             "catalogNumber", "basisOfRecord", "recordedBy", "earliestDateCollected", # "latestDateCollected",
             "downloadDate", "resourceName", "resourceFolder","resourceIncorporationDate",
             "privateData", "otherID")


speciesTableCols <<- c("taxID","acceptedNameUsage","species",
                      "specificEpithet", "genus","family","order","class","phylum", "kingdom",
                      "endemic","invasive","validName","speciesInCountry","sppInCol","recordsVisualizationPrivileges",
                      "bmClass","uploadUser","uploadDate","amenazaNacional","iucn")

taxCols <<- c("species" ,"taxonomicStatus", "acceptedNameUsage", "scientificNameAuthorship",
              "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet",
              "nameAccordingTo", "taxVerifSource",
              "sppInCol", "speciesInCountry", "validName", 
              "bmClass", "invasive", "endemic", "iucn", "iucnSpeciesID", "amenazaNacional", "cites", "migratoryType",
              "taxID", 'recordsVisualizationPrivileges', 'uploadUser', 'uploadDate')

## Format the original record
formatRecord <- function(rec, aoi, .privateData = .privateData, .override = FALSE, .url = '', fr.onlySp = FALSE,
                         .recordsVisualizationPrivileges = 0, .privateDataset = 0, fr.use = TRUE,
                         .tmpTax = 1, .tmpGeo = 1, .contributedRecord = 'NULL', fr.delMissCoords = FALSE, fr.delOutCoords = FALSE){
  
  #.privateData = 0; .override = 0; .url = ''; .recordsVisualizationPrivileges = 0; .privateDataset = 0; .tmpTax = 1; .tmpGeo = 1; .contributedRecord = FALSE
  # fr.onlySp = FALSE
  # rec <- .recT <- origRec <- record
  
  ## Create new formated record
=======
## Define columns
# Taxonomic and requierd Cols for script. NA is given if is absent
taxCols <<- c('kingdom', 'phylum', 'class', 'order', 'family')

reqCols <<- c("source", "occurrenceID", "species", "speciesOriginal", "continent", 
             "country", "adm1", "adm2", "locality","lat",
             "lon","coordUncertaintyM", "alt","institution", "collection",
             "catalogNumber", "basisOfRecord", "collector", "earliestDateCollected",
             "latestDateCollected", "downloadDate", "resourceName", "resourceFolder","resourceIncorporationDate",
             "privateData", "otherID")

## Format the original record
formatRecord <- function(rec, aoi, .privateData = .privateData, .override = FALSE, .url = '', fr.onlySp = FALSE,
                         .visualizationPrivileges = 0, .privateDataset = 0, fr.use = TRUE,
                         .tmpTax = 1, .tmpGeo = 1, .contributedRecord = 'NULL'){
  
  #.privateData = 0; .override = 0; .url = ''; .visualizationPrivileges = 0; .privateDataset = 0; .tmpTax = 1; .tmpGeo = 1; .contributedRecord = FALSE
  # fr.onlySp = FALSE
  # rec <- .recT <- origRec <- record
  
    ## Create new formated record
>>>>>>> origin/master
  if (is.null(rec$speciesOriginal)) rec$speciesOriginal <- rec$species
  nwRec <- nwRecFun(.rec = rec, .reqCols = reqCols)
  
  if (fr.onlySp == FALSE ){
<<<<<<< HEAD
    nwRec[, c('latNum', 'lonNum')] <- sapply(nwRec[, c('lat', 'lon')], as.numeric)
    
    # 1 Incomplet coords ----
    badCoords <- is.na(nwRec$latNum) | is.na(nwRec$lonNum)
    
    if(any(badCoords)) {
      message(paste0(length(which(badCoords)), ' (of ', nrow(nwRec), ') incomplete coordinates!'))
    }
    
    if (fr.delMissCoords){
      if(all(badCoords)){
        nwRec <- nwRec[1, ]
        nwRec$dbDuplicate <- TRUE
        fr.onlySp <- TRUE
      } else{
        nwRec <- nwRec[!badCoords, ]
      }
    }
    
    nwRec$lon[is.na(nwRec$lon)] <- 0
    nwRec$lat[is.na(nwRec$lat)] <- 0
    
    # 2 Coords in aoi ----
    outRecs <- !(nwRec$lon > extent(aoi)@xmin & nwRec$lon < extent(aoi)@xmax &
                         nwRec$lat > extent(aoi)@ymin & nwRec$lat < extent(aoi)@ymax)
    
    if(any(outRecs) & fr.delOutCoords){
      if(all(outRecs)){
        nwRec <- nwRec[1, ]
        message('Non valid coordinates')
        fr.onlySp <- TRUE
        nwRec$dbDuplicate <- TRUE
      } else{
        nwRec <- nwRec[!outRecs, ]
        rec <- rec[!outRecs, ]
      }
      cat('\n', length(outRecs), 'coordinates outside region of interest!\n')
    }
  }

=======
    nwRec[, c('lat', 'lon')] <- sapply(nwRec[, c('lat', 'lon')], as.numeric)
    
    # 1 Incomplet coords ----
    if(any(is.na(nwRec$lat)) | any(is.na(nwRec$lon))) {
      message('Incomplete coordinates!')
      nwRec$lon <- nwRec$lat <- 0
      warning('Incomplete coordinates!')
    } else {
      
      # 2 Coords in aoi ----
      outRecs <- which(!(nwRec$lon > extent(aoi)@xmin & nwRec$lon < extent(aoi)@xmax &
                               nwRec$lat > extent(aoi)@ymin & nwRec$lat < extent(aoi)@ymax))
      if(any(outRecs)){
        nwRec <- nwRec[-outRecs, ]
        rec <- rec[-outRecs, ]
        cat('\n', length(outRecs), 'coordinates outside region of interest!\n')
      }
    }
  }
>>>>>>> origin/master
  
  # Taxonomic validation ----
  nwRec$species <- cleanSciNames(y = nwRec$species)
  nwRec <- taxValidation(.recT = nwRec, origRec = rec) # .recT = nwRec; origRec = rec
  
  if (fr.onlySp == FALSE ){
    
    #  Geographical validation ----
    #nwRec <- withCoords(.recG = nwRec)
    nwRec <- geoValidation(.recG = nwRec)
    
    nwRec <- geoValidationMunicipality(.recG = nwRec)
  }
  
  # Spp in Colombia
  nwRec <- sppInCol(.rec = nwRec)
  nwRec <- speciesInCountry(.rec = nwRec)
  
  # Record size 
  nwRec <- sizeBytes(.rec = nwRec)
  
  if (fr.onlySp == FALSE ){
    
    # db Duplicated
    nwRec <- dbDuplicate(.rec = nwRec)
    if (any(nwRec$dbDuplicate == TRUE)){
      message('Record duplicated inside the current data base. Catalog number and collection code values already registered')
    }
    
    # Uncertanity
    nwRec <- uncertanity(.rec = nwRec, thresh = 5000)
  }
  
  # Valid name
  nwRec <- validName(.rec = nwRec)
  
  # Taxonomic doubt
  nwRec <- hasTaxDoubt(.rec = nwRec)
  
  if (fr.onlySp == FALSE ){
    
    # Has locality
    nwRec <- hasLocality(.rec = nwRec)
    
    # Record in urban area
    nwRec <- inUrbanArea(.rec = nwRec)
    
    # Pixel ID y spatial duplicated
    nwRec <- cellID(.rec = nwRec)
    nwRec <- spatialDuplicated(.rec = nwRec)
    
    # URL
    nwRec <- url(.rec = nwRec, origRec = rec)
    
    # Dates
    nwRec <- dates(.rec = nwRec)
    
    # Override
<<<<<<< HEAD
    #nwRec$override <- .override
    
    # Visualization privileges in Biomodelos
    nwRec$recordsVisualizationPrivileges <- .recordsVisualizationPrivileges
=======
    nwRec$override <- .override
    
    # Visualization privileges in Biomodelos
    nwRec$visualizationPrivileges <- .visualizationPrivileges
>>>>>>> origin/master
    
    # contributedRecord
    nwRec$contributedRecord <- .contributedRecord
    
    # Inside known distribution of IUCN
    nwRec <- insideKnownDistribution(.rec = nwRec)
    
    # Get elevation from coordinates and google API
    nwRec <- demAltitude(.rec = nwRec)
    
    nwRec <- interpretedElevation(.rec = nwRec, .rule = 'mean')
    
    nwRec$altitudinalOutlier <- nwRec$consistentAltitude <- nwRec$diferenceInAltitude  <- nwRec$environmentalOutlier <- NA
    
    # nwRec <- altitudinalOutlier(.rec = nwRec)
    # nwRec <- environmentalOutlier(.rec = nwRec)
    # nwRec <- consistentAltitude(.rec = nwRec)
    # nwRec <- diferenceInAltitude(.rec = nwRec)
    
    nwRec$privateDataset <- .privateDataset
    
    nwRec <- tmpTax(.rec = nwRec, tmpTax = .tmpTax)
    nwRec <- tmpGeo(.rec = nwRec, tmpGeo = .tmpGeo)
    
    nwRec <- Use(.rec = nwRec, .fr.use = fr.use)
  }
  
  nwRec <- sppCateg(.rec = nwRec)
  
<<<<<<< HEAD
  return(list(nwRec = nwRec, fr.onlySp = fr.onlySp))
=======
  return(nwRec)
>>>>>>> origin/master
}

# <- function(.rec){
#   return(nwRec = .rec)
# }

<<<<<<< HEAD
checkMandatoryFieldsSpecies <- function(.rec){
  mandatoryFields <- .rec[1, speciesTableCols]
  all(!is.na(mandatoryFields) | mandatoryFields != '')
  return(nwRec = .rec)
}

sppCateg <- function(.rec){
  .rec$bmClass <- .rec$cites <- .rec$migratoryType <- ''
  .rec$iucn <- .rec$amenazaNacional <- 'NE'
  .rec$invasive <-  .rec$endemic <- FALSE
  .rec$iucnSpeciesID <- 0
=======
sppCateg <- function(.rec){
  .rec$bmClass <- .rec$iucn <- .rec$cites <- ''
  .rec$invasive <-  .rec$endemic <- FALSE
>>>>>>> origin/master
  
  .rec$bmClass[grep('Mammalia', .rec$class)] <- 'mamiferos'
  .rec$bmClass[grep('Amphibia', .rec$class)] <- 'anfibios'
  .rec$bmClass[grep('Reptilia', .rec$class)] <- 'reptiles'
  .rec$bmClass[grep('Aves', .rec$class)] <- 'aves'
  .rec$bmClass[
    grep('Actinopterygii|Myxini|^Characiformes$|^Siluriformes$|^Perciformes$|^Gymnotiformes$|^Cyprinodontiformes$|^Clupeiformes$|^Myliobatiformes$|^Pleuronectiformes$|^Beloniformes$|^Batrachoidiformes$|^Osteoglossiformes$|^Lepidosireniformes$|^Synbranchiformes$|^Tetraodontiformes$',
         .rec$class)] <- 'peces'
  .rec$bmClass[grep('Plantae', .rec$kingdom)] <- 'plantas'
  .rec$bmClass[grep('Arthropoda|Mollusca', .rec$phylum)] <- 'invertebrados'
  
  # Invasive
  .rec$invasive[.rec$acceptedNameUsage %in% inv] <- TRUE
  if (.rec$invasive[1] == FALSE){
    .rec$invasive[.rec$species %in% inv] <- TRUE
  }
  
  .rec$endemic[.rec$acceptedNameUsage %in% end] <- TRUE
  if (.rec$endemic[1] == FALSE){
    .rec$endemic[.rec$species %in% end] <- TRUE
  }
  
  iuPos <- any(.rec$acceptedNameUsage %in% iuc$acceptedNameUsage)
<<<<<<< HEAD
  if (iuPos & !is.na(.rec$acceptedNameUsage)){
    .rec$iucn <- iuc$Red.List.status[iuc$acceptedNameUsage %in% .rec$acceptedNameUsage][1]
  } else {
    iuPos <- any(.rec$acceptedNameUsage %in% iuc$sciName)
    if (iuPos & !is.na(.rec$acceptedNameUsage)){
      .rec$iucn <- iuc$Red.List.status[iuc$sciName %in% .rec$acceptedNameUsage][1]
    } else {
      iuPos <- any(.rec$species %in% iuc$acceptedNameUsage)
      if (iuPos & !is.na(.rec$species)){
        .rec$iucn <- iuc$Red.List.status[iuc$acceptedNameUsage %in% .rec$species][1]
      } else {
        iuPos <- any(.rec$species %in% iuc$sciName)
        if (iuPos & !is.na(.rec$species)){
=======
  if (iuPos){
    .rec$iucn <- iuc$Red.List.status[iuc$acceptedNameUsage %in% .rec$acceptedNameUsage][1]
  } else {
    iuPos <- any(.rec$acceptedNameUsage %in% iuc$sciName)
    if (iuPos){
      .rec$iucn <- iuc$Red.List.status[iuc$sciName %in% .rec$acceptedNameUsage][1]
    } else {
      iuPos <- any(.rec$species %in% iuc$acceptedNameUsage)
      if (iuPos){
        .rec$iucn <- iuc$Red.List.status[iuc$acceptedNameUsage %in% .rec$species][1]
      } else {
        iuPos <- any(.rec$species %in% iuc$sciName)
        if (iuPos){
>>>>>>> origin/master
          .rec$iucn <- iuc$Red.List.status[iuc$sciName %in% .rec$species][1]
        }
      }
    }
  }
  
  citPos <- any(.rec$acceptedNameUsage %in% na.omit(cit$acceptedNameUsage))
<<<<<<< HEAD
  if (citPos & !is.na(.rec$acceptedNameUsage)){
    .rec$cites <- cit$CurrentListing[cit$acceptedNameUsage %in% .rec$acceptedNameUsage][1]
  } else {
    citPos <- any(.rec$acceptedNameUsage %in% cit$FullName)
    if (citPos & !is.na(.rec$acceptedNameUsage)){
      .rec$cites <- cit$CurrentListing[cit$FullName %in% .rec$acceptedNameUsage][1]
    } else {
      citPos <- any(.rec$species %in% cit$acceptedNameUsage)
      if (citPos & !is.na(.rec$species)){
        .rec$cites <- cit$CurrentListing[cit$acceptedNameUsage %in% .rec$species][1]
      } else {
        citPos <- any(.rec$species %in% cit$FullName)
        if (citPos & !is.na(.rec$species)){
=======
  if (citPos){
    .rec$cites <- cit$CurrentListing[cit$acceptedNameUsage %in% .rec$acceptedNameUsage][1]
  } else {
    citPos <- any(.rec$acceptedNameUsage %in% cit$FullName)
    if (citPos){
      .rec$cites <- cit$CurrentListing[cit$FullName %in% .rec$acceptedNameUsage][1]
    } else {
      citPos <- any(.rec$species %in% cit$acceptedNameUsage)
      if (citPos){
        .rec$cites <- cit$CurrentListing[cit$acceptedNameUsage %in% .rec$species][1]
      } else {
        citPos <- any(.rec$species %in% cit$FullName)
        if (citPos){
>>>>>>> origin/master
          .rec$cites <- cit$CurrentListing[cit$FullName %in% .rec$species][1]
        }
      }
    }
  }
<<<<<<< HEAD
=======
  .rec$migratoryType <- NA
>>>>>>> origin/master
  return(nwRec = .rec)
}

sppCategDifSpp <- function(.rec){
<<<<<<< HEAD
  .rec$bmClass <- .rec$iucn <- .rec$cites <- .rec$migratoryType <-  ''
=======
  .rec$bmClass <- .rec$iucn <- .rec$cites <- ''
>>>>>>> origin/master
  .rec$invasive <-  .rec$endemic <- FALSE
  
  .rec$bmClass[grep('Mammalia', .rec$class)] <- 'mamiferos'
  .rec$bmClass[grep('Amphibia', .rec$class)] <- 'anfibios'
  .rec$bmClass[grep('Reptilia', .rec$class)] <- 'reptiles'
  .rec$bmClass[grep('Aves', .rec$class)] <- 'aves'
  .rec$bmClass[
    grep('Actinopterygii|Myxini|^Characiformes$|^Siluriformes$|^Perciformes$|^Gymnotiformes$|^Cyprinodontiformes$|^Clupeiformes$|^Myliobatiformes$|^Pleuronectiformes$|^Beloniformes$|^Batrachoidiformes$|^Osteoglossiformes$|^Lepidosireniformes$|^Synbranchiformes$|^Tetraodontiformes$',
         .rec$class)] <- 'peces'
  .rec$bmClass[grep('Plantae', .rec$kingdom)] <- 'plantas'
  .rec$bmClass[grep('Arthropoda|Mollusca', .rec$phylum)] <- 'invertebrados'
  
  # Invasive
  .rec$invasive[.rec$acceptedNameUsage %in% inv] <- TRUE
  .rec$invasive[.rec$species %in% inv] <- TRUE

  .rec$endemic[.rec$acceptedNameUsage %in% end] <- TRUE
  .rec$endemic[.rec$species %in% end] <- TRUE

  ## UICN
  iuAA <- iuc$Red.List.status[match(.rec$acceptedNameUsage, iuc$acceptedNameUsage)]
  iuAS <- iuc$Red.List.status[match(.rec$acceptedNameUsage, iuc$originalNameUsage)]
  iuSA <- iuc$Red.List.status[match(.rec$species, iuc$acceptedNameUsage)]
  iuSS <- iuc$Red.List.status[match(.rec$species, iuc$originalNameUsage)]
  
  iuAA[is.na(.rec$acceptedNameUsage)] <- iuAS[is.na(.rec$acceptedNameUsage)] <- NA
  iuSA[is.na(.rec$species)] <- iuSS[is.na(.rec$species)] <- NA
  iuDF <- data.frame(iuAA, iuAS, iuSA, iuSS)
  uiValsAcc <- apply(iuDF[, c('iuAA', 'iuAS')], 1, function(x){
    paste0(na.omit(unique(x)), collapse = ', ')
  })
  
  uiValsOri <- apply(iuDF[, c('iuSA', 'iuSS')], 1, function(x){
    paste0(na.omit(unique(x)), collapse = ', ')
  })
  
  
  ## CITES
  citAA <- cit$Red.List.status[match(.rec$acceptedNameUsage, cit$acceptedNameUsage)]
  citAS <- cit$Red.List.status[match(.rec$acceptedNameUsage, cit$FullName)]
  citSA <- cit$Red.List.status[match(.rec$species, cit$acceptedNameUsage)]
  citSS <- cit$Red.List.status[match(.rec$species, cit$FullName)]
  
  citAA[is.na(.rec$acceptedNameUsage)] <- citAS[is.na(.rec$acceptedNameUsage)] <- NA
  citSA[is.na(.rec$species)] <- citSS[is.na(.rec$species)] <- NA
  citDF <- data.frame(citAA, citAS, citSA, citSS)

  citValsAcc <- apply(citDF[, c('citAA', 'citAS')], 1, function(x){
    paste0(na.omit(unique(x)), collapse = ', ')
  })
  
  citValsOri <- apply(citDF[, c('citSA', 'citSS')], 1, function(x){
    paste0(na.omit(unique(x)), collapse = ', ')
  })
  
  .rec$citesOrig <- citValsOri
  .rec$citesAcc <- citValsOri
  .rec$iucnOrig <- uiValsOri
  .rec$iucnAcc <- uiValsAcc
  
  .rec$migratoryType <- NA
  return(nwRec = .rec)
}


<<<<<<< HEAD
dates <- function(.rec){
  
  .rec$earliestDateCollected <- ifelse(.rec$earliestDateCollected == '', NA, .rec$earliestDateCollected)
  #.rec$latestDateCollected <- ifelse(.rec$latestDateCollected == '', NA, .rec$latestDateCollected)
  
  Dates <- sapply(.rec[, c('earliestDateCollected')], # , 'latestDateCollected'. From apply to sapply
                   function(y){
=======

dates <- function(.rec){
  
  .rec$earliestDateCollected <- ifelse(.rec$earliestDateCollected == '', NA, .rec$earliestDateCollected)
  .rec$latestDateCollected <- ifelse(.rec$latestDateCollected == '', NA, .rec$latestDateCollected)
  
  Dates <- t(apply(.rec[, c('earliestDateCollected', 'latestDateCollected')], 1, function(y){
>>>>>>> origin/master
    #y <- .rec[11, c('earliestDateCollected', 'latestDateCollected')]
    x <- na.omit(as.character(y))[1]
    if (length(x) == 0) return(NA)
    x <- gsub('-  NA / NA / NA', '', x)
    puncts <- gsub('[0-9]|[a-zA-Z]|\\+|:|\\.', '', x)
    tPunc <- table(strsplit(puncts, '*')[[1]])
    sep <- names(which.max(tPunc))
    potSep <- names(which(tPunc == 1))
    if(length(potSep)>0){
      x <- strsplit(x, potSep)[[1]][1]
    }
    if(any(grep('[0-9]T[0-9]|[0-9]t[0-9]', x))){
      x <- strsplit(x, 't|T')[[1]][1]
    }
    if(!is.null(sep)){
      (x <- gsub(sep, '-', x))
    }
    (pDate <- parse_date_time(x, c("Y-m-d", "Y-m", "dbY HMS", "dmyHMS", "BdY H", 'y', "Ymd", "mdY", 'dmY', '%m/%d/%Y')))
    # as.character(gsub('[a-zA-Z]| ', '', pDate))
    rDate <- c(y = year(pDate), m = month(pDate), d = day(pDate))
    return(rDate)
<<<<<<< HEAD
  })
  
  .rec$year <- Dates[1]
  .rec$month <- Dates[2]
  .rec$day <- Dates[3]
=======
  }))
  
  .rec$yyyy <- Dates[, 1]
  .rec$mm <- Dates[, 2]
  .rec$dd <- Dates[, 3]
>>>>>>> origin/master
  return(nwRec = .rec)
}

tmpGeo <- function(.rec, tmpGeo){
  .rec$tmpGeo <- tmpGeo
  return(nwRec = .rec)
}

tmpTax <- function(.rec, tmpTax){
  .rec$tmpTax <- tmpTax
  return(nwRec = .rec)
}

altitudinalOutlier <- function(.rec){
  .rec$altitudinalOutlier <- FALSE
  return(nwRec = .rec)
}

environmentalOutlier <- function(.rec){
  .rec$environmentalOutlier <- NA
  mongoQ <- ifelse(is.na(.rec$acceptedNameUsage),
                   paste0('{"$or":[{"species":"', .rec$species, '"}, {"acceptedNameUsage":"', .rec$species, '"}]}'),
                   paste0('{"$or":[{"species":"', .rec$species, '"}, {"acceptedNameUsage":"', .rec$species,
                          '"}, {"species":"', .rec$acceptedNameUsage, '", "acceptedNameUsage":"', .rec$acceptedNameUsage, '"}]}'))
  mongoTable <- mongoConectRec$find(mongoQ, fields = '{"_id" : 0, "lat" : 1, "lon" : 1, "dbDuplicate" : 1}')
  head(mongoTable)
  if (nrow(mongoTable) >= 9){
    dat <- rbind(mongoTable, .rec[, c('lat', 'lon', 'dbDuplicate')])
    coordSpatial <- SpatialPoints(dat[, c(2:1)], proj4string = CRS("+proj=longlat +datum=WGS84"))
    extVals <- raster::extract(layNE, coordSpatial)
    envOut <- apply(extVals, 2, FUN = function(x) {
      apply(outliersIG(rid = 1:nrow(dat), species = rep('SP', nrow(dat)), dups = dat[, 3], ev = x), 1, function(x) any(x) * 1)
    })
    
    .rec$environmentalOutlierLayer <- NA
    if( any(tail(envOut, 1) == 1)) {
      .rec$environmentalOutlier <- TRUE
      .rec$environmentalOutlierLayer <- paste0(colnames(envOut)[which(tail(envOut, 1) == 1)], collapse = '-')
    }
  }
  return(nwRec = .rec)
}

outliersIG <- function(rid, species, dups, ev){
  # rid = .datenv$ID; species = .datenv$Species; dups = rep(0, nrow(.datenv)); ev = x
  # rid = rid; species = cid2$Species; dups = cid2$dups; ev = env[, i]
  uspp <- unique(species)
  nr <- length(species)
  ee <- rep(0, nr)
  ee2 <- rep(0, nr)
  for (j in 1:length(uspp)) {
    spp <- uspp[j]
    fsp <- which(species == spp & dups == 0 & !is.na(ev))
    if (length(fsp) >= 10) {
      xc <- ev[fsp] # datos seleccionados para especie
      ri <- rid[fsp] # ids de los datos
      b1 <- boxplot.stats(xc, coef = 1.5)
      xr <- range(b1$stats)
      fe <- which(xc > xr[2] | xc < xr[1]) # posicion de datos y ID fuera de rango m1
      fe2 <- rjackIG(d = xc) # posicion de datos y ID fuera de rango m2
      ff1 <- which(rid %in% ri[fe])
      ff2 <- which(rid %in% ri[fe2])
      if (any(ff1)) ee[ff1] <- 1
      if (any(ff2)) ee2[ff2] <- 1
    }
  }
  out <- cbind(ee, ee2)
  return(out)
}

rjackIG <- function (d) {
  xx <- d
  d <- unique(d)
  mx <- mean(d)
  n <- length(d)
  n1 <- n - 1
  t1 <- (0.95 * sqrt(n)) + 0.2
  x <- sort(d)
  y <- rep(0, n1)
  for (i in 1:n1) {
    x1 <- x[i + 1]
    if (x[i] < mx) {
      y[i] <- (x1 - x[i]) * (mx - x[i])
    }
    else {
      y[i] <- (x1 - x[i]) * (x1 - mx)
    }
  }
  my <- mean(y)
  z <- y/(sqrt(sum((y - my)^2)/n1))
  
  out <- rep(0, length(xx))
  if (any(z > t1)) {
    f <- which(z > t1)
    v <- x[f]
    if (v < median(x)) {
      xa <- (xx <= v) * 1
      out <- out + xa
    }
    if (v > median(x)) {
      xb <- (xx >= v) * 1
      out <- out + xb
    }
  } else {
    out <- out
  }
  #return(out)
  return(which(out == 1))
}

demAltitude <- function(.rec, .method = 'raster'){
  .rec$demAltitude <- NA
  
  if (.method == 'raster'){
    naCoords <- which(is.na(.rec$lat) & is.na(.rec$lon))
    Coords <- .rec[, c('lon', 'lat')]
    Coords[naCoords, ] <- 0
    coordSpatial <- SpatialPoints(Coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
    Coords$demAltitude <- raster::extract(demAoi, coordSpatial)
    Coords$demAltitude[naCoords] <- NA
    .rec$demAltitude <- as.numeric(Coords$demAltitude)
  }
  
  if (.method == 'web'){
    #url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=",
    #              .rec$lat, ',', .rec$lon, "&key=AIzaSyAz6sPnMLuIJrOWvNKnn6-PWYK56PDGa30")
    # # #x <- url[1]
    # # elevation <- sapply(url, USE.NAMES = FALSE, function(x){
    # #   # x <-  url[1] 
    # #   .elev <- tryCatch(GET(x), error = function (e) {e})
    # #   if(any(grep('error', class(.elev)))){
    # #     elev <- -9999
    # #   } else{
    # #     elevChar <- as.character(.elev)
    # #     elev2 <- strsplit(elevChar, ' ')[[1]]
    # #     elev3 <- elev2[grep('elev', elev2) + 2]
    # #     finPos <- regexec(pattern = '\\,', elev3)[[1]]
    # #     elev <- as.numeric(substr(elev3, 0, finPos - 1))
    # #   }
    # #   return(elev)
    # # })
    # # 
    # .rec$demAltitude <- elevation
    
  }
  
  return(nwRec = .rec)
}


Use <- function(.rec, .fr.use){
  # toUse <- which(.rec$withLat== 1 & .rec$withLon == 1,
  #                .rec$sppInCol == 1 & .rec$speciesInCountry == TRUE &
  #                  .rec$tmpTax == 1 & .rec$tmpGeo == 1 &
  #                  .rec$bigSizeRecord == 0 &
  #                  .rec$dbDuplicate == FALSE & .rec$spatialDuplicated == FALSE & 
  #                  .rec$lowUncertainty == 0 &
  #                  .rec$hasLocality == TRUE &
  #                  .rec$hasTaxDoubt == 0 & 
  #                  .rec$override == FALSE )
  # 
  # .rec$use <- ifelse(length(toUse) == 0, 0, 1)
  .rec$use <- .fr.use
  return (nwRec = .rec)
}

url <- function(.rec, origRec){
  .rec <- .rec
  origRec$url <- ifelse(is.null(origRec$url), NA, as.character(origRec$url))
  # atUrl <- grep('@', origRec$url)
  # posUrl <- c(which(is.na(origRec$url)), atUrl)
  # pos <- ! (1:nrow(origRec) %in% posUrl)
  # if(any(pos)){
  #   .rec$url[pos] <- sapply(origRec$url[pos], function(x) {
  #     tryCatch(ifelse(GET(x)$status == 200, origRec$url, NA), error = function (e) {NA})
  #   })
  # }
  return (nwRec = .rec)
}

consistentAltitude <- function(.rec){
  .rec$consistentAltitude <- FALSE
  return(nwRec = .rec)
}

diferenceInAltitude <- function(.rec){
  .rec$diferenceInAltitude <- NA
  return(nwRec = .rec)
}

#oSpec <- .rec$species; .rec$species <- 'Ateles hybridus'
insideKnownDistribution <- function(.rec){
  .rec$insideKnownDistribution <- FALSE
  .rec$dist2KnowRange <- NA
  #return(nwRec = .rec) # comment when uncoment the other
  #if(.rec$lat != 0 & .rec$lon != 0 & !is.na(.rec$lat) & !is.na(.rec$lon)){
  
  # availableMap <- c(.rec$species[1], .rec$acceptedNameUsage[1]) %in%
  #   unique(na.omit(c(iucnShapes$iucnShapes, iucnShapes$acceptedNameUsage)))
  # 
  # if (any(availableMap)){
  #   spIucn <- which(iucnShapes$acceptedNameUsage %in% c(.rec$species[1], .rec$acceptedNameUsage[1]))
  #   spIucn2 <- which(iucnShapes$iucnShapes %in% c(.rec$species[1], .rec$acceptedNameUsage[1]))
  #   if (any(spIucn)){
  #     #field2Load <- c(.rec$acceptedNameUsage[1], .rec$species[1]) %in% iucnShapes$iucnShapes[spIucn]
  #     #field2Load2 <- c(.rec$acceptedNameUsage[1], .rec$species[1])[field2Load][1]
  #     #iucnShape <- readOGR(iucnShapesPath, field2Load2, verbose = FALSE)
  #     iucnShape <- readOGR(iucnShapesPath, iucnShapes$iucnShapes[spIucn], verbose = FALSE)
  #   } else if(!any(spIucn)) {
  #     #field2Load <- c(.rec$acceptedNameUsage, .rec$species) %in% iucnShapes$acceptedNameUsage[spIucn]
  #     #field2Load2 <- c(.rec$acceptedNameUsage, .rec$species)[field2Load][1]
  #     iucnShape <- readOGR(iucnShapesPath, iucnShapes$iucnShapes[spIucn2], verbose = FALSE)
  #   }
  #   coords0 <- .rec
  #   coordinates(coords0) =~ lon + lat
  #   coords0@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #   
  #   #if(iucnShape@proj4string@projargs != coords@proj4string@projargs){
  #   #  coords1 <- spTransform(coords0[1, ], CRSobj = CRS(prjUTM))
  #   #}
  #   # 
  #   # if(iucnShape@proj4string@projargs != coords@proj4string@projargs){
  #   #   iucnShape <- spTransform(iucnShape, CRSobj = coords@proj4string)
  #   # }
  #   
  #   over <- over(coords0, iucnShape)
  #   outPoints <- is.na(over$id_no)
  #   .rec$insideKnownDistribution[!outPoints] <- TRUE
  #   if (any(outPoints)){
  #     .rec$dist2KnowRange[outPoints] <- gDistance(coords0[outPoints, ], iucnShape, byid = TRUE) * 0.00083
  #   }
  #   
  # }
  return(nwRec = .rec)
}


spatialDuplicated <- function(.rec){
  
  ## Reemplazar 'speciesOriginal' por 'species'
  #mongoConectRec <- mongo(db = 'records', collection = 'records', url='mongodb://192.168.11.81:27017', verbose = FALSE)
  .rec$spatialDuplicated <- FALSE
  #latOk <- ifelse(abs(.rec$lat > 180) | is.na(.rec$lat) | .rec$lat == 0, 0, 1)
  #lonOk <- ifelse(abs(.rec$lon > 180) | is.na(.rec$lon) | .rec$lon == 0, 0, 1)
  
  mongoQ <- ifelse(is.na(.rec$acceptedNameUsage[1]),
                   paste0('{"$or":[{"species":"', .rec$species[1], '"}, {"acceptedNameUsage":"', .rec$species[1], '"}]}'),
                   paste0('{"$or":[{"species":"', .rec$species[1], '"}, {"acceptedNameUsage":"', .rec$species[1],
                          '"}, {"species":"', .rec$acceptedNameUsage[1], '", "acceptedNameUsage":"', .rec$acceptedNameUsage[1], '"}]}'))
  
  mongoTable <- tryCatch(mongoConectRec$find(mongoQ), error = function (e) {NULL})
  if(!is.null(mongoTable)){
    

  #mongoConectRec$count(mongoQ)
  #mongoTable$catalogNumber
  if (nrow(mongoTable) != 0){
    
    recInDb <- mongoTable[, c('cellID', 'species', 'acceptedNameUsage', 'correctCounty')]
    
    indDup <- paste0(recInDb$acceptedNameUsage, '-', recInDb$cellID, '_', recInDb$correctCounty)
    indDup2 <- paste0(recInDb$species,          '-', recInDb$cellID, '_', recInDb$correctCounty)
    
    isSpatialDup <- ifelse(is.na(.rec$acceptedNameUsage),
                           paste0(.rec$species, '-', .rec$cellID, '_', .rec$correctCounty) %in% indDup2,
                           paste0(.rec$acceptedNameUsage, '-', .rec$cellID, '_', .rec$correctCounty) %in% indDup)
    
    .rec$spatialDuplicated <- ifelse(isSpatialDup == TRUE, TRUE, FALSE)
  } 
  }
  return(nwRec = .rec)
  
}

cellID <- function(.rec){
  .rec$cellID <- 0
  #latOk <- ifelse(abs(.rec$lat > 180) | is.na(.rec$lat) | .rec$lat == 0, 0, 1)
  #lonOk <- ifelse(abs(.rec$lon > 180) | is.na(.rec$lon) | .rec$lon == 0, 0, 1)
  #if(latOk & lonOk){      
  coords <- .rec
  .rec$lon[is.na(.rec$lon)] <- 0
  .rec$lat[is.na(.rec$lat)] <- 0
  coordinates(coords) =~ lon + lat
  .rec$cellID <- as.numeric(raster::extract(id, coords))
  #}
  return(nwRec = .rec)
}

inUrbanArea <- function(.rec){
  .rec$inUrbanArea <- ifelse(is.na(.rec$suggestedMunicipality), FALSE, TRUE)
  return(nwRec = .rec)
}

hasLocality <- function(.rec){
<<<<<<< HEAD
  .rec$hasLocality <- ifelse(is.na(.rec$verbatimLocality) | .rec$verbatimLocality == '', FALSE, TRUE)
=======
  .rec$hasLocality <- ifelse(is.na(.rec$locality) | .rec$locality == '', FALSE, TRUE)
>>>>>>> origin/master
  return(nwRec = .rec)
}

hasTaxDoubt<- function(.rec){
  .rec$hasTaxDoubt <- 0
  taxDoubdt <- grep(" cf\\. | cf |\\?| aff | sp\\.| sp\\. | sp | aff\\. | affin\\. | aff | affin | sp$", 
                    .rec$speciesOriginal)
  .rec$hasTaxDoubt[taxDoubdt] <- 1
  return(nwRec = .rec)
}

validName <- function(.rec){
  binom <- grep(' ', .rec$species)
  .rec$validName <- ifelse(is.na(.rec$acceptedNameUsage) | length(binom) == 0, FALSE, TRUE)
  return(nwRec = .rec)
}

interpretedElevation <- function(.rec, .rule = 'mean'){
<<<<<<< HEAD
  sep <- grep('-|/', .rec$verbatimElevation)
  .rec$verbatimElevation[is.na(.rec$verbatimElevation)] <- '-'
  if (any(sep)){
    sepAlts <- sapply(.rec$verbatimElevation, USE.NAMES = FALSE, function(x) {
      #  (x <- .rec$verbatimElevation[1])
      sepAlts <- as.numeric(strsplit(gsub('[[:alpha:]]|\\.', '', x), '-|/')[[1]])
      if(.rule == 'mean') z <- mean(sepAlts)
      if(.rule == 'max')  z <- max(sepAlts)
      if(.rule == 'min') z <- min(sepAlts)
      return(z)
    })
    .rec$interpretedElevation <- sepAlts
  } else {
    .rec$interpretedElevation <- as.numeric(gsub('[[:alpha:]]', '', .rec$verbatimElevation))
=======
  sep <- grep('-|/', .rec$alt)
  if (any(sep)){
    sepAlts <- as.numeric(strsplit(.rec$alt, '-|/')[[1]])
    if(.rule == 'mean') .rec$interpretedElevation <- mean(sepAlts)
    if(.rule == 'max') .rec$interpretedElevation <- max(sepAlts)
    if(.rule == 'min') .rec$interpretedElevation <- min(sepAlts)
  } else {
    .rec$interpretedElevation <- as.numeric(gsub('[[:alpha:]]', '', .rec$alt))
>>>>>>> origin/master
  }
  return(nwRec = .rec)
}

uncertanity <- function(.rec, thresh = 5000){
<<<<<<< HEAD
  numUnc <- as.numeric(gsub('[[:alpha:]]', '', .rec$coordinateUncertaintyInMeters))
  feet <- grep('f', .rec$coordinateUncertaintyInMeters)
=======
  numUnc <- as.numeric(gsub('[[:alpha:]]', '', .rec$coordUncertaintyM))
  feet <- grep('f', .rec$coordUncertaintyM)
>>>>>>> origin/master
  if (any(feet)){
    .rec$numUnc[feet] <- .rec$numUnc[feet] * .3048
  }
  .rec$lowUncertainty <- ifelse(numUnc >= 5000, TRUE, FALSE)
  #.rec$lowUncertainty <- ifelse(is.na(numUnc), FALSE, .rec$lowUncertainty)
  return(nwRec = .rec)
}

dbDuplicate <- function(.rec){
  .rec$dbDuplicate <- FALSE
  #   .rec$collection <- 'IAvH-M'
  #   .rec$catalogNumber <- 421  
  #mongoConectRec <- mongo(db = 'records', collection= 'records', url='mongodb://192.168.11.81:27017', verbose = FALSE)
  
<<<<<<< HEAD
  dup <- paste0(.rec$acceptedNameUsage, '__', .rec$collectionCode, '--', .rec$catalogNumber)
=======
  dup <- paste0(.rec$acceptedNameUsage, '__', .rec$collection, '--', .rec$catalogNumber)
>>>>>>> origin/master
  naCases <- '^NA__|^__|__NA--|__--|--$|--NA$'
  naValue <- grep(naCases, dup)
  if(!any(naValue)){
    
    mongoQ <- ifelse(is.na(.rec$acceptedNameUsage[1]) | .rec$acceptedNameUsage[1] == '',
                     paste0('{"species":"', .rec$species[1], '", "acceptedNameUsage":"', .rec$species[1], '"}'),
                     paste0('{"species":"', .rec$acceptedNameUsage[1], '", "acceptedNameUsage":"', .rec$acceptedNameUsage[1], '"}'))
    
    mongoTable <- mongoConectRec$find(mongoQ)
    if (nrow(mongoTable) != 0){
      recSppInDb <- mongoTable[, c('colection', 'catalogNumber')]
      indDup <- paste(recSppInDb$colection, recSppInDb$catalogNumber, sep ="-")
      indDup <- gsub("^NA-NA$|^-$|^-NA$|^NA-$", '', indDup)
      dup <- paste0(.rec$colection, '-', .rec$catalogNumber)
      dupInd <- which(dup %in% indDup)
      .rec$dbDuplicate[dupInd] <- TRUE
    }
  }
  return(nwRec = .rec) 
}

sizeBytes <- function(.rec){
  .rec$recSizeBytes <- as.numeric(object.size(.rec))
  .rec$bigSizeRecord <-  ifelse(.rec$recSizeBytes > 16000000, 1, 0)
  return(nwRec = .rec)
}

speciesInCountry <- function(.rec){
  .rec$speciesInCountry <- TRUE
  return(nwRec = .rec)
}

sppInCol <- function(.rec){
  .rec$sppInCol <- FALSE
  #mongoConectSpe <- mongo(db = "records", collection = "species", url = "mongodb://192.168.11.81:27017", verbose = FALSE)
  
  # Desde las coordenadas
  sugg <- grep('Colombia', .rec$suggestedCountry)
  validMun <- grep('CO|^co', .rec$sourceLayer)
  if (!any(unique(c(sugg, validMun)))){
    .rec$sppInCol <- TRUE
    rm(sugg, validMun)
  } else {
    # Desde el listado de especies (TAX de DINAVIS)
    sp.Mongo <- mongoConectSpe$find('{"sppInCol":1}')
    if(nrow(sp.Mongo) != 0){
      mongoNames <- gsub(' ', '', na.omit(unique(c(sp.Mongo$species, sp.Mongo$acceptedNameUsage))))
      sp.Mongo1 <- which(gsub(' ', '', .rec$species) %in% mongoNames)
      sp.Mongo2 <- which(gsub(' ', '', .rec$speciesOriginal) %in% mongoNames)
      sp.Mongo3 <- which(gsub(' ', '', .rec$acceptedNameUsage) %in% mongoNames)
      
      sp.TAX1 <- which(gsub(' ', '', .rec$species) %in% TAXNames)
      sp.TAX2 <- which(gsub(' ', '', .rec$speciesOriginal) %in% TAXNames)
      sp.TAX3 <- which(gsub(' ', '', .rec$acceptedNameUsage) %in% TAXNames)
      posSppTAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3, sp.Mongo1, sp.Mongo2, sp.Mongo3))
      if (any(posSppTAX)) .rec$sppInCol <- TRUE
    }
  }
  return(nwRec = .rec)
}

geoValidationMunicipality <- function(.recG, .scriptMaxchar = .2){  
  coords <- .recG
  coordinates(coords) =~ lon + lat
  coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ovMunicipality <- over(coords, casco)
  
  .recG$suggestedMunicipality <- ovMunicipality$NOMBRE_GEO
  return(nwRec = .recG)
}  

nwRecFun <- function(.rec, .reqCols){
  .nw <- data.frame(matrix(NA, nrow = nrow(.rec), ncol = length(.reqCols)), stringsAsFactors = FALSE)
  colnames(.nw) <- .reqCols
  commCols <- base::intersect(colnames(.nw), colnames(.rec))
  .nw[, commCols] <- .rec[, commCols]
  return(.nw)
}

taxValidation <- function(.recT, origRec){
  #origRec$acceptedNameUsage <- .recT$species # for test
  #mongoConectSpe <- mongo(db = "records", collection = "species", url = "mongodb://192.168.11.81:27017", verbose = FALSE)
  
  mongoQ <- paste0('{"$or":[{"species":"', .recT$species[1], '"}, {"acceptedNameUsage":"', .recT$species[1], '"}]}')
  mongoCount <- mongoConectSpe$count(mongoQ)
  
  if(mongoCount != 0){
    currentTax <- mongoConectSpe$find(mongoQ)[1, ]
    #deleteMe <- mongoConectSpe$find()
    .recT$originalGenus <- currentTax$originalGenus
    .recT$originalSpecificEpithet <- currentTax$originalSpecificEpithet
    .recT$taxonomicStatus <- currentTax$taxonomicStatus
    .recT$originalScientificNameID <- currentTax$originalScientificNameID
    .recT$scientificNameID <- currentTax$scientificNameID
    .recT$acceptedNameUsage <- currentTax$acceptedNameUsage
    .recT$scientificNameAuthorship <- currentTax$scientificNameAuthorship
    .recT$kingdom <- currentTax$kingdom
    .recT$phylum <- currentTax$phylum
    .recT$class <- currentTax$class
    .recT$order <- currentTax$order
    .recT$family <- currentTax$family
    .recT$genus <- currentTax$genus
    .recT$specificEpithet <- currentTax$specificEpithet
    .recT$nameAccordingTo <- currentTax$nameAccordingTo
<<<<<<< HEAD
    .recT$taxVerifSource <- currentTax$taxVerifSource
    
  } else if(mongoCount == 0){
    
    vtaxCols <- c('acceptedNameUsage')
    vtaxCols2 <- c('kingdom', 'phylum', 'class', 'order', 'family')
    haveAllTaxFields <- all(vtaxCols %in% colnames(origRec))
    haveAllTaxFieldsFilled <- ifelse(haveAllTaxFields == TRUE, 
                                     all(origRec[, vtaxCols2] != '' & !is.na(origRec[, vtaxCols2])),
=======
    .recT$TaxVerifSource <- currentTax$TaxVerifSource
    
  } else if(mongoCount == 0){
    
    taxCols <- c('acceptedNameUsage')
    taxCols2 <- c('kingdom', 'phylum', 'class', 'order', 'family')
    haveAllTaxFields <- all(taxCols %in% colnames(origRec))
    haveAllTaxFieldsFilled <- ifelse(haveAllTaxFields == TRUE, 
                                     all(origRec[, taxCols] != '' & !is.na(origRec[, taxCols])),
>>>>>>> origin/master
                                     FALSE)
    if(haveAllTaxFields & haveAllTaxFieldsFilled){
      
      set1 <- data.frame(id = 1, nombre = .recT$species,
                         genero = strsplit(.recT$species, ' ')[[1]][1], 
                         epiteto_especifico = strsplit(.recT$species, ' ')[[1]][2],
                         stringsAsFactors = FALSE)
      
      set1 <- nameValidationGenus(con = conMSQ, inTable = set1[1, ]) # 1.98 sec
      set1$specificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      set1$originalNameUsage <- NULL
      .recT <- cbind(.recT, set1[, -c(1)])
<<<<<<< HEAD
      .recT$taxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
=======
      .recT$TaxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
>>>>>>> origin/master
      #       .recT$originalGenus <- strsplit(.recT$species, ' ')[[1]][1]
      #       .recT$originalSpecificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      .recT$originalSpecificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      .recT$taxonomicStatus <- 'pending'
      .recT$acceptedNameUsage <- origRec$acceptedNameUsage
      #       .recT$originalScientificNameID <- .recT$scientificNameID <- ''
      #       .recT$acceptedNameUsage <- .recT$species
      #       .recT$scientificNameAuthorship <- ''
      .recT$kingdom <- origRec$kingdom
      .recT$phylum <- origRec$phylum
      .recT$class <- origRec$class
      .recT$order <- origRec$order
      .recT$family <- origRec$family
      #       .recT$genus <- .recT$originalGenus
      #       .recT$specificEpithet <- .recT$originalSpecificEpithet
      
      .recT$nameAccordingTo <- 'Record author'
<<<<<<< HEAD
      .recT$taxVerifSource <- 'Record author'
=======
      .recT$TaxVerifSource <- 'Record author'
>>>>>>> origin/master
      
    } else {
      
      set0 <- data.frame(id = 1, nombre = .recT$species[1],
                         genero = strsplit(.recT$species[1], ' ')[[1]][1], 
                         epiteto_especifico = strsplit(.recT$species[1], ' ')[[1]][2],
                         stringsAsFactors = FALSE)
      
      
      set1 <- tryCatch(nameValidation2(con = conMSQ, inTable = set0), error = function (e) {e})
      if (any(class(set1) == 'error')){
        unErr <- unlist(as.character(set1))
        msqErr <- grep('MySQL', unErr)
        if (any(msqErr)){
          conMSQ <<- dbConnect(dbDriver("MySQL"), user = "root", password = "root", dbname = "col2015ac", host = "localhost")
        }
        set1 <- nameValidation2(con = conMSQ, inTable = set0) # 1.98 sec
      }
      
      set1$originalNameUsage <- NULL
      .recT <- cbind(.recT, set1[, -c(1)])
<<<<<<< HEAD
      .recT$taxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
=======
      .recT$TaxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
>>>>>>> origin/master
      
      ## Taxize
      #       if(is.na(set1$acceptedNameUsage)){
      #         require(taxize)
      #         dbs <- c("itis", "col", "tropicos", "eol", "ncbi")
      #         db <- 2
      #         for (db in 1:length(dbs)){
      #           class.db <- classification(nwRec$species, db = dbs[db])
      #           if (!is.logical(class.db[[1]])){
      #             taxize <- class.db[[1]]
      #             taxize <- taxize[taxize$rank %in% c("family", "order", "class", "phylum", "kingdom"), ]
      #             rankT <- taxize$rank
      #             taxize <- t(taxize$name)
      #             colnames(taxize) <- rankT
      #             .recT[, colnames(taxize)] <- taxize[, colnames(taxize)]
<<<<<<< HEAD
      #             .recT$taxVerifSource <- dbs[db]
=======
      #             .recT$TaxVerifSource <- dbs[db]
>>>>>>> origin/master
      #             .recT$acceptedNameUsage <- nwRec$species
      #             break
      #           }
      #         } 
      #       }
      #     
      
      ## COL RST API | http://api.cybertaxonomy.org/col/name_catalogue/fuzzy.json?accuracy=0.1&hits=1&query=Ateles%2520hybridus
      #       
      #       if(is.na(set1$acceptedNameUsage)){
      #         
      #         .recT$originalGenus <- strsplit(.recT$species, ' ')[[1]][1]
      #         .recT$originalSpecificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      #         .recT$taxonomicStatus <- 'pending'
      #         .recT$originalScientificNameID <- .recT$scientificNameID <- ''
      #         .recT$acceptedNameUsage <- .recT$species
      #         .recT$scientificNameAuthorship <- ''
      #         .recT$kingdom <- origRec$kingdom 
      #         .recT$phylum <- origRec$phylum
      #         .recT$class <- origRec$class
      #         .recT$order <- origRec$order
      #         .recT$family <- origRec$family
      #         .recT$genus <- .recT$originalGenus
      #         .recT$specificEpithet <- .recT$originalSpecificEpithet
      #         .recT$nameAccordingTo <- 'Record author'
<<<<<<< HEAD
      #         .recT$taxVerifSource <- 'Record author'
=======
      #         .recT$TaxVerifSource <- 'Record author'
>>>>>>> origin/master
      #       }
      
      ## The plant list
      # taxVal <- TPL(.recT$species, corr=TRUE)
    }
  }
<<<<<<< HEAD
  .recT$taxonomicStatus[grep('accepted name', .recT$taxonomicStatus)] <- 'accepted'
  .recT$taxonomicStatus[grep('ambiguous synonym', .recT$taxonomicStatus)] <- ''
  .recT$taxonomicStatus[grep('misapplied name', .recT$taxonomicStatus)] <- 'misapplied'
  .recT$taxonomicStatus[grep('provisionally accepted name', .recT$taxonomicStatus)] <- 'provisionally accepted name'
  .recT$taxonomicStatus[grep('synonym', .recT$taxonomicStatus)] <- 'synonym'
 
=======
>>>>>>> origin/master
  return(.recT)
}

withCoords <- function(.recG){  
  .recG$withLat <- ifelse(is.na(.recG$lat) | .recG$lat == 0, 0, 1)
  .recG$withLon <- ifelse(is.na(.recG$lon) | .recG$lon == 0, 0, 1)
  return(.recG) 
}

geoValidation <- function(.recG, .scriptMaxchar = .2){  
  .recG$geoID <- 1:nrow(.recG)
<<<<<<< HEAD
  coords <- .recG[, c('lon', 'lat', 'geoID', 'country', 'stateProvince', 'county')]
=======
  coords <- .recG[, c('lon', 'lat', 'geoID', 'country', 'adm1', 'adm2')]
>>>>>>> origin/master
  coordinates(coords) =~ lon + lat
  coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  if(nrow(.recG) == 1){
    coords <- rbind(coords, coords)
  } 
  
  system.time(Extract <- raster::extract(mpios, coords)) # 3.9 vs.1.46
<<<<<<< HEAD
  Extract[, c('country', 'stateProvince', 'county')] <- .recG[match(Extract$point.ID, .recG$geoID), c('country', 'stateProvince', 'county')]
=======
  Extract[, c('country', 'adm1', 'adm2')] <- .recG[match(Extract$point.ID, .recG$geoID), c('country', 'adm1', 'adm2')]
>>>>>>> origin/master
  
  if(nrow(.recG) == 1){
    Extract <- Extract[Extract$point.ID == 1, ]
  } 
  
  
  # Extract0 <- Extract
  # Extract <- Extract0[1: 50, ]
  
  Split <- base::split(Extract, Extract$point.ID)
  #length(Split)
  #x0 <- Split[[5]]
  
  listVals <- lapply(Split, function(x0){
    #x0 <- Split[[1]]
    {
    x0$geoID <- 1:nrow(x0)
    
    cou0 <- corroboracion(overPointsField = x0$PAIS, dataField = x0$country, 
                          dataID = x0$geoID, maxchar = .scriptMaxchar)
    cou1 <- corroboracion(overPointsField = x0$ISO2, dataField = x0$country, 
                          dataID = x0$geoID, maxchar = .scriptMaxchar)
    cou2 <- corroboracion(overPointsField = x0$ISO3, dataField = x0$country, 
                          dataID = x0$geoID, maxchar = .scriptMaxchar)
    
    couSumm <- unique(c(cou0[[1]], cou1[[1]], cou2[[1]]))
    suggestedCountry <- x0$PAIS[which.max(x0$date)]
    sourceCountry <- x0$layer[which.max(x0$date[couSumm])]
    if(!any(couSumm)) {
      couSumm <- NA
      sourceCountry <- NA
    }
    
    if(!any(length(suggestedCountry))) {
      suggestedCountry <- NA
    }
    
    ##
<<<<<<< HEAD
    dept0 <- corroboracion(overPointsField = x0$DPTOS, dataField = x0$stateProvince, 
=======
    dept0 <- corroboracion(overPointsField = x0$DPTOS, dataField = x0$adm1, 
>>>>>>> origin/master
                           dataID = x0$geoID, maxchar = .scriptMaxchar)
    posDept <- which( x0$date == max(x0$date[dept0[[1]]]))
    suggestedStateProvince <- x0$DPTOS[which.max(x0$date)]
    sourceStateProvince <- x0$layer[posDept]
    
    if(!any(posDept)) {
      posDept <- NA
      sourceStateProvince <- NA
    }
    if(!any(length(suggestedStateProvince))) {
      suggestedStateProvince <- NA
    }
    
    
    
    ##
<<<<<<< HEAD
    mun0 <- corroboracion(overPointsField = x0$MPIOS, dataField = x0$county, 
=======
    mun0 <- corroboracion(overPointsField = x0$MPIOS, dataField = x0$adm2, 
>>>>>>> origin/master
                          dataID = x0$geoID, maxchar = .scriptMaxchar)
    posMun <- which( x0$date == max(x0$date[mun0[[1]]]))
    suggestedCounty <- x0$MPIOS[which.max(x0$date)]
    sourceCounty <- x0$layer[posMun]
    if(!any(posMun)) {
      posMun <- NA
      sourceCounty <- NA
    }
    if(!any(length(suggestedCounty))) {
      suggestedCounty <- NA
    }
    
    
    
    (ans <- data.frame(correctCountry = any(na.omit(couSumm)),
                       suggestedCountry = unique(na.omit(cou0[[3]]))[1],
                       sourceCountry = sourceCountry,
                       
                       correctStateProvince = any(dept0[[1]]),
                       suggestedStateProvince = suggestedStateProvince,
                       sourceStateProvince =  sourceStateProvince,
                       
                       correctCounty = any(mun0[[1]]),
                       suggestedCounty = suggestedCounty,
                       sourceCounty = sourceCounty,
                       
                       stringsAsFactors = FALSE)[1, ])
    }
    
  })
  
  dfVals <- as.data.frame(do.call(rbind, listVals))
  dfVals$sourceLayer <- paste0(dfVals$sourceCountry, ' - ', dfVals$sourceStateProvince, ' - ',
                               dfVals$sourceCounty)
  dfVals$sourceCountry <- dfVals$sourceStateProvince <- dfVals$sourceCounty <- NULL
  
  .recG <- cbind(.recG, dfVals)
  .recG$geoID <- NULL
  return(.recG)
} 



corroboracion <- function(overPointsField, dataField, dataID, maxchar = 0.2){
  
  targetLayerField <- overPointsField
  lenLayerField <- length(which(!is.na(targetLayerField)))
  lenTableLocalities <- length(which(!is.na(dataField)))
  
  if (lenTableLocalities == 0 | lenLayerField == 0){
    correctRows <- 0
  } else if(lenTableLocalities == 1){
    tmp <- stringMatch(targetLayerField, dataField, maxChar = maxchar)
    correctRows <- ifelse(length(tmp) == 0, 0, dataID[tmp])
  } else if(lenTableLocalities > 1){
    eqLayerNTable <- which(gsub('[[:blank:]]', '', tolower(targetLayerField)) == gsub('[[:blank:]]', '', tolower(dataField))) ## datos con igual entidad # jmx
    difLayerNTable <- which(gsub('[[:blank:]]', '', tolower(targetLayerField)) != gsub('[[:blank:]]', '', tolower(dataField))) ## datos con diferente entidad # imx
    
    na.Rows <- which(is.na(targetLayerField))
    id.eqRows <- dataID[eqLayerNTable] # ID's de las filas exactas # id.exa
    
    CompareMun <- data.frame(posDifRows = difLayerNTable, layerValue = targetLayerField[difLayerNTable], 
                             tableValue = as.character(dataField[difLayerNTable]), tableID = dataID[difLayerNTable], stringsAsFactors = FALSE)
    
    uniqueLayerMun <- sort(unique(CompareMun$layerValue)) # Saco valores unicos por municipio reportados en la capa
    (uniqueLayerMun <- uniqueLayerMun[which(!is.na(uniqueLayerMun) & uniqueLayerMun != '')])
    
    mmx <- c(0, 0)
    nmx <- c(0, 0)
    
    if (length(uniqueLayerMun) > 0){
      validRows <- NULL
      for (i in 1:length(uniqueLayerMun)){
        (pos.mun <- which(CompareMun$layerValue == uniqueLayerMun[i])) # Selecciono posiciones que contienen al municipio[i]
        if (length(pos.mun)> 0){
          
          # Sel. municipios de tabla para el municipio de tabla
          (mun.i <- CompareMun$tableValue[pos.mun])
          
          # Comparo similitud entre municipios reportados en tabla y extraidos con coordenada
          tmp <- stringMatch(pattern =  gsub('[[:blank:]]', '', uniqueLayerMun[i]), x = gsub('[[:blank:]]', '', mun.i), maxChar = maxchar)
          #(tmp <- agrep(gsub(" ", "", uniqueLayerMun[i]), gsub(" ", "", mun.i), max = 2, value = FALSE, ignore.case = TRUE))
          
          # Genero tabla con los ID de las filas bien
          (validRows <- c(validRows, CompareMun$tableID[pos.mun][tmp]))
        }
      }
      correctRows <- sort(as.integer(c(id.eqRows, validRows))) # Filas de la tabla con datos validados positivamente
      
    } else if (length(id.eqRows) > 0 & length(uniqueLayerMun) == 0){
      correctRows <- sort(as.integer(c(id.eqRows))) 
    } else {
      correctRows <- 0
    }
  }
  
  output <- list()
  output[[1]] <- correctRows
  output[[2]] <- 0
  output[[3]] <- targetLayerField
  return(output)
}

stringMatch <- function(pattern, x, maxChar){
  # textB <- 'SataTeresa'; textA <- 'Santa Tereza'
  # maxChar <- 'A0.33'; maxChar <- 'B0.8'; maxChar <- 2  
  isNumMaxChar <- as.numeric(maxChar)
  if (is.numeric(isNumMaxChar) & !is.na(isNumMaxChar)){
    nChar <- as.numeric(maxChar)
  } else if(length(grep('p', maxChar)) > 0) {
    porpA <- as.numeric(gsub('p', '', maxChar))
    nChar <- porpA * nchar(pattern)[1]
    
  } else if(length(grep('x', maxChar)) > 0) {
    porpB <- as.numeric(gsub('x', '', maxChar))
    nChar <- porpB * nchar(x)[1]
  }
  return(agrep(pattern = pattern, x = x, max = nChar, value = FALSE, ignore.case = TRUE))
}

nameValidation2 <- function(con, inTable){  
  tablaTrabajoNames <- c("id", "Nombre", "genero", "epiteto_especifico", "es_aceptadoCoL", 
                         "id_nombre_CoL", "id_nombre_aceptado",   
                         "nombre_aceptado", "autor_nombre_aceptado", 
                         "reino_CoL", "phylum_CoL", "clase_CoL", "orden_CoL", 
                         "familia_CoL", "genero_CoL", "epiteto_CoL",'dbSource')
  
  tabla_trabajo <- data.frame(matrix('', nrow = 0, ncol = length(tablaTrabajoNames)))
  colnames(tabla_trabajo) <- tablaTrabajoNames
  dbWriteTable(con,"tabladetrabajo", value = tabla_trabajo, overwrite = TRUE)  
  
  inTable <- data.frame(inTable)
  inTable$id <- as.numeric(as.character(inTable$id))
  
  inTable$nombre <- as.character(inTable$nombre)
  uniqueTable <- inTable[!duplicated(as.character(inTable$nombre)), ] # dim(uniqueTable)
  
  dbSendQuery(con,"truncate tabladetrabajo")
  dbWriteTable(con,"gbif_sib", data.frame(uniqueTable), overwrite=TRUE)
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"NA\"")
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"\"")
  dbSendQuery(con,"update gbif_sib set genero = null where genero =\"\"")
  dbSendQuery(con,"delete from gbif_sib where id = \"\"")
  dbSendQuery(con,"INSERT INTO tabladetrabajo (row_names, id,nombre,genero,epiteto_especifico) select row_names,id,nombre,genero,epiteto_especifico from gbif_sib")
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.genero = _search_scientific.genus) AND (tabladetrabajo.epiteto_especifico = _search_scientific.species) SET 
              tabladetrabajo.es_aceptadoCoL = _search_scientific.status, 
              tabladetrabajo.id_nombre_CoL = _search_scientific.id, 
              tabladetrabajo.id_nombre_aceptado = _search_scientific.id
              WHERE ((( _search_scientific.infraspecies)=''))")
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN synonym ON (tabladetrabajo.id_nombre_CoL = synonym.id) SET
              tabladetrabajo.id_nombre_aceptado = synonym.taxon_id 
              WHERE tabladetrabajo.es_aceptadoCoL between 2 and 5")
  
  
  ## Genus for non acepted species
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.genero = _search_scientific.genus) SET
              tabladetrabajo.reino_CoL = _search_scientific.kingdom,
              tabladetrabajo.phylum_CoL = _search_scientific.phylum,
              tabladetrabajo.clase_CoL = _search_scientific.class,
              tabladetrabajo.orden_CoL = _search_scientific.order,
              tabladetrabajo.familia_CoL = _search_scientific.family,
              tabladetrabajo.epiteto_CoL = _search_scientific.species,
              tabladetrabajo.genero_CoL = _search_scientific.genus")
  ###
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.id_nombre_aceptado = _search_scientific.id) SET
              tabladetrabajo.epiteto_CoL = _search_scientific.species,
              tabladetrabajo.autor_nombre_aceptado = _search_scientific.author,
              tabladetrabajo.dbSource = _search_scientific.source_database_name")
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.id_nombre_aceptado = _search_scientific.id) SET
              tabladetrabajo.dbSource = _search_scientific.source_database_name")
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN scientific_name_status ON (tabladetrabajo.es_aceptadoCoL = scientific_name_status.id) SET
              tabladetrabajo.es_aceptadoCoL = scientific_name_status.name_status") 
  
  
  
  
  outTable <- dbGetQuery(con, "select * from tabladetrabajo")[, -c(1:2)]
  pos <- !is.na(outTable$es_aceptadoCoL)
  
  outTable$nombre_aceptado[pos] <- paste(outTable$genero_CoL[pos], outTable$epiteto_CoL[pos])
  colnames(outTable) <- gsub('Nombre', 'nombre', colnames(outTable))
  nombre <- data.frame(id = inTable[, 1], nombre = inTable[, 2])
  
  finalTable <- merge(nombre, outTable, all.x = T, by = 'nombre')
  finalTable <- finalTable[order(finalTable$id), ]
  pos.id <- grep('^id$', colnames(finalTable))
  finalTable <- cbind(id = finalTable$id, finalTable[, -c(pos.id)])
  rownames(finalTable) <- finalTable$id
  colnames(finalTable) <- c('ID', 'originalNameUsage', 'originalGenus', 'originalSpecificEpithet', 'taxonomicStatus', 'originalScientificNameID', 
                            'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship', 'kingdom', 'phylum', 'class', 'order', 'family', 
                            'genus', 'specificEpithet', 'nameAccordingTo')
  return(finalTable)
}

nameValidationGenus <- function(con, inTable){  
  tablaTrabajoNames <- c("id", "Nombre", "genero", "epiteto_especifico", "es_aceptadoCoL", 
                         "id_nombre_CoL", "id_nombre_aceptado",   
                         "nombre_aceptado", "autor_nombre_aceptado", 
                         "reino_CoL", "phylum_CoL", "clase_CoL", "orden_CoL", "familia_CoL", "genero_CoL", "epiteto_CoL", 'dbSource')
  
  tabla_trabajo <- data.frame(matrix('', nrow = 0, ncol = length(tablaTrabajoNames)))
  colnames(tabla_trabajo) <- tablaTrabajoNames
  dbWriteTable(con,"tabladetrabajo", value = tabla_trabajo, overwrite = TRUE)  
  
  inTable <- data.frame(inTable)
  inTable$id <- as.numeric(as.character(inTable$id))
  
  inTable$nombre <- as.character(inTable$nombre)
  uniqueTable <- inTable[!duplicated(as.character(inTable$nombre)), ] # dim(uniqueTable)
  
  dbSendQuery(con,"truncate tabladetrabajo")
  dbWriteTable(con,"gbif_sib", data.frame(uniqueTable), overwrite=TRUE)
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"NA\"")
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"\"")
  dbSendQuery(con,"update gbif_sib set genero = null where genero =\"\"")
  dbSendQuery(con,"delete from gbif_sib where id = \"\"")
  dbSendQuery(con,"INSERT INTO tabladetrabajo (row_names, id,nombre,genero) select row_names,id,nombre,genero from gbif_sib")
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.genero = _search_scientific.genus) SET
              
              tabladetrabajo.reino_CoL = _search_scientific.kingdom,
              tabladetrabajo.phylum_CoL = _search_scientific.phylum,
              tabladetrabajo.clase_CoL = _search_scientific.class,
              tabladetrabajo.orden_CoL = _search_scientific.order,
              tabladetrabajo.familia_CoL = _search_scientific.family,
              tabladetrabajo.genero_CoL = _search_scientific.genus,
              tabladetrabajo.dbSource = _search_scientific.source_database_name")
  
  outTable <- dbGetQuery(con, "select * from tabladetrabajo")[, -c(1:2)]
  pos <- !is.na(outTable$es_aceptadoCoL)
  
  outTable$nombre_aceptado[pos] <- paste(outTable$genero_CoL[pos], outTable$epiteto_CoL[pos])
  colnames(outTable) <- gsub('Nombre', 'nombre', colnames(outTable))
  nombre <- data.frame(id = inTable[, 1], nombre = inTable[, 2])
  
  finalTable <- merge(nombre, outTable, all.x = T, by = 'nombre')
  finalTable <- finalTable[order(finalTable$id), ]
  pos.id <- grep('^id$', colnames(finalTable))
  finalTable <- cbind(id = finalTable$id, finalTable[, -c(pos.id)])
  rownames(finalTable) <- finalTable$id
  colnames(finalTable) <- c('ID', 'originalNameUsage', 'originalGenus', 'originalSpecificEpithet', 'taxonomicStatus', 'originalScientificNameID', 
                            'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship', 'kingdom', 'phylum', 'class', 'order', 'family', 
                            'genus', 'specificEpithet', 'nameAccordingTo')
  return(finalTable)
}

splitSciName <- function(spNames){
  #spNames <- uniqueAcepN
  cofTable <- t(sapply(spNames, USE.NAMES = FALSE, FUN = function(y){
    t(c(y, strsplit(y, ' ')[[1]][1:2]))
  }))
  
  id <- 1:nrow(cofTable)
  cofTable <- data.frame(id, "nombre" = cofTable[, 1], "genero" = cofTable[, 2], 
                         "epiteto_especifico" = cofTable[, 3],
                         row.names = id, stringsAsFactors = FALSE)
  return(cofTable)
}

genusValidation <- function(con,inTable){  
  tablaTrabajoNames <- c("id", "Nombre", "genero", "epiteto_especifico", "es_aceptadoCoL", 
                         "id_nombre_CoL", "id_nombre_aceptado",   
                         "nombre_aceptado", "autor_nombre_aceptado", 
                         "reino_CoL", "phylum_CoL", "clase_CoL", "orden_CoL", "familia_CoL", "genero_CoL", "epiteto_CoL", 'dbSource')
  
  tabla_trabajo <- data.frame(matrix('', nrow = 0, ncol = length(tablaTrabajoNames)))
  colnames(tabla_trabajo) <- tablaTrabajoNames
  dbWriteTable(con,"tabladetrabajo", value = tabla_trabajo, overwrite = TRUE)  
  
  inTable <- data.frame(inTable)
  inTable$id <- as.numeric(as.character(inTable$id))
  
  inTable$nombre <- as.character(inTable$nombre)
  uniqueTable <- inTable[!duplicated(as.character(inTable$nombre)), ] # dim(uniqueTable)
  
  dbSendQuery(con,"truncate tabladetrabajo")
  dbWriteTable(con,"gbif_sib", data.frame(uniqueTable), overwrite=TRUE)
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"NA\"")
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"\"")
  dbSendQuery(con,"update gbif_sib set genero = null where genero =\"\"")
  dbSendQuery(con,"delete from gbif_sib where id = \"\"")
  dbSendQuery(con,"INSERT INTO tabladetrabajo (row_names, id,nombre,genero) select row_names,id,nombre,genero from gbif_sib")
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.genero = _search_scientific.genus) SET
              
              tabladetrabajo.reino_CoL = _search_scientific.kingdom,
              tabladetrabajo.phylum_CoL = _search_scientific.phylum,
              tabladetrabajo.clase_CoL = _search_scientific.class,
              tabladetrabajo.orden_CoL = _search_scientific.order,
              tabladetrabajo.familia_CoL = _search_scientific.family,
              tabladetrabajo.genero_CoL = _search_scientific.genus,
              tabladetrabajo.dbSource = _search_scientific.source_database_name")
  
  outTable <- dbGetQuery(con, "select * from tabladetrabajo")[, -c(1:2)]
  pos <- !is.na(outTable$es_aceptadoCoL)
  
  outTable$nombre_aceptado[pos] <- paste(outTable$genero_CoL[pos], outTable$epiteto_CoL[pos])
  colnames(outTable) <- gsub('Nombre', 'nombre', colnames(outTable))
  nombre <- data.frame(id = inTable[, 1], nombre = inTable[, 2])
  
  finalTable <- merge(nombre, outTable, all.x = T, by = 'nombre')
  finalTable <- finalTable[order(finalTable$id), ]
  pos.id <- grep('^id$', colnames(finalTable))
  finalTable <- cbind(id = finalTable$id, finalTable[, -c(pos.id)])
  rownames(finalTable) <- finalTable$id
  colnames(finalTable) <- c('ID', 'originalNameUsage', 'originalGenus', 'originalSpecificEpithet', 'taxonomicStatus', 'originalScientificNameID', 
                            'scientificNameID', 'acceptedNameUsage', 'scientificNameAuthorship', 'kingdom', 'phylum', 'class', 'order', 'family', 
                            'genus', 'specificEpithet', 'nameAccordingTo')
  #   finalTable$taxonomicStatus <- gsub(1, 'Accepted name', 
  #                                      gsub(2, 'Ambiguous synonym', 
  #                                           gsub(3, 'Misapplied name',
  #                                                gsub(4, 'Provisionally accepted name',
  #                                                     gsub(5, 'Synonym',  finalTable$taxonomicStatus)))))
  
<<<<<<< HEAD
  return(finalTable)
=======
  return(finalTabl||e)
>>>>>>> origin/master
}

cleanSciNames <- function(y){ 
  require(R.utils)
  if (!require(R.utils)) {
    stop("You need to install the 'R.utils' package to use this function")
  }
  (y <- gsub("\n"," ",y))
  (y <- gsub("'","", y))
  (y <- gsub('"','', y))
  (y <- gsub('[[:space:]][[:space:]]|[[:space:]][[:space:]][[:space:]]',' ', y))
  (y <- gsub('[[:space:]][[:space:]]|[[:space:]][[:space:]][[:space:]]',' ', y))
  
  (y <- gsub("^[[:space:]][[:space:]]|^[[:space:]]|[[:space:]][[:space:]]$|[[:space:]]$","", y))
  (y <- gsub("^[[:space:]][[:space:]]|^[[:space:]]|[[:space:]][[:space:]]$|[[:space:]]$","", y))
  
  y <- sapply(y, FUN = function(x){
    # x <- y
    spaces <- gregexpr(" ", x)[[1]]
    if (gregexpr("idae ", x)[[1]][1] != -1 & length(spaces) >= 2){
      famPos <- gregexpr("idae ", x)[[1]]
      x <- substr(x, famPos[1] + attr(famPos,"match.length"), nchar(x))
    }
    
    if (gregexpr("aceae ", x)[[1]][1] != -1 & length(spaces) >= 2){
      famPos <- gregexpr("aceae ", x)[[1]]
      x <- substr(x, famPos[1] + attr(famPos,"match.length"), nchar(x))
    }
    
    if (gregexpr("\\).*", x)[[1]][1] != -1){
      ip <- gregexpr("\\(.*",x)[[1]][1]
      fp <- gregexpr("\\).*",x)[[1]][1]
      x1 <- substr(x, 0, ip - 2)
      x2 <- substr(x, fp + 1, nchar(x))
      x <- paste0(x1, x2)
    }
    
    (x <- gsub(" cf\\.| cf "," ",x))
    (x <- gsub (" var\\.| var "," ",x))
    (x <- gsub(" sp\\.| sp\\.| sp | spp | spp\\.| spp$"," ", x))
    (x <- gsub(" aff\\. | affin\\. | aff | affin "," ", x))
    (x <- gsub("  "," ", x))
    (x <- gsub("  "," ", x))
    
    if (!is.na(gregexpr(" ", x)[[1]][2])){
      pos <- gregexpr(" ", x)[[1]][2]
      (x <- substr(x, 0, pos - 1))
    }
    return(capitalize(tolower(x)))
  })
  
  (y <- gsub('"|\\*|\\.', '', y))
  (y <- gsub('-', 'XYZ', y))
  (y <- gsub('[[:punct:]]| $|  $|^ |^  ', '', y))
  (y <- as.character(gsub('XYZ', '-', y)))
  return(y)
}

compileManualTaxValidation <- function(manualTaxValidationPath, .pattern = 'fill'){
  pendSpeTax <- list.files(path = manualTaxValidationPath, full.names = TRUE, pattern = .pattern)
  if(!length(pendSpeTax)){
    cat('No taxonomic pendings! Check errors')
  } else {
    
    pendTax <- NULL
    for (pt in 1:length(pendSpeTax)){
      tmpTax <- read.csv(pendSpeTax[pt],as.is = TRUE)
      pendTax <- rbind(pendTax, tmpTax)
    }
    return(pendTax)
  }
}

compileErrors <- function(tempDirRecordsPath, .pattern = '.csv'){
  # tempDirRecordsPath = .tempTaxValidation; .pattern = '.csv'
  pendRecErr <- list.files(path = tempDirRecordsPath, full.names = TRUE, pattern = .pattern)
  if(!length(pendRecErr)) {
    cat('No error pendings! Check errors')
  }   else { 
    pendErr <- NULL
    for (pt in 1:length(pendRecErr)){
      tmpRec <- read.csv(pendRecErr[pt],as.is = TRUE)
      pendErr <- rbind(pendErr, tmpRec)
    }
    return(pendErr)
  }
}

checkErrors <- function(x){
  ifelse(length(sapply(x, list.files)) == 0,
         'Complete and total success', '): Check ... some errors')
}

updateFlags <- function(.acceptedNameUsage = 'NA',
                        .flags = c('altitudinalOutlier', 'consistentAltitude', 
                                   'diferenceInAltitude', 'environmentalOutlier')){
  # .acceptedNameUsage = 'Zamia tolimensis'; .acceptedNameUsage = 'Atelopus muisca'
  
  mT <- mongoConectRec$find(paste0('{"species":"', .acceptedNameUsage, '"}'),
<<<<<<< HEAD
                            fields = '{"_id" : 1, "lat" : 1, "lon" : 1, "demAltitude": 1, "interpretedElevation":1, "verbatimElevation": 1}')
=======
                            fields = '{"_id" : 1, "lat" : 1, "lon" : 1, "demAltitude": 1, "interpretedElevation":1, "alt": 1}')
>>>>>>> origin/master
  mT <- mT[which(!is.na(mT$lat) & !is.na(mT$lon)), ]
  if(nrow(mT) > 1){
    coordSpatial <- SpatialPoints(mT[, c('lon', 'lat')], proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    if('altitudinalOutlier' %in% .flags){
      mT$demAltitude <- raster::extract(demAoi, coordSpatial)
      # pos9 <- which(mT$demAltitude == -9999
      # if (any(pos9)){
      #   mT$demAltitude[pos9] <- raster::extract(demAoi, coordSpatial[pos9, ])
      #   .flags <- c(.flags, 'demAltitude')
      # }
      mT$altitudinalOutlier <- rowSums(outliersIG(rid = 1:nrow(mT), species = rep('SP', nrow(mT)), dups = rep(0, nrow(mT)), ev = as.numeric(mT$demAltitude))) > 0
    }
    
    
    if('consistentAltitude' %in% .flags){
      mT$consistentAltitude <- 'NULL'
    }
    
    if('diferenceInAltitude' %in% .flags){
<<<<<<< HEAD
      mT$diferenceInAltitude <- abs(as.numeric(mT$demAltitude) - as.numeric(mT$verbatimElevation))
=======
      mT$diferenceInAltitude <- abs(as.numeric(mT$demAltitude) - as.numeric(mT$alt))
>>>>>>> origin/master
      mT$diferenceInAltitude[is.na(mT$diferenceInAltitude)] <- 'NULL'
    }
    
    if('environmentalOutlier' %in% .flags){
      mT$environmentalOutlier <- FALSE
      extVals <- raster::extract(layNE, coordSpatial)
      if(nrow(extVals) > 1){
        
        envOut <- apply(extVals, 2, FUN = function(x) {
          rowSums(outliersIG(rid = 1:nrow(mT), species = rep('SP', nrow(mT)), dups = rep(0, nrow(mT)), ev = x))
        })
        mT$environmentalOutlier <- rowSums(envOut) > 0
      }
    }
    
    # Loop for upload records one-by-one
    for(m in 1:nrow(mT)){
      oid <- paste0(mT$`_id`[[m]], collapse = '')
      upQ <- paste0('"', .flags, '":"', mT[m, c(.flags)], '"', collapse = ', ')
      
      mongoConectRec$update(query = paste0('{"_id":{"$oid":"',oid,'"}}'), 
                            update = paste0('{"$set":{',upQ,'}}'), 
                            upsert = TRUE)
      #mongoConectRec$find(query = paste0('{"_id":{"$oid":"',oid,'"}}'))
    }
  }
}