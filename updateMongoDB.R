# # uncomment 544 for taxize function
# 
# Leave the 3-19 lines commented!
# library(rgdal); library(raster); library(RMySQL); library(rgeos); library(mongolite); library(httr); library(lubridate)
# conMSQ <<- dbConnect(dbDriver("MySQL"), user = "root", password = "root", dbname = "col2015ac", host = "localhost")
# mongoConectRec <<- mongo(db = "records", collection = "records", url ="mongodb://192.168.11.81:27017", verbose = FALSE)
# mongoConectSpe <<- mongo(db = "records", collection = "species", url ="mongodb://192.168.11.81:27017", verbose = FALSE)
# iucnShapes <- read.csv('D:/dbDownload/nwDB/spp&TaxonomyCol2015.csv', as.is = TRUE)
# iucnShapesPath <- 'D:/dbDownload/nwDB/bySppClipCol_UTM'
# load('D:/dbDownload/CIT_IUCN_END_INV_tables.RData')
# load('D:/dbDownload/climaticNE.RData')
# prjUTM <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# load('D:/dbDownload/verifGeoMongo.RData')
# load('D:/dbDownload/nwDB/TAXnwDb.RData')
# id <<- raster('D:/dbDownload/nwDB/aoiId.tif')
# aoi <- readOGR('D:/dbDownload/nwDB', 'aoi', verbose = FALSE)
# tstdat <- read.csv('D:/dbDownload/nwDB/uploadsPath/Zamias2017/Zamias2017.csv', as.is = TRUE)
# record <- tstdat[1, ]
# source('D:/dbDownload/nwDB/updateMongoDB.R')

## Functions -----
updateMongoDB <- function(record, aoi, .PrivateData = 0, onlySp = FALSE,
                          .tempTaxValidation, .tempDirRecords, .dupDirRecords, .update = TRUE){
  # .PrivateData = 0; .update = TRUE; onlySp = TRUE
  # rec <- .recT <- origRec <- record
  # rec <- .recT <- origRec <- record <- data.frame(species = 'Ateles hybridus', acceptedNameUsage = 'Ateles hybridus', country = 'CO', lat = 4, lon = -70,stringsAsFactors=F)
  ## Take the original record and structure it using some parameters
  formRecord <- formatRecord(rec = record, aoi, .privateData = .PrivateData, fr.onlySp = onlySp)

  ## Check if species is not accepted and save it and the record in a temporal folder
  if (is.na(formRecord$acceptedNameUsage) | formRecord$acceptedNameUsage == ''){
    currSpe2Validate <- list.files(path = .tempDirManualTaxValidation, pattern = '.csv')
    csvFiles <- gsub('.csv', '', currSpe2Validate)
    if(!formRecord$species %in% csvFiles){
      taxRec <- formRecord[, c('species', 'speciesOriginal', 'kingdom', 'phylum', 
                               'class', 'order', 'family', 'acceptedNameUsage')]
      write.csv(taxRec, paste0(.tempDirManualTaxValidation, '/', formRecord$species,'.csv'), row.names = FALSE, na = '')
    }
    write.csv(formRecord, paste0(.tempDirRecords, '/', formRecord$species,'-', basename(tempfile(formRecord$species)),'.csv'), row.names = FALSE, na = '')
    cat(formRecord$species, ' saved in temporal records and taxonimc folder ')
    
  ## If species exist then the record is uploaded into mongoDB
  } else {
    existsID <- specAlreadyInDb(rec = formRecord)[1]
    ## Is the species new in mongoDB? If not then add it
    if (is.logical(existsID) & existsID == FALSE){
      formRecord$taxID <- (mongoConectSpe$count() + 1)
      if (.update == TRUE){
        formRecord$consecutivoID <- (mongoConectSpe$count() + 1)
        updateSpeciesTable2(rec = formRecord)
        cat(formRecord$species, 'upload in species table')
      } else{
        cat(formRecord$species, 'upload in species table (FAKE - change to .update = TRUE)')
      }
    } else {
      formRecord$taxID <- existsID
    }
    if (is.data.frame(formRecord)){
        if(.update == TRUE & onlySp == FALSE){
          if(formRecord$dbDuplicate == FALSE) {
            formRecord$consecutivoID <- (mongoConectRec$count() + 1)
            updateRecordsTable2(rec = formRecord)
            cat(' - ', formRecord$species, 'upload in records table ')
          } else {
            write.csv(formRecord, paste0(.dupDirRecords, '/', formRecord$species,'-', basename(tempfile(formRecord$species)),'.csv'), row.names = FALSE, na = '')
            cat(formRecord$species, ' saved in temporal records and taxonimc folder ')
          }
      } else {
          cat(' - ', formRecord$species, 'upload in records table (FAKE - change to .update = TRUE)')  
      }
  }
  cat(' Success! \n')
}
}

## Insert record in species table
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
  mongoConectSpe$insert(speciesTable)
}

## Insert record in records table v2
updateRecordsTable2 <- function(rec){
  excCols <- c("taxonomicStatus", "scientificNameAuthorship",
               "specificEpithet", "nameAccordingTo", "TaxVerifSource",
               "kingdom", "phylum", "class", "order", "family", "genus", 
                "sppInCol", "speciesInCountry", "validName", 
               "bmClass", "invasive", "endemic", "iucn", "cites", "migratoryType",
               "consecutivoID", "otherID", "withLat", "withLon", "recSizeBytes", "bigSizeRecord",
               "hasTaxDoubt", "privateDataset", "tmpTax", "tmpGeo")
  
  recordsTable <- rec[,  setdiff(colnames(rec), excCols)]
  recordsTable[1, is.na(recordsTable[1, ]) | recordsTable[1, ] == ''] <- 'NULL'
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
  
  mongoQ <- paste0('{"$or":[{"species":"', rec$species, '"}, {"acceptedNameUsage":"', rec$species,
                   '"}, {"species":"', rec$acceptedNameUsage, '", "acceptedNameUsage":"', rec$acceptedNameUsage, '"}]}')
  
  
  mongoCount <- mongoConectSpe$count(mongoQ)
  
  if (mongoCount == 0){
    return(FALSE)
  } else if (mongoCount != 0){
    return(mongoConectSpe$find(mongoQ)$taxID)
  }
}

## Format the original record
formatRecord <- function(rec, aoi, .privateData = .privateData, .override = FALSE, .url = '', fr.onlySp = FALSE,
                         .visualizationPrivileges = 0, .privateDataset = 0,
                         .tmpTax = 1, .tmpGeo = 1, .contributedRecord = FALSE){
  
  #.privateData = 0; .override = 0; .url = ''; .visualizationPrivileges = 0; .privateDataset = 0; .tmpTax = 1; .tmpGeo = 1; .contributedRecord = FALSE
  # fr.onlySp = TRUE
  # rec <- .recT <- origRec <- record
  
  # Taxonomic and requierd Cols for script. NA is given if is absent
  taxCols <- c('kingdom', 'phylum', 'class', 'order', 'family')
  
  reqCols <- c("source", "occurrenceID", "species", "speciesOriginal", "continent", 
               "country", "adm1", "adm2", "locality","lat",
               "lon","coordUncertaintyM", "alt","institution", "collection",
               "catalogNumber", "basisOfRecord", "collector", "earliestDateCollected",
               "latestDateCollected", "downloadDate", "resourceName", "resourceFolder","resourceIncorporationDate",
               "privateData", "otherID")
  
  
  ## Create new formated record
  if (is.null(rec$speciesOriginal)) rec$speciesOriginal <- rec$species
  nwRec <- nwRecFun(.rec = rec, .reqCols = reqCols)
  nwRec[, c('lat', 'lon')] <- sapply(nwRec[, c('lat', 'lon')], as.numeric)
  
  # 1 Incomplet coords ----
  if(is.na(nwRec$lat) | is.na(nwRec$lon)) {
    message('Incomplete coordinates!')
    nwRec$lon <- nwRec$lat <- 0
    stop('Incomplete coordinates!')
  } else {
  
    # 2 Coords in aoi ----
    if(!(nwRec$lon > extent(aoi)@xmin & nwRec$lon < extent(aoi)@xmax &
           nwRec$lat > extent(aoi)@ymin & nwRec$lat < extent(aoi)@ymax)){
      message('Coordinates outside region of interest!')
      stop('Coordinates outside region of interest!')
      
    }
  }
  
  # Taxonomic validation ----
  nwRec$species <- cleanSciNames(y = nwRec$species)
  nwRec <- taxValidation(.recT = nwRec, origRec = rec) # .recT = nwRec; origRec = rec
  
  if (fr.onlySp == FALSE ){
    
    #  Geographical validation ----
    #nwRec <- withCoords(.recG = nwRec)
    
    validCountry <- geoValidationCountry(.recG = nwRec)
    nwRec <- validCountry$nwRec
    
    validState <- geoValidationState(.recG = nwRec)
    nwRec <- validState$nwRec
    
    validCounty <- geoValidationCounty(.recG = nwRec)
    nwRec <- validCounty$nwRec
    
    nwRec$sourceLayer <- paste0(validCountry$sourceLayer, ' - ', validState$sourceLayer, ' - ',
                                validCounty$sourceLayer) #32
    
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
    if (nwRec$dbDuplicate == TRUE){
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
    nwRec$override <- .override
    
    # Visualization privileges in Biomodelos
    nwRec$visualizationPrivileges <- .visualizationPrivileges
    
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
    
    nwRec <- Use(.rec = nwRec)
  }
    
    nwRec <- sppCateg(.rec = nwRec)
  
    return(nwRec)
}

# <- function(.rec){
#   return(nwRec = .rec)
# }

sppCateg <- function(.rec){
  .rec$bmClass <- .rec$iucn <- .rec$cites <- ''
  .rec$invasive <-  .rec$endemic <- FALSE
  
  .rec$bmClass[grep('Mammalia', .rec$class)] <- 'Mamíferos'
  .rec$bmClass[grep('Amphibia', .rec$class)] <- 'Anfibios'
  .rec$bmClass[grep('Reptilia', .rec$class)] <- 'Reptiles'
  .rec$bmClass[grep('Aves', .rec$class)] <- 'Aves'
  .rec$bmClass[
    grep('Actinopterygii|Myxini|^Characiformes$|^Siluriformes$|^Perciformes$|^Gymnotiformes$|^Cyprinodontiformes$|^Clupeiformes$|^Myliobatiformes$|^Pleuronectiformes$|^Beloniformes$|^Batrachoidiformes$|^Osteoglossiformes$|^Lepidosireniformes$|^Synbranchiformes$|^Tetraodontiformes$',
         .rec$class)] <- 'Peces'
  .rec$bmClass[grep('Plantae', .rec$kingdom)] <- 'Plantas'
  .rec$bmClass[grep('Arthropoda|Mollusca', .rec$phylum)] <- 'Invertebrados'
  
  # Invasive
  .rec$invasive[.rec$acceptedNameUsage %in% inv] <- TRUE
  if (.rec$invasive == FALSE){
    .rec$invasive[.rec$species %in% inv] <- TRUE
  }
  
  .rec$endemic[.rec$acceptedNameUsage %in% end] <- TRUE
  if (.rec$endemic == FALSE){
    .rec$endemic[.rec$species %in% end] <- TRUE
  }
  
  iuPos <- any(.rec$acceptedNameUsage %in% iuc$acceptedNameUsage)
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
          .rec$iucn <- iuc$Red.List.status[iuc$sciName %in% .rec$species][1]
        }
      }
    }
  }
  
  citPos <- any(.rec$acceptedNameUsage %in% na.omit(cit$acceptedNameUsage))
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
          .rec$cites <- cit$CurrentListing[cit$FullName %in% .rec$species][1]
        }
      }
    }
  }
  .rec$migratoryType <- NA
  return(nwRec = .rec)
}


dates <- function(.rec){
  
  .rec$earliestDateCollected <- ifelse(.rec$earliestDateCollected == '', NA, .rec$earliestDateCollected)
  .rec$latestDateCollected <- ifelse(.rec$latestDateCollected == '', NA, .rec$latestDateCollected)
  
  nwDate <- na.omit(c(.rec$earliestDateCollected, .rec$latestDateCollected))[1]
  
  nwDate <- parse_date_time(nwDate, c("Y-m-d HMS", "dbY HMS", "dmyHMS", "BdY H", 'y', "Ymd", "mdY", 'dmY'))
  .rec$yyyy <- year(nwDate)
  .rec$mm <- month(nwDate)
  .rec$dd <- day(nwDate)
  
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

demAltitude <- function(.rec){
  .rec$demAltitude <- NA
  latOk <- ifelse(abs(.rec$lat) >  90 | is.na(.rec$lat) | .rec$lat == 0, 0, 1)
  lonOk <- ifelse(abs(.rec$lon) > 180 | is.na(.rec$lon) | .rec$lon == 0, 0, 1)
  
  if(latOk & lonOk){
    url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=",
                  .rec$lat, ',', .rec$lon, "&key=AIzaSyAz6sPnMLuIJrOWvNKnn6-PWYK56PDGa30")
    
    elev <- GET(url)
    
    elevChar <- as.character(elev)
    if(any(grep('You have exceeded your daily', elevChar))){
      .rec$demAltitude <- -9999
    } else{
      elev2 <- strsplit(elevChar, ' ')[[1]]
      elev3 <- elev2[grep('elev', elev2) + 2]
      finPos <- regexec(pattern = '\\,', elev3)[[1]]
      elev <- substr(elev3, 0, finPos - 1)    
      .rec$demAltitude <- as.numeric(elev)
    }
    
    return(nwRec = .rec)
  }
}

Use <- function(.rec){
  toUse <- which(.rec$withLat== 1 & .rec$withLon == 1,
                 .rec$sppInCol == 1 & .rec$speciesInCountry == TRUE &
                   .rec$tmpTax == 1 & .rec$tmpGeo == 1 &
                   .rec$bigSizeRecord == 0 &
                   .rec$dbDuplicate == FALSE & .rec$spatialDuplicated == FALSE & 
                   .rec$lowUncertainty == 0 &
                   .rec$hasLocality == TRUE &
                   .rec$hasTaxDoubt == 0 & 
                   .rec$override == FALSE )
  
  .rec$use <- ifelse(length(toUse) == 0, 0, 1)
  return (nwRec = .rec)
}

url <- function(.rec, origRec){
  origRec$url <- ifelse(is.null(origRec$url), NA, as.character(origRec$url))
  
  if (origRec$url == '' | is.na(origRec$url)){
    .rec$url <- NA
  } else {
    if(!any(grep('@', origRec$url)) ){
      .rec$url <- ifelse(GET(origRec$url)$status == 200, origRec$url, NA)  
    }
  }
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
  if(.rec$lat != 0 & .rec$lon != 0 & !is.na(.rec$lat) & !is.na(.rec$lon)){
  availableMap <- c(.rec$species, .rec$acceptedNameUsage) %in%
    unique(na.omit(c(iucnShapes$iucnShapes, iucnShapes$acceptedNameUsage)))
  if (any(availableMap)){
    spIucn <- which(iucnShapes$acceptedNameUsage %in% c(.rec$species, .rec$acceptedNameUsage))
    spIucn2 <- which(iucnShapes$iucnShapes %in% c(.rec$species, .rec$acceptedNameUsage))
    if (any(spIucn)){
      field2Load <- c(.rec$acceptedNameUsage, .rec$species) %in% iucnShapes$iucnShapes[spIucn]
      field2Load2 <- c(.rec$acceptedNameUsage, .rec$species)[field2Load][1]
      iucnShape <- readOGR(iucnShapesPath, field2Load2, verbose = FALSE)
    } else if(!any(spIucn) & any(spIucn2)) {
      field2Load <- c(.rec$acceptedNameUsage, .rec$species) %in% iucnShapes$acceptedNameUsage[spIucn]
      field2Load2 <- c(.rec$acceptedNameUsage, .rec$species)[field2Load][1]
      iucnShape <- readOGR(iucnShapesPath, field2Load2, verbose = FALSE)
    }
    coords <- .rec
    coordinates(coords) =~ lon + lat
    coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    coords <- spTransform(coords, CRSobj = CRS(prjUTM))
    if(iucnShape@proj4string@projargs != prjUTM){
      iucnShape <- spTransform(iucnShape, CRSobj = CRS(prjUTM))
    }
    over <- over(coords, iucnShape)
    if (is.na(over$id_no)){
      .rec$dist2KnowRange <- gDistance(iucnShape, coords)
      return(nwRec = .rec)
    } else if(!is.na(over$id_no)){
      .rec$insideKnownDistribution <- TRUE
      .rec$dist2KnowRange <- 0
      return(nwRec = .rec)
    }
  } else {
    return(nwRec = .rec)
  }
  } else {
    return(nwRec = .rec)
  }
}


spatialDuplicated <- function(.rec){
  
  ## Reemplazar 'speciesOriginal' por 'species'
  #mongoConectRec <- mongo(db = 'records', collection = 'records', url='mongodb://192.168.11.81:27017', verbose = FALSE)
  .rec$spatialDuplicated <- FALSE
  latOk <- ifelse(abs(.rec$lat > 180) | is.na(.rec$lat) | .rec$lat == 0, 0, 1)
  lonOk <- ifelse(abs(.rec$lon > 180) | is.na(.rec$lon) | .rec$lon == 0, 0, 1)
  
  if(latOk & lonOk){
    mongoQ <- ifelse(is.na(.rec$acceptedNameUsage),
                     paste0('{"$or":[{"species":"', .rec$species, '"}, {"acceptedNameUsage":"', .rec$species, '"}]}'),
                     paste0('{"$or":[{"species":"', .rec$species, '"}, {"acceptedNameUsage":"', .rec$species,
                            '"}, {"species":"', .rec$acceptedNameUsage, '", "acceptedNameUsage":"', .rec$acceptedNameUsage, '"}]}'))
    
    mongoTable <- mongoConectRec$find(mongoQ)
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
  latOk <- ifelse(abs(.rec$lat > 180) | is.na(.rec$lat) | .rec$lat == 0, 0, 1)
  lonOk <- ifelse(abs(.rec$lon > 180) | is.na(.rec$lon) | .rec$lon == 0, 0, 1)
  if(latOk & lonOk){      
    coords <- .rec
    coordinates(coords) =~ lon + lat
    .rec$cellID <- raster::extract(id, coords)
  }
  return(nwRec = .rec)
}

inUrbanArea <- function(.rec){
  .rec$inUrbanArea <- ifelse(is.na(.rec$suggestedMunicipality), FALSE, TRUE)
  return(nwRec = .rec)
}

hasLocality <- function(.rec){
  .rec$hasLocality <- ifelse(is.na(.rec$locality) | .rec$locality == '', FALSE, TRUE)
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
  sep <- grep('-|/', .rec$alt)
  if (any(sep)){
    sepAlts <- as.numeric(strsplit(.rec$alt, '-|/')[[1]])
  if(.rule == 'mean') .rec$interpretedElevation <- mean(sepAlts)
  if(.rule == 'max') .rec$interpretedElevation <- max(sepAlts)
  if(.rule == 'min') .rec$interpretedElevation <- min(sepAlts)
  } else {
    .rec$interpretedElevation <- as.numeric(gsub('[[:alpha:]]', '', .rec$alt))
  }
  return(nwRec = .rec)
}

uncertanity <- function(.rec, thresh = 5000){
  numUnc <- as.numeric(gsub('[[:alpha:]]', '', .rec$coordUncertaintyM))
  feet <- grep('f', .rec$coordUncertaintyM)
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
   
  dup <- paste0(.rec$acceptedNameUsage, '__', .rec$collection, '--', .rec$catalogNumber)
  naCases <- '^NA
  02__|^__|__NA--|__--|--$|--NA$'
  naValue <- grep(naCases, dup)
  if(!any(naValue)){
    
    mongoQ <- ifelse(is.na(.rec$acceptedNameUsage) | .rec$acceptedNameUsage == '',
                     paste0('{"species":"', .rec$species, '", "acceptedNameUsage":"', .rec$species, '"}'),
                     paste0('{"species":"', .rec$acceptedNameUsage, '", "acceptedNameUsage":"', .rec$acceptedNameUsage, '"}'))
    
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
  validMun <- grep('CO', .rec$sourceLayer)
  .rec$sppInCol[unique(c(sugg, validMun))] <- TRUE
  rm(sugg, validMun)
  
  # Desde el listado de especies (TAX de DINAVIS)
  if(.rec$sppInCol == FALSE){
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
      .rec$sppInCol[posSppTAX] <- TRUE
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
  .nw <- data.frame(matrix(rep(NA, length(.reqCols)), ncol = length(.reqCols)), stringsAsFactors = FALSE)
  colnames(.nw) <- .reqCols
  commCols <- base::intersect(colnames(.nw), colnames(.rec))
  .nw[, commCols] <- .rec[, commCols]
  return(.nw)
}

taxValidation <- function(.recT, origRec){
  #origRec$acceptedNameUsage <- .recT$species # for test
  #mongoConectSpe <- mongo(db = "records", collection = "species", url = "mongodb://192.168.11.81:27017", verbose = FALSE)
  
  mongoQ <- paste0('{"$or":[{"species":"', .recT$species, '"}, {"acceptedNameUsage":"', .recT$species, '"}]}')
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
    .recT$TaxVerifSource <- currentTax$TaxVerifSource
    
  } else if(mongoCount == 0){
   
    #taxCols <- c('kingdom', 'phylum', 'class', 'order', 'family')
    taxCols <- c('acceptedNameUsage')
    haveAllTaxFields <- all(taxCols %in% colnames(origRec))
    haveAllTaxFieldsFilled <- ifelse(haveAllTaxFields == TRUE, 
                                     all(origRec[, taxCols] != '' & !is.na(origRec[, taxCols])),
                                     FALSE)
    if(haveAllTaxFields & haveAllTaxFieldsFilled){
      
      set1 <- data.frame(id = 1, nombre = .recT$species,
                         genero = strsplit(.recT$species, ' ')[[1]][1], 
                         epiteto_especifico = strsplit(.recT$species, ' ')[[1]][2],
                         stringsAsFactors = FALSE)
      
      set1 <- nameValidationGenus(con = conMSQ, inTable = set1) # 1.98 sec
      set1$specificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      set1$originalNameUsage <- NULL
      .recT <- cbind(.recT, set1[, -c(1)])
      .recT$TaxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
#       .recT$originalGenus <- strsplit(.recT$species, ' ')[[1]][1]
#       .recT$originalSpecificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      .recT$originalSpecificEpithet <- strsplit(.recT$species, ' ')[[1]][2]
      .recT$taxonomicStatus <- 'pending'
      .recT$acceptedNameUsage <- origRec$acceptedNameUsage
#       .recT$originalScientificNameID <- .recT$scientificNameID <- ''
#       .recT$acceptedNameUsage <- .recT$species
#       .recT$scientificNameAuthorship <- ''
#       .recT$kingdom <- origRec$kingdom 
#       .recT$phylum <- origRec$phylum
#       .recT$class <- origRec$class
#       .recT$order <- origRec$order
#       .recT$family <- origRec$family
#       .recT$genus <- .recT$originalGenus
#       .recT$specificEpithet <- .recT$originalSpecificEpithet
      .recT$nameAccordingTo <- 'Record author'
      .recT$TaxVerifSource <- 'Record author'
    
     } else {
      
      set1 <- data.frame(id = 1, nombre = .recT$species,
                         genero = strsplit(.recT$species, ' ')[[1]][1], 
                         epiteto_especifico = strsplit(.recT$species, ' ')[[1]][2],
                         stringsAsFactors = FALSE)
      
      set1 <- nameValidation2(con = conMSQ, inTable = set1) # 1.98 sec
      
      set1$originalNameUsage <- NULL
      .recT <- cbind(.recT, set1[, -c(1)])
      .recT$TaxVerifSource <- ifelse(is.na(set1$acceptedNameUsage), NA, "Catalogue of Life 2015")
      
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
      #             .recT$TaxVerifSource <- dbs[db]
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
#         .recT$TaxVerifSource <- 'Record author'
#       }
      
      ## The plant list
      # taxVal <- TPL(.recT$species, corr=TRUE)
    }
  }
  return(.recT)
}

withCoords <- function(.recG){  
  .recG$withLat <- ifelse(is.na(.recG$lat) | .recG$lat == 0, 0, 1)
  .recG$withLon <- ifelse(is.na(.recG$lon) | .recG$lon == 0, 0, 1)
  return(.recG) 
}

geoValidationCountry <- function(.recG, .scriptMaxchar = .2){  
  
 validCou <- ifelse(.recG$country == '' |.recG$country == ' ' | is.na(.recG$country) , FALSE, TRUE)
  if (!(validCou)){
    .recG$correctCountry <- 0
    .recG$suggestedCountry <- NA
    return(list(nwRec = .recG, sourceLayer = NA))
  } else {
    coords <- .recG
    coordinates(coords) =~ lon + lat
    coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    over <- over(coords, paises)
    
    corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                            dataID = 1, maxchar = .scriptMaxchar)
    if (corPai[[1]] == 0){
      corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
    }
    
    sourceLayer <- 'southAmerica'
    suggCountry <- corPai[[3]]
    
    if (corPai[[1]] == 0){
      over <- over(coords, mpios2014)
      corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                              dataID = 1, maxchar = .scriptMaxchar)  
      if (corPai[[1]] == 0){
        corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
      }
      
      sourceLayer <- 'mpiosCol2014' 
      suggCountry <- corPai[[3]]
      
      if (corPai[[1]] == 0){
        over <- over(coords, mpios2011)
        corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                dataID = 1, maxchar = .scriptMaxchar)
        sourceLayer <- 'mpiosCol2011' 
        if (corPai[[1]] == 0){
          corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
        }
        
        if (corPai[[1]] == 0){
          over <- over(coords, mpios2003)
          corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                  dataID = 1, maxchar = .scriptMaxchar)
          sourceLayer <- 'mpiosCol2003' 
          if (corPai[[1]] == 0){
            corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
          }
          
          if (corPai[[1]] == 0){
            over <- over(coords, mpios1993)
            corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                    dataID = 1, maxchar = .scriptMaxchar)
            sourceLayer <- 'mpiosCol1993' 
            if (corPai[[1]] == 0){
              corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
            }
            
            if (corPai[[1]] == 0){
              over <- over(coords, mpios1985)
              corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                      dataID = 1, maxchar = .scriptMaxchar)
              sourceLayer <- 'mpiosCol1985' 
              if (corPai[[1]] == 0){
                corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
              }
              
              if (corPai[[1]] == 0){
                over <- over(coords, mpios1973)
                corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                        dataID = 1, maxchar = .scriptMaxchar)
                sourceLayer <- 'mpiosCol1973' 
                if (corPai[[1]] == 0){
                  corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
                }
                
                if (corPai[[1]] == 0){
                  over <- over(coords, mpios1964)
                  corPai <- corroboracion(overPointsField = over$PAIS, dataField = .recG$country, 
                                          dataID = 1, maxchar = .scriptMaxchar)
                  sourceLayer <- 'mpiosCol1964' 
                  if (corPai[[1]] == 0){
                    corPai[[1]] <- (.recG$country %in% c(over$ISO3, over$ISO2)) * 1
                  }
                  
                  if (corPai[[1]] == 0){
                    sourceLayer <- 'NA' 
                  }}}}}}
      }
    } 
    .recG$correctCountry <- as.logical(corPai[[1]])
    .recG$suggestedCountry <- suggCountry
    return(list(nwRec = .recG, sourceLayer = sourceLayer))
  } 
}

geoValidationState <- function(.recG, .scriptMaxchar = .2){
  validSta <- ifelse(.recG$adm1 == '' |.recG$adm1 == ' ' | is.na(.recG$adm1) , FALSE, TRUE)
  if (!(validSta)){
    .recG$correctStateProvince <- FALSE
    .recG$suggestedStateProvince <- NA
    return(list(nwRec = .recG, sourceLayer = NA))
  } else {
    coords <- .recG
    coordinates(coords) =~ lon + lat
    coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    over <- over(coords, paises)
    corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                            dataID = 1, maxchar = .scriptMaxchar)  
    sourceLayer <- 'southAmerica' 
    if (corDep[[1]] == 0){
      over <- over(coords, mpios2014)
      corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                              dataID = 1, maxchar = .scriptMaxchar)  
      sourceLayer <- 'mpiosCol2014' 
      
      if (corDep[[1]] == 0){
        over <- over(coords, mpios2011)
        corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                dataID = 1, maxchar = .scriptMaxchar)  
        sourceLayer <- 'mpiosCol2011' 
        
        if (corDep[[1]] == 0){
          over <- over(coords, mpios2003)
          corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                  dataID = 1, maxchar = .scriptMaxchar)  
          sourceLayer <- 'mpiosCol2003' 
          
          if (corDep[[1]] == 0){
            over <- over(coords, mpios1993)
            corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                    dataID = 1, maxchar = .scriptMaxchar)  
            sourceLayer <- 'mpiosCol1993' 
            
            if (corDep[[1]] == 0){
              over <- over(coords, mpios1985)
              corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                      dataID = 1, maxchar = .scriptMaxchar)  
              sourceLayer <- 'mpiosCol1985' 
              
              if (corDep[[1]] == 0){
                over <- over(coords, mpios1973)
                corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                        dataID = 1, maxchar = .scriptMaxchar)  
                sourceLayer <- 'mpiosCol1973' 
                
                if (corDep[[1]] == 0){
                  over <- over(coords, mpios1964)
                  corDep <- corroboracion(overPointsField = over$DPTOS, dataField = .recG$adm1, 
                                          dataID = 1, maxchar = .scriptMaxchar)  
                  sourceLayer <- 'mpiosCol1964' 
                  if (corDep[[1]] == 0){
                    sourceLayer <- 'NA' 
                  } }}}}}
      }
    }
    
    .recG$correctStateProvince <- as.logical(corDep[[1]])
    .recG$suggestedStateProvince <- corDep[[3]]
    return(list(nwRec = .recG, sourceLayer = sourceLayer))
  } 
}  

geoValidationCounty <- function(.recG, .scriptMaxchar = .2){
  validCon <- ifelse(.recG$adm2 == '' |.recG$adm2 == ' ' | is.na(.recG$adm2) , FALSE, TRUE)
  if (!(validCon)){
    .recG$correctCounty <- FALSE
    .recG$suggestedCounty <- NA
    return(list(nwRec = .recG, sourceLayer = NA))
  } else {
    coords <- .recG
    coordinates(coords) =~ lon + lat
    coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    over <- over(coords, paises)
    
    corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                            dataID = 1, maxchar = .scriptMaxchar)
    
    sourceLayer <- 'southAmerica'
    
    if (corMun[[1]] == 0 & validCon){
      over <- over(coords, mpios2014)
      corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                              dataID = 1, maxchar = .scriptMaxchar)  
      sourceLayer <- 'mpiosCol2014' 
      
      if (corMun[[1]] == 0){
        over <- over(coords, mpios2011)
        corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                dataID = 1, maxchar = .scriptMaxchar)
        sourceLayer <- 'mpiosCol2011' 
        
        if (corMun[[1]] == 0){
          over <- over(coords, mpios2003)
          corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                  dataID = 1, maxchar = .scriptMaxchar)
          sourceLayer <- 'mpiosCol2003' 
          
          if (corMun[[1]] == 0){
            over <- over(coords, mpios1993)
            corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                    dataID = 1, maxchar = .scriptMaxchar)
            sourceLayer <- 'mpiosCol1993' 
            
            if (corMun[[1]] == 0){
              over <- over(coords, mpios1985)
              corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                      dataID = 1, maxchar = .scriptMaxchar)
              sourceLayer <- 'mpiosCol1985' 
              
              if (corMun[[1]] == 0){
                over <- over(coords, mpios1973)
                corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                        dataID = 1, maxchar = .scriptMaxchar)
                sourceLayer <- 'mpiosCol1973' 
                
                if (corMun[[1]] == 0){
                  over <- over(coords, mpios1964)
                  corMun <- corroboracion(overPointsField = over$MPIOS, dataField = .recG$adm2, 
                                          dataID = 1, maxchar = .scriptMaxchar)
                  sourceLayer <- 'mpiosCol1964' 
                  if (corMun[[1]] == 0){
                    sourceLayer <- 'NA' 
                  }  
                }}}}}
      }
    }
    
    .recG$correctCounty <- as.logical(corMun[[1]])
    .recG$suggestedCounty <- corMun[[3]]  
    return(list(nwRec = .recG, sourceLayer = sourceLayer))
  }
  
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
  
  dbSendQuery(con,"UPDATE tabladetrabajo INNER JOIN _search_scientific ON (tabladetrabajo.id_nombre_aceptado = _search_scientific.id) SET
              tabladetrabajo.autor_nombre_aceptado = _search_scientific.author,
              
              tabladetrabajo.reino_CoL = _search_scientific.kingdom,
              tabladetrabajo.phylum_CoL = _search_scientific.phylum,
              tabladetrabajo.clase_CoL = _search_scientific.class,
              tabladetrabajo.orden_CoL = _search_scientific.order,
              tabladetrabajo.familia_CoL = _search_scientific.family,
              tabladetrabajo.genero_CoL = _search_scientific.genus,
              tabladetrabajo.epiteto_CoL = _search_scientific.species,
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
  
  return(finalTabl||e)
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
  if(length(pendSpeTax)){
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
  # tempDirRecordsPath = tempDirManualTaxValidation; .pattern = '.csv'
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
                                      fields = '{"_id" : 1, "lat" : 1, "lon" : 1, "demAltitude": 1, "interpretedElevation":1, "alt": 1}')
    coordSpatial <- SpatialPoints(mT[, c('lon', 'lat')], proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    if('altitudinalOutlier' %in% .flags){
      mT$altitudinalOutlier <- rowSums(outliersIG(rid = 1:nrow(mT), species = rep('SP', nrow(mT)), dups = rep(0, nrow(mT)), ev = mT$demAltitude)) > 0
    }
    
     if('consistentAltitude' %in% .flags){
       mT$consistentAltitude <- 'NULL'
     }
    
    if('diferenceInAltitude' %in% .flags){
      mT$diferenceInAltitude <- abs(mT$demAltitude - as.numeric(mT$alt))
      mT$diferenceInAltitude[is.na(mT$diferenceInAltitude)] <- 'NULL'
    }
    
    if('environmentalOutlier' %in% .flags){
      extVals <- raster::extract(layNE, coordSpatial)
        envOut <- apply(extVals, 2, FUN = function(x) {
          rowSums(outliersIG(rid = 1:nrow(mT), species = rep('SP', nrow(mT)), dups = rep(0, nrow(mT)), ev = x))
      })
        mT$environmentalOutlier <- rowSums(envOut) > 0
    }
    
    # Loop for upload records one-by-one
    for(m in 1:nrow(mT)){
      oid <- paste0(mT$`_id`[[m]], collapse = '')
      upQ <- paste0('"', .flags, '":"', mT[m, c(.flags)], '"', collapse = ', ')
      
      mongoConectRec$update(query = paste0('{"_id":{"$oid":"',oid,'"}}'), 
                            update = paste0('{"$set":{',upQ,'}}'), 
                            upsert = TRUE)
      mongoConectRec$find(query = paste0('{"_id":{"$oid":"',oid,'"}}'))
    }
}
