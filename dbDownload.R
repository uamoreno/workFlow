library(xlsx)
library(raster)
library(maptools)
library(dismo)
  

updateLBAB <- function(root, outPath = "set0/", refineType = "species", refreshDB = FALSE, spRefine = NULL, 
                       inShape = NULL, inFile = NULL){
  
  # outPath = "set0/"; refineType = "species"; refreshDB = TRUE; spRefine = "Ara macao"; inShape = NULL; inFile = NULL
  
  ##  Variables interactivas:
  # spRefine <- "cadena"
  # InShape <- "shapeFile in WGS84"
  # InFile <- "csv file with standard format"
  
  library(maptools)
  library(raster)
  library(rvertnet)
  
  if (refreshDB == TRUE){ 
    if (refineType == "region") {
  
      aoi <- InShape
        
      registros <- updateGSDB(root, aoi)
    
      registros <- cbind("ID" = c(1:nrow(registros)), registros)
      registros$species <- cleanSciNames(registros$species)
          
    } else if (refineType == "species") {
      
      gen <- strsplit(spRefine, " ")[[1]][1]
      spe <- strsplit(spRefine, " ")[[1]][2]
      
      baseGBIF <- "http://data.gbif.org/ws/rest/occurrence/"
      baseSiB <- "http://data.sibcolombia.net/ws/rest/occurrence/"
      
      (spec <- paste0(gen, "+", spe))
      
      urlGBIF <- paste(baseGBIF, "list?scientificname=", spec, "&coordinateissues=true&coordinatestatus=true&mode=processed&format=darwin", sep = "")
      urlSiB <- paste(baseSiB, "list?scientificname=", spec, "&coordinateissues=true&coordinatestatus=true&mode=processed&format=darwin", sep = "")
      
      webGBIF <- try(readLines(urlGBIF, warn = FALSE))
      webSiB <- try(readLines(urlSiB, warn = FALSE))
      
      dataGBIF <- try(gbifxmlToDataFrame2(webGBIF))
      dataSiB <- try(gbifxmlToDataFrame2(webSiB))
  
      dataGBIF <- cbind("source" = "GBIF", dataGBIF)
      dataSiB <- cbind("source" = "SiB", dataSiB)
      
      dataVer <- verRecords(spRefine)
      
      urlSpL <- paste0("http://splink2humboldt.services.cria.org.br/retrieve/userid/humboldt/genus/", gen, "/species/", spe)
      webSpL <- try(readLines(urlSpL, warn = FALSE))
      dataSpL <- try(gbifxmlToDataFrame3(webSpL))
      
      registros <- rbind(dataGBIF, dataSiB, dataSpL)

      if (class(dataVer) != "logical") {
        registros <- rbind(registros, dataVer)    
      }
      
      colnames(registros) <- c("source", "occurrenceID", "speciesOriginal","species", "continent", "country", "adm1", 
                          "adm2", "locality", "lat", "lon", "coordUncertaintyM", "maxElevationM",
                          "minElevationM", "maxDepthM", "minDepthM", "institution", "collection",
                          "catalogNumber", "basisOfRecord", "collector", "earliestDateCollected",
                          "latestDateCollected", "gbifNotes")
      
      registros$occurrenceID <- as.character(registros$occurrenceID)
      registros$species <- cleanSciNames(registros$speciesOriginal)  
    } 
  } else if (!is.null(inFile) & refreshDB != TRUE){
    registros <- rbind(registros, InFile)
  }

  ## Guardar archivo en la nube y hacer descargable
  save(registros, file = paste0(root, "/", outPath, "/set0.RData"))  
}


updateGSDB <- function(root, aoi, resume = FALSE, resumeObj){
  #STEP O: LOAD LIBRARIES AND SET WORKSPACE
  library(raster)
  library(sp)
  library(XML)
  pathGBIF<-paste(root,"/gbif",sep="")
  pathSIB<-paste(root,"/sib",sep="")
  
  dir.create(pathGBIF, showWarnings = FALSE, recursive=T)
  dir.create(pathSIB, showWarnings = FALSE, recursive=T)
  
  #Get GBIF gridcell IDs within AOI
  a<-matrix(0:64799,nrow=180,ncol=360,byrow=TRUE)
  c<-a[nrow(a):1,]
  gbifWorld<-raster(c,xmn=-180,xmx=180,ymn=-90,ymx=90,crs= "+proj=longlat +datum=WGS84")
  # plot(gbifWorld)
  rm(a,c)
  aoi_raster <- rasterize(aoi, gbifWorld, getCover=T, background=0)
  plot(aoi_raster, xlim=c(-85,-50), ylim=c(-18,15), legend = F)
  gb_cellids <- Which(aoi_raster>0, cells=TRUE)
  xy_cellids <- xyFromCell(aoi_raster, gb_cellids)
  cellids <- extract(gbifWorld, xy_cellids) #GBIF cells that overlap with Colombia
  rm(aoi_raster,gb_cellids,xy_cellids)
  cellobjs <- paste("c", cellids, sep = "")
  
  start1=1
  start2=1
  step1=TRUE
  step2=TRUE
  
  if(resume){
    step1=resumeObj$step1
    step2=resumeObj$step2
    if(step1){
      start1 = resumeObj$errorIter
      start2 = 1
    }
    if(step2){
      start2 = resumeObj$errorIter
    }
  }
  
  #STEP 1:GET DATA FROM GBIF
  if(step1){
    gbif<-dwnData(cellids,start=start1,path=pathGBIF,base="http://data.gbif.org/ws/rest/occurrence/")
    if(gbif$isError){
      print("Stopping execution of updateGSDB. Use resume=TRUE to continue")
      return(c(gbif, step1 = TRUE, step2 = TRUE))
    }
  }
  
  #STEP 2: GET DATA FROM SIB COLOMBIA
  if(step2){
    sib<-dwnData(cellids,start=start2,path=pathSIB,base="http://data.sibcolombia.net/ws/rest/occurrence/")
    if(sib$isError){
      print("Stopping execution of updateGSDB. Use resume=TRUE to continue")
      return(c(sib, step1 = FALSE, step2 = TRUE))
    }
  }
  
  #Read gbif tables and concatenate them in a single file
  z<-NULL
  for(i in 1:length(cellobjs)){
    print(paste0("GBIF - ", cellobjs[i], ".txt - ", round(i/length(cellobjs)*100, 1), "%")) 
    if(!file.exists(paste0(pathGBIF, "/", cellobjs[i], ".txt"))){#Skips empty files
      next}
    inFile<-tryCatch(read.table(paste0(pathGBIF,"/",cellobjs[i],".txt"),header=TRUE,as.is=TRUE), error = function(e){
      error.read <- FALSE})
    if (class(inFile) != "logical"){
      z<-rbind(z,cbind("source" = "GBIF",inFile))  
    }
    if (class(inFile) == "logical"){
      cat("FALSO")
      table.error <- read.table(paste0(pathGBIF,"/",cellobjs[i],".txt"), header=TRUE,stringsAsFactors = FALSE, fill = TRUE); dim(table.error)
      table.error2 <- cbind("source" = "GBIF", table.error[, 1:24])
      z <- rbind(z, table.error2)
    }
    z<-rbind(z,cbind("source" = "GBIF",inFile))
  }
  
  #Read sib tables and concatenate them into a single file
  w<-NULL
  for(i in 1:length(cellobjs)){
    print(paste0("SiB - ",cellobjs[i],".txt - ", round(i/length(cellobjs)*100,1),"%")) 
    if(!file.exists(paste0(pathSIB,"/",cellobjs[i],".txt"))){
      next}
    inFile<-tryCatch(read.table(paste0(pathSIB,"/",cellobjs[i],".txt"),header=TRUE,as.is=TRUE), error = function(e){
      error.read <- FALSE})
    if (class(inFile) != "logical"){
      w<-rbind(w,cbind("source" = "SiB",inFile)) 
    } 
    if (class(inFile) == "logical"){
      cat("FALSO")
      table.error <- read.table(paste0(pathSIB,"/",cellobjs[i],".txt"), header=TRUE,stringsAsFactors = FALSE, fill = TRUE); dim(table.error)
      table.error2 <- cbind("source" = "SiB",table.error[,1:24])
      w<-rbind(w,table.error2)
    }
    w<-rbind(w,cbind("source" = "SiB",inFile)) 
  }
  rm(i,inFile)
  
  head(z)
  #Filter z and w by limits of AOI
  filterAOI<-function(dt,aoi){
    dt_aoi<-data.frame(lon=dt$lon,lat=dt$lat)
    coordinates(dt_aoi)=~lon+lat
    projection(dt_aoi)<-projection(aoi)
    ind<-overlay(dt_aoi,aoi)
    return(dt[-is.na(ind),])
  }
  zFilt<-filterAOI(z,aoi)
  wFilt<-filterAOI(w,aoi)
  
  # Merge GBIF and SIB
  db <-rbind(zFilt,wFilt)
  return(db)
}


# Funcion para descargar los datos
dwnData<-function(cellids, start = 1, path, base){
  for(iter in start:length(cellids)){
    print(paste("Working on cell",cellids[iter],"-", iter,"of",length(cellids), "cells" ))
    print(base)
    out<-tryCatch(gbif2(cellid=cellids[iter],base=base),
                  error=function(e){
                    errorIter=iter
                    print(paste0("Code stopped at cell ",cellids[iter]))
                    return(errorIter)
                  })
    if(is.data.frame(out)){
      write.table(out,paste(path,"/c",cellids[iter],".txt",sep=""),row.names=FALSE)
      next
    } 
    if(is.null(out)){
      next
    } else {
      return(list(isError=TRUE,errorIter=out))
    }
  }
  return(list(isError=FALSE))
}

# Funcion para pasar los datos descargados a formato tabla
gbif2<-function (genus=NULL, species = "", ext = NULL, cellid=NULL,geo = TRUE, sp = FALSE, 
                 removeZeros = TRUE, download = TRUE, getAlt = TRUE, ntries = 5, 
                 nrecs = 1000, start = 1, end = NULL, feedback = 3,
                 base="http://data.gbif.org/ws/rest/occurrence/")
{
  if (!require(XML)) {
    stop("You need to install the XML package to use this function")
  }
  gbifxmlToDataFrame <- function(s) {
    doc = xmlInternalTreeParse(s)
    nodes <- getNodeSet(doc, "//to:TaxonOccurrence")
    ids <- getNodeSet(doc, "to:TaxonOccurrence")
    
    if (length(nodes) == 0) 
      return(data.frame())
    varNames <- c("occurrenceID","continent", "country", "stateProvince", 
                  "county", "locality", "decimalLatitude", "decimalLongitude", 
                  "coordinateUncertaintyInMeters", "maximumElevationInMeters", 
                  "minimumElevationInMeters", "maximumDepthInMeters", 
                  "minimumDepthInMeters", "institutionCode", "collectionCode", 
                  "catalogNumber", "basisOfRecordString", "collector", 
                  "earliestDateCollected", "latestDateCollected", "gbifNotes")
        
    dims <- c(length(nodes), length(varNames))
    ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), 
                                                dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
    names(ans) <- varNames
    for (i in seq(length = dims[1])) {
      ans[i, ] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
      ans[i,"occurrenceID"] <- xmlToList(nodes[[i]])$.attrs@.Data[1]
    }
    nodes <- getNodeSet(doc, "//to:Identification")
    varNames <- c("taxonName")
    dims = c(length(nodes), length(varNames))
    tax = as.data.frame(replicate(dims[2], rep(as.character(NA), 
                                               dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
    names(tax) = varNames
    for (i in seq(length = dims[1])) {
      tax[i, ] = xmlSApply(nodes[[i]], xmlValue)[varNames]
    }
    names(tax) <- NULL
    cbind(occurrenceID = ans[, 1], "speciesOriginal" = tax, "species" = tax, ans[, -1])
  }
  if (!is.null(ext)) {
    ex <- round(extent(ext), 5)
    ex <- paste("&minlatitude=", max(-90, ex@ymin), "&maxlatitude=", 
                min(90, ex@ymax), "&minlongitude=", max(-180, ex@xmin), 
                "&maxlongitude=", min(180, ex@xmax), sep = "")
  }
  else {
    ex <- NULL
  }
  if (sp) 
    geo <- TRUE
  if (geo) {
    cds <- "&coordinatestatus=true"
  }
  else {
    cds <- ""
  }  
  if(!is.null(genus)){
    genus <- trim(genus)
    species <- trim(species)
    gensp <- paste(genus, species)
    spec <- gsub("   ", " ", species)
    spec <- gsub("  ", " ", spec)
    spec <- gsub(" ", "%20", spec)
    spec <- paste(genus, "+", spec, sep = "")
    url <- paste(base, "count?scientificname=", spec, cds, ex,"&coordinateissues=false", sep = "")
  } else {
    gensp=cellid
    url <- paste(base, "count?cellid=",cellid,cds,"&coordinateissues=false",sep = "")
  }
  tries <- 0
  while (TRUE) {
    tries <- tries + 1
    if (tries > 10) {
      stop("GBIF server does not return a valid answer after 5 tries")
    }
    x <- try(readLines(url, warn = FALSE))
    if (class(x) != "try-error") 
      break
  }
  x <- x[grep("totalMatched", x)]
  n <- as.integer(unlist(strsplit(x, "\""))[2])
  if (!download) {
    return(n)
  }
  if (n == 0) {
    cat(gensp, ": no occurrences found\n")
    return(invisible(NULL))
  }
  else {
    if (feedback > 0) {
      cat(gensp, ":", n, "occurrences found\n")
      flush.console()
    }
  }
  ntries <- min(max(ntries, 1), 100)
  if (!download) {
    return(n)
  }
  nrecs <- min(max(nrecs, 1), 1000)
  iter <- n%/%nrecs
  breakout <- FALSE
  if (start > 1) {
    ss <- floor(start/nrecs)
  }
  else {
    ss <- 0
  }
  z <- NULL
  for (group in ss:iter) {
    start <- group * nrecs
    if (feedback > 1) {
      if (group == iter) {
        end <- n - 1
      }
      else {
        end <- start + nrecs - 1
      }
      if (group == ss) {
        cat(ss, "-", end + 1, sep = "")
      }
      else {
        cat("-", end + 1, sep = "")
      }
      if ((group > ss & group%%20 == 0) | group == iter) {
        cat("\n")
      }
      flush.console()
    }
    if(!is.null(genus)){
      aurl <- paste(base, "list?scientificname=", spec, "&mode=processed&format=darwin&startindex=", 
                    format(start, scientific = FALSE), cds, ex,"&coordinateissues=false",sep = "")
    } else {
      aurl <- paste(base, "list?cellid=", cellid, "&mode=processed&format=darwin&startindex=", 
                    format(start, scientific = FALSE), cds,"&coordinateissues=false", sep = "")
    }
    tries <- 0
    while (TRUE) {
      tries <- tries + 1
      if (tries > ntries) {
        warning("GBIF did not return the data in ", ntries)
        breakout <- TRUE
        break
      }
      zz <- try(gbifxmlToDataFrame(aurl))
      if (class(zz) != "try-error") 
        break
    }
    if (breakout) {
      break
    }
    else {
      z <- rbind(z, zz)
    }
  }
  d <- as.Date(Sys.time())
  z <- cbind(z, d)
  names(z) <- c("occurrenceID","species", "continent", "country", "adm1", 
                "adm2", "locality", "lat", "lon", "coordUncertaintyM", 
                "maxElevationM", "minElevationM", "maxDepthM", "minDepthM", 
                "institution", "collection", "catalogNumber", "basisOfRecord", 
                "collector", "earliestDateCollected", "latestDateCollected", 
                "gbifNotes", "downloadDate")
  
  #   
  z[, "lon"] <- gsub(",", ".", z[, "lon"])
  z[, "lat"] <- gsub(",", ".", z[, "lat"])
  z[, "lon"] <- as.numeric(z[, "lon"])
  z[, "lat"] <- as.numeric(z[, "lat"])
  if (removeZeros) {
    i <- isTRUE(z[, "lon"] == 0 & z[, "lat"] == 0)
    if (geo) {
      z <- z[!i, ]
    }
    else {
      z[i, "lat"] <- NA
      z[i, "lon"] <- NA
    }
  }
  if (getAlt) {
    altfun <- function(x) {
      a <- mean(as.numeric(unlist(strsplit(gsub("-", " ", 
                                                gsub("m", "", (gsub(",", "", gsub("\"", "", x))))), 
                                           " ")), silent = TRUE), na.rm = TRUE)
      a[a == 0] <- NA
      mean(a, na.rm = TRUE)
    }
    if (feedback < 3) {
      w <- options("warn")
      options(warn = -1)
    }
    alt <- apply(z[, c("maxElevationM", "minElevationM", 
                       "maxDepthM", "minDepthM")], 1, FUN = altfun)
    if (feedback < 3) 
      options(warn = w)
    z <- cbind(z[, c("occurrenceID","species", "continent", "country", "adm1", 
                     "adm2", "locality", "lat", "lon", "coordUncertaintyM")], 
               alt, z[, c("institution", "collection", "catalogNumber", 
                          "basisOfRecord", "collector", "earliestDateCollected", 
                          "latestDateCollected", "gbifNotes", "downloadDate", 
                          "maxElevationM", "minElevationM", "maxDepthM", 
                          "minDepthM")])
  }
  if (sp) {
    i <- z[!(is.na(z[, "lon"] | is.na(z[, "lat"]))), ]
    if (dim(z)[1] > 0) {
      coordinates(z) <- ~lon + lat
    }
  }
  return(z)
}


gbifxmlToDataFrame2 <- function(s) {
  require(XML)
  doc = xmlInternalTreeParse(s)
  nodes <- getNodeSet(doc, "//to:TaxonOccurrence")
  ids <- getNodeSet(doc, "to:TaxonOccurrence")
  
  if (length(nodes) == 0) 
    return(data.frame())
  varNames <- c("occurrenceID","continent", "country", "stateProvince", 
                "county", "locality", "decimalLatitude", "decimalLongitude", 
                "coordinateUncertaintyInMeters", "maximumElevationInMeters", 
                "minimumElevationInMeters", "maximumDepthInMeters", 
                "minimumDepthInMeters", "institutionCode", "collectionCode", 
                "catalogNumber", "basisOfRecordString", "collector", 
                "earliestDateCollected", "latestDateCollected", "gbifNotes")
  
  dims <- c(length(nodes), length(varNames))
  ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), 
                                              dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
  names(ans) <- varNames
  for (i in seq(length = dims[1])) {
    ans[i, ] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
    ans[i,"occurrenceID"] <- xmlToList(nodes[[i]])$.attrs@.Data[1]
  }
  nodes <- getNodeSet(doc, "//to:Identification")
  varNames <- c("taxonName")
  dims = c(length(nodes), length(varNames))
  tax = as.data.frame(replicate(dims[2], rep(as.character(NA), 
                                             dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
  names(tax) = varNames
  for (i in seq(length = dims[1])) {
    tax[i, ] = xmlSApply(nodes[[i]], xmlValue)[varNames]
  }
  names(tax) <- NULL
  cbind(occurrenceID = ans[, 1], "speciesOriginal" = tax, "species" = tax, ans[, -1])
}

gbifxmlToDataFrame3 <- function(s) {
  require(XML)
  doc <- xmlInternalTreeParse(s)
  nodes <- getNodeSet(doc, "//record")
  ids <- getNodeSet(doc, "//record")
  
  if (length(nodes) == 0) 
    return(data.frame())
  varNames <- c("source","occurrenceID", "scientificname", "scientificname","continent", "country", "stateprovince", "county", "locality",
                "latitude", "longitude", "coordinateUncertaintyInMeters", "maximumElevationInMeters", 
                "minimumElevationInMeters", "maximumDepthInMeters", "minimumDepthInMeters","institutionCode", "collectioncode",
                "catalognumber", "basisOfRecordString","collector","earliestDateCollected", "latestDateCollected", "gbifNotes")
                
  
  # "collectioncode","catalognumber", "scientificname", "collector", "stateprovince", "county", "locality", "latitude", "longitude"
  
  dims <- c(length(nodes), length(varNames))
  ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), 
                                              dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
  names(ans) <- varNames
  for (i in seq(length = dims[1])) {
    ans[i, ] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
    ans[i,"occurrenceID"] <- xmlToList(nodes[[i]])$.attrs@.Data[1]
    ans[i, "source"] <- "spLink"
  }
  colnames(ans) <- colnames(dataGBIF)
  return(ans)
}


verRecords <- function(sp) {
  lapp <- lapply(c("fish", "bird", "herp"), FUN = function(y) {
    vert <- tryCatch(vertoccurrence(t = sp,  grp = y), error = function(e) {FALSE})
    if (class(vert) == "data.frame") {
      i <- sapply(vert, is.factor)
      vert[i] <- lapply(vert[i], as.character)
      mat <- cbind(source = "VertNet", occurrenceID = NA, vert[, c("ScientificName","ScientificName","Country","Country","StateProvince",
                                                                   "County","Locality","Latitude","Longitude","CoordinateUncertaintyInMeters",
                                                                   "VerbatimElevation","VerbatimElevation","VerbatimDepth","VerbatimDepth",
                                                                   "InstitutionCode","CollectionCode","CatalogNumber","BasisOfRecord",
                                                                   "Collector")], paste(vert$YearCollected, "-", vert$MonthCollected, "-", vert$DayCollected, sep =""),
                   paste(vert$YearCollected, "-", vert$MonthCollected, "-", vert$DayCollected, sep =""), 
                   paste0(vert$Remarks, ";", vert$Tissues))
      colnames(mat) <- colnames(dataGBIF)
      return(mat)
    }
  })
  good <- which(!(sapply(lapp, is.null)))
  if (length(good)>0){
    return(lapp[[good]])
  } else {
    NA
  }
}


# Depurar nombres cientmficos a ginero y epmteto eliminando caracteres como:
#
#  Confirmation: cf., ' cf '.
#  Afinis: sp. aff., aff., affin. http://en.wikipedia.org/wiki/Species_affinis
#  Variety: var., var 

(x <- "Miconia\niconia chamissois")
#str_replace_all(x, "[^[:alnum:]]", " ")
cleanSciNames  <- function(y){ 
  require(R.utils)
  if (!require(R.utils)) {
    stop("You need to install the 'R.utils' package to use this function")
  }
  y <- sapply(y, FUN = function(x){ 
    (x <- gsub("_","",gsub(" _","",gsub("  _","",paste0(x,"_")))))
    (x <- gsub("_"," ",gsub("/"," ",x)))
    (x <- gsub("\n"," n",x))
    
    if (gregexpr("\\).*",x)[[1]][1] != -1){
      (x <- gsub("_"," ",x))
      ip <- gregexpr("\\(.*",x)[[1]][1]
      fp <- gregexpr("\\).*",x)[[1]][1]
      x1 <- substr(x, 0, ip-2)
      x2 <- substr(x, fp+1, nchar(x))
      x <- paste0(x1,x2)
    }
    (x <- gsub(" cf\\.","",x))
    (x <- gsub(" cf "," ",x))
    (x <- gsub(" var\\.","",x))
    (x <- gsub(" var "," ",x))
    (x <- gsub("sp\\.","",x))
    (x <- gsub(" sp\\.","",x))
    (x <- gsub(" sp ","",x))
    (x <- gsub(" aff\\.","",x))
    (x <- gsub(" affin\\.","",x))
    (x <- gsub(" aff ","",x))
    (x <- gsub(" affin ","",x))
    (x <- gsub("  "," ",x))
    if (!is.na(gregexpr(" ",x)[[1]][2])){
      pos <- gregexpr(" ",x)[[1]][2]
      (x <- substr(x,0,pos-1))
    }
    return(capitalize(x))
  }
  )
}

# orig2set16
orig2set16 <- function(archivo.i, format, fuente = NULL, occID = NULL, cleanscinames = TRUE, startID = 0, cleanCoords = FALSE){
  
  #  Se carga el formato 'set16' para que los datos cargados poseriormente sean establecidos con istos estandares
  #  Esta instruccion ya no se cargara desde la funcion sino que se pasara como argumento de la funcion  
  #format.datos <- read.csv("C:/Google Drive/datos_otros/estructuraset16.csv")
  #format <- format.datos[-(1:nrow(format.datos)),]
  #fuente = NULL; occID = NULL; cleanscinames = TRUE; startID = 0; cleanCoords = FALSE
  
  #archivo.i <- read.delim("C:/Google Drive/datos_otros/vernet/Vernet 2014 04 11.txt"); fuente <- "Vernet";occID <- "w"
  
  #  Se crea una tabla vacma con la estructura set 16 que se llenara con la informacisn del archivo
  format.i <- as.data.frame(matrix("",nrow(archivo.i),ncol(format))); dim(format)
  colnames(format.i) <- colnames(format)
  #  Ciclo entre el nzmero de campos del archivo cargado
  j <-1
  for (j in 1:ncol(format.i)){
    (pos <- which(colnames(archivo.i) == colnames(format)[j]))
    if (length(pos)!=0){
      format.i[,j] <- as.character(archivo.i[,pos])
      #cat(fuente,j,"columnas de",ncol(archivo.i),"para", ncol(format), "esperadas","\n")
    }
    # head(format.i)
  }
  if (!is.null(fuente)){
    format.i$'source' <- fuente # Agregar fuente de la informacisn
    if (is.null(occID)){
      format.i$occurrenceID <- paste0(fuente,"-",startID:(startID+nrow(format.i)-1)) #Creo un ID consecutivo
    }}
  if (!is.null(occID)){
    format.i$occurrenceID <- occID#paste0(occID,"-",startID:(startID+nrow(format.i)-1)) #Creo un ID consecutivo
  }
  
  # Volver formato numirico las variables lat, lon, alt
  if (cleanCoords != TRUE){
    if (length(which(colnames(format.i) == "lat"))>0){
      format.i$lat <- as.numeric(format.i$lat); format.i$lon <- as.numeric(format.i$lon)
      # Eliminar los registros que no tengan valores en coordenadas en caso de haber
      no_coords <- unique(append(which(is.na(format.i$lat)),which(is.na(format.i$lon))))
      if (length(no_coords)>0) {format.i <- format.i[-no_coords,]}
    }
  }
  
  # Cambiar a 2 palabras el nombre cientifico
  if  (cleanscinames == TRUE){
    format.i$species <- cleanSciNames(gsub("_"," ",format.i$speciesOriginal))
  } else {
    format.i$species <- format.i$speciesOriginal
  }
  return(format.i)
}

# Rescate de localidades
grep2 <- function(y,line){
  patterns <- c(paste0(c(" ","-"),tolower(y)), paste0(tolower(y),c("."," ","-")))
  multigrep <- sapply(patterns, FUN = function(x){
    g1 <- grep(x,tolower(line))
  })
  pos <- which(tolower(line) == tolower(y))
  return(unique(c(unlist(multigrep),pos)))
}