verifGeo <- function(root, routineType = "Colombia", inShape = NULL,
                     set1Path = "set1/", set2Path = "set2/", set16Path = "set16/", mapsPath = "maps/"){
  
  # routineType = "Colombia"; inShape = NULL; set1Path = "set1/"; set2Path = "set2/"; set16Path = "set16/"; mapsPath = "maps/"
  
  ##  Variables interactivas:
  # set2 <- "cadena" indicando ubicacion archivo o tabla
  # inShape <- poligono de class() "SpatialPolygonsDataFrame" y con inShape@proj4string@projargs == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # routineType <- "cadena" indicando el tipo de filtro para hacer en los datos. "Colombia" or "World"    
  
  #rm(list = ls())  # Borra  todo lo que este en memoria
  #memory.limit(size = 1000000) 
  
  library(dismo)
  library(maptools)
  library(sp)
  library(maps)
  library(svDialogs)
  library(xlsx)
  library(xlsxjars)
  library(rJava)
  library(R.utils)
  
  load(paste0(root, "/", set1Path, "/set1.RData"))  
  
  aoiFilter(registros, root, routineType = "Colombia", inShape = NULL, outPath = set2Path, taxPath = "maps/TAX.RData")

  load(paste0(root, "/", set2Path, "/set2.RData"))
  
  set3 <- set2[, c("ID", "source", "especie_aceptada", "country", "adm1", "adm2", "lat", "lon", "earliestDateCollected", "locality")]
  colnames(set3) <- c("id", "source", "nombre", "pais", "departamento", "municipio", "latitud", "longitud", "fecha_inicial", "localidad")
       
  RESULTADOS <- VERIFICACION_PAISES(set3, routineType = "Colombia", mapsPath = "/maps", outPath = set16Path)  
}

aoiFilter <- function(set2, root, routineType = "Colombia", inShape = NULL, outPath = "/set2/", taxPath = "maps/TAX.RData"){
  
  ##  Variables interactivas:
  # set2 <- "cadena" indicando ubicacion archivo otabla
  # inShape <- poligono de class() "SpatialPolygonsDataFrame" y con inShape@proj4string@projargs == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # routineType <- "cadena" indicando el tipo de filtro para hacer en los datos. "Colombia" or "World"

  #----------------------------------------#
  #  1. Liberar espacio y cargar paquetes  # 
  #----------------------------------------# 
  
  #rm(list = ls())
  gc()
  memory.limit(size = 1000000) 
  library(raster)
  library(sp)
  #------------------------------------------#
  #  2. Cargar datos de una o varias fuentes # 
  #------------------------------------------# 
  
  set2 <- registros
  rm(registros)
  
  # Arreglar naturaleza de los campos
  set2$speciesOriginal <- as.character(set2$speciesOriginal)
  set2$especie_aceptada <- as.character(set2$especie_aceptada)
  set2$lat <- as.numeric(set2$lat)
  set2$lon <- as.numeric(set2$lon)
  set2$alt <- as.numeric(set2$alt)
  set2$coordUncertaintyM <- as.numeric(set2$coordUncertaintyM)
  set2$maxElevationM <- as.numeric(set2$maxElevationM)
  set2$minElevationM <- as.numeric(set2$minElevationM)
  set2$maxDepthM <- as.numeric(set2$maxDepthM)
  set2$minDepthM <- as.numeric(set2$minDepthM)
  set2$source <- as.factor(set2$source)
  
  
  #-------------------------------------------------------------------#
  #  3. Identificar especies en poligono segun coordenadas # 
  #-------------------------------------------------------------------# 
  #inShape <- readShapePoly("C:/IAvH/DINAVIS_set16/maps/co_buffe10km.shp")
  
  # Condicional que identifica si el objeto 'inShape' es un poligono espacial y genera un filtro de para los registros
  if (class(inShape) == "SpatialPolygonsDataFrame"){
    library(maptools)
    library(rgdal)
    inShape$inAoi <- 1

    coords <- set2[, c("ID","source","species","speciesOriginal","lat","lon","especie_aceptada")]
    coordinates(coords) =~ lon + lat 
    coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    over.coords <- over(coords, inShape)
    set2 <- cbind(set2, "enAOI" = over.coords$inAoi)
      
    world <- readShapePoly("C:/IAvH/VERIFICACION PAISES VECINOS/Info geografica/PAISES_COMPLETO5.shp")
    nsame <- readShapePoly("C:/Users/GIC 76/Downloads/Mapas/gadm2.shp")
    
    tabla.over <- (table(set2$species, set2$enAOI))
    tabla.overO <- (table(set2$speciesOriginal, set2$enAOI))
    tabla.overV <- (table(set2$especie_aceptada, set2$enAOI))
    
    tabla.over <- cbind(tabla.over, rowSums(tabla.over), rowSums(tabla.over)>0)
    tabla.overO <- cbind(tabla.overO, rowSums(tabla.overO), rowSums(tabla.overO)>0)
    tabla.overV <- cbind(tabla.overV, rowSums(tabla.overV), rowSums(tabla.overV)>0)
    
    sp.col <- rownames(tabla.over)[unique(c(which(tabla.over[, 3]>0)))]
    sp.colO <- rownames(tabla.overO)[unique(c(which(tabla.overO[, 3]>0)))]
    sp.colV <- rownames(tabla.overV)[unique(c(which(tabla.overV[, 3]>0), which(rownames(tabla.overV) == "NA NA")))]
    
    sp.pos <-  which(set2$species %in% sp.col)
    sp.posO <- which(set2$speciesOriginal %in% sp.colO)
    sp.posV <- which(set2$especie_aceptada %in% sp.colV)
    
    indexspcol <- unique(c(sp.pos, sp.posO, sp.posV))
  
    #----------------------------------------------------------------#
    #  4. Identificar especies en Colombia segun listado en DINAVIS  # 
    #----------------------------------------------------------------# 
    
    if (routineType == "Colombia"){
      load(paste(root, "/", taxPath))
      sp.TAX1 <- which(set2$species %in% TAX$scientificName)
      sp.TAX2 <- which(set2$speciesOriginal %in% TAX$scientificName)
      sp.TAX3 <- which(set2$especie_aceptada %in% TAX$scientificName)
      sp.TAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3))
      indexspcol <- unique(c(indexspcol, sp.TAX))
    }
    
    set2 <- set2[indexspcol, ]  
    
  } else if (is.null(inShape) & routineType == "Colombia") {
    load(paste0(root, "/", taxPath))
    sp.TAX1 <- which(set2$species %in% TAX$scientificName)
    sp.TAX2 <- which(set2$speciesOriginal %in% TAX$scientificName)
    sp.TAX3 <- which(set2$especie_aceptada %in% TAX$scientificName)
    sp.TAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3))
    indexspcol <- unique(c(sp.TAX))
    set2 <- set2[indexspcol, ]  
  }
  
  #-------------------------------#
  #  6. Aplicar indice duplicidad # 
  #-------------------------------# 
  
  set2$ID <-1:nrow(set2)
  indDup <- paste(set2$collection,set2$catalogNumber, sep ="-")
  set2$DupIndex <- 0
  dup <- duplicated(indDup, fromLast = TRUE)
  dupNA <- which(indDup =="NA-NA" |indDup == "-")
  set2$DupIndex[dup] <- 1
  set2$DupIndex[dupNA] <- 0
  
  #-----------------------------------------------#
  #  7. Generar campo de incertidumbre taxonomica # 
  #-----------------------------------------------# 
  
  set2$taxDoubdt <- 0
  cf <- grep(" cf\\.", set2$speciesOriginal)
  cf2 <- grep(" cf ", set2$speciesOriginal)
  inter <- grep("\\?", set2$speciesOriginal)
  taxDoubdt <- unique(c(cf, cf2, inter))
  set2$taxDoubdt[taxDoubdt] <- 1

  save(set2, file = paste0(root, "/", outPath, "/set2.RData")) 
}

info_geografica <- function(root, mapsPath, routineType = "Colombia"){
  
  ##  Variables interactivas:
  # routineType <- "cadena" indicando el tipo de filtro para hacer en los datos. "Colombia" or "World"
  
  if (routineType == "World") {
    load(paste0(root, "/", mapsPath, "/Colombia.RData"))
  } else if (routineType == "Colombia"){    
    load(paste0(root, "/", mapsPath, "/Colombia.RData"))
  }
}

#datos <- set6A; mun <- mpios1964; layer.field <- "NAME_2"; data.field <- "municipio"
corroboracion <- function(datos, mun, layer.field, data.field){
  (a <- Sys.time())
  cat("    i. Coordenadas \n")
  coordinates(datos)=~longitud+latitud1
  cat("   ii. Overlay \n")
  ovm <- overlay(datos, mun)#;  str(ovm)
  assign("cntrm", eval(parse(text = paste0("as.character(mun@data$",layer.field,"[ovm])"))))
  assign("l", eval(parse(text = paste0("length(which(!is.na(datos@data$",data.field,")))"))))
  if(l == 1){
    assign("tmp", eval(parse(text = paste0("agrep(mun$",layer.field,"[ovm], datos$",data.field,", max = 1, value=F, ignore.case=T)"))))
    mmx <- c(1, 1, 1)[tmp]
    nmx <- NA
  } else if(l > 0){
    assign("jmx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) == gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con igual entidad
    assign("imx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) != gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con diferente entidad
    na.mx <- which(is.na(cntrm))
    id.exa <- datos@data$id[jmx] # ID's de las filas exactas 
    
    CompareMun <- cbind(imx, cntrm[imx], datos@data$municipio[imx], datos@data$id[imx])
    assign("CompareMun", eval(parse(text = paste0("cbind(imx, cntrm[imx], datos@data$", data.field,"[imx], datos@data$id[imx])")))) 
    
    uniqueMun <- (sort(unique(CompareMun[, 3]))) # Saco valores unicos por municipio reportados en tabla
    (uniqueMun <- uniqueMun[which(!is.na(uniqueMun) & uniqueMun != "")]) # Eliminos NA's de los municipios
    mmx <- c(0, 0)
    nmx <- c(0, 0)
    
    if (length(uniqueMun) > 0){
      cat("  iii. Ciclo \n")
      ma1x <- NULL
      cat("  iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n")
      for (i in 1:length(uniqueMun)){
        (uniqueMun[i])
        (pos.mun <- which(CompareMun[, 3] == uniqueMun[i])) # Selecciono posiciones que cotienen al municipio i del over
        (mun.i <- CompareMun[pos.mun, 2]) # selecciono municipios de tabla para el municipio de tabla i
        (tmp <- agrep(gsub(" ", "", uniqueMun[i]), gsub(" ", "",mun.i), max = 2, value=F, ignore.case=T)) #Comparo similitud entre municipios reportados en tabla y extraidos con coordenada
        (max <- cbind(as.integer(CompareMun[pos.mun, 4]), 0)) # Genero tabla con resultados
        (max[tmp, 2] <- 1) #Asigno 1 para los que esten bien
        (ma1x <- rbind(ma1x,max))
      }
      cat("  iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n") 
      lmx <- ma1x[which(ma1x[, 2] == 1), 1] # municipio igual. Extraigo posiciones de tabla original con municipios reconocidos validos 
      mmx <- sort(as.integer(c(id.exa, lmx))) # Filas de la tabla con datos validados positivamente
      
      kmx <- ma1x[which(ma1x[, 2] == 0), 1] # municipio diferente. 
      nmx <- sort(c(imx, kmx, na.mx))
    } 
    if (length(id.exa) > 0 | length(uniqueMun) <= 0){
      mmx <- sort(as.integer(c(id.exa))) 
      nmx <- sort(c(na.mx))
    }
  } else {
    mmx <- rep(0, nrow(datos))
    nmx <- rep(NA, nrow(datos))
  }
  Xx <- list()
  Xx[[1]] <- mmx
  Xx[[2]] <- nmx
  Xx[[3]] <- cntrm
  return(Xx)
  cat(print(a-Sys.time()))
}

VERIFICACION_PAISES <- function(set3, routineType = "Colombia", mapsPath = "/maps", outPath = "/set16"){
  
  info_geografica(root, mapsPath = mapsPath, routineType = "Colombia")
  
  set3$latitud <- as.numeric(set3$latitud)
  set3$longitud <- as.numeric(set3$longitud)
  set3$municipio <- iconv(set3$municipio)
  set3$departamento <- iconv(set3$departamento)
  set3$localidad <- iconv(set3$localidad)
  
  cat("  1. Evaluando registros registros con coordenadas (1 de 11)", "\n")
  
  set3$latitud1<-set3$latitud#  
  lat <- set3$latitud1#
  (p0 <- nrow(set3))
  
  row.without.lat <- which(!is.na(lat)) # filas sin latitud #
  conlat <- rep(NA, length(lat))
  conlat[row.without.lat] <- 1 #Vector resultados #
  set3conlat <- cbind(set3, conlat)  # pega vector a tabla #
  
  rm(lat,row.without.lat)
  
  # NO LONGITUD
  row.without.lon <- which(!is.na(set3conlat$longitud)) ## filas sin longitud #
  conlon <- rep(NA, nrow(set3conlat))
  conlon[row.without.lon] <- 1 #Vector resultados #
  set5Coord <- cbind(set3conlat, conlon) # pega vector a tabla #
  set5 <- set5Coord[which(!is.na(set5Coord$conlon) | !is.na(set5Coord$conlat)), ] #
  
  rm(set3conlat, row.without.lon, conlat, conlon)
  
  # ### 6. CONCORDANCIA PAISES -------------------------------------------  
  cat("  2. Evaluando concordancia del pais (2 de 11)", "\n")
  
  ubicacion <- set5 # 
  coordinates(ubicacion)=~longitud+latitud #
  ubicacion <<- ubicacion
  ##evaluar otros paises
  
  bien_pais <- overlay(ubicacion, paises)# 10 minutos
  sugerencia_pais1 <- as.character(paises@data$NAME_0[bien_pais])
  sugerencia_pais2 <- as.character(paises@data$ISO2[bien_pais])#
  sugerencia_pais3 <- as.character(paises@data$ISO3[bien_pais])
  sugerencia_paises <- data.frame("NOMB" = unique(sugerencia_pais1), "ISO2" = unique(sugerencia_pais2), "ISO3" = unique(sugerencia_pais2))
  sugerencia_paises <- sugerencia_paises <- na.omit(sugerencia_paises)
  
  set5$bienPais <- 0 
  for (p in 1:nrow(sugerencia_paises)){
    set5$bienPais[grep(tolower(sugerencia_paises$NOMB[p]), tolower(set5$pais))] <- 1    
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO2[p]))] <- 1
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO3[p]))] <- 1
  }
  set5$bienPais[which(gsub(" ", "", set5$pais) == gsub(" ", "", sugerencia_pais1))] <- 1
  
  set5C <- cbind(set5, "sugerencia_pais" = sugerencia_pais1)#
  
  ubicacion<<-set5C #
  ubicacion <- set5C # debug add
  
  rm(bien_pais)
  
  coordinates(ubicacion)<-~longitud+latitud1 # debug
  
  # ##### 7. CONSISTENCIA GEOGR?FICA DEPARTAMENTOS MUNICIPIOS--------------------------------------
  #  DEPARTAMENTOS------
  cat("  3. Evaluando concordancia de departamentos (3 de 11)", "\n")
  IGeo <- set5C #
  DEP <- corroboracion(IGeo, paises, "NAME_1", "departamento") #12 seg
  bien_depto <- rep(NA, nrow(IGeo))
  bien_depto[IGeo[, 1] %in% DEP[[1]]] <- 1 #
  sugerencia_depto <- DEP[[3]] #
  set6 <- cbind(IGeo, bien_depto, sugerencia_depto) #
  
  ## ----MUNICIPIOS------
  
  ### Revisar "overlay" de los registros con el "Shape" de municipio
  cat("  4. Evaluando concordancia de municipios (3 de 11)", "\n")
  
  #Corroboracion municipios todos los paises
  IGeom <- set6 #
  A <- corroboracion(IGeom, paises, "NAME_2", "municipio")   
  bien_mun <- cbind(set6$id, rep(NA, nrow(IGeo)), "Mapa" = rep(NA, nrow(IGeo))) # 
  bien_mun[bien_mun[, 1] %in% A[[1]], 2] <- 1 #
  bien_mun[bien_mun[, 1] %in% A[[1]], 3] <- "Suramerica" #
  
  sugerencia_mun <- A[[3]] # 
  
  rm(DEP,bien_depto,sugerencia_depto, set5C, set5)
  rm(A, IGeo)
  
  if (routineType == "Colombia") {
    
    ##CORROBORACION CON MUNICIPIOS DE 1964
    #  set para colombia 
    cat("  4. Evaluando concordancia de municipios (1964) (4 de 11)", "\n")
    
    #selcciona los que no han pasado
    set6col <- subset(set6, set6$sugerencia_pais == "Colombia") #
    select <- bien_mun[which(is.na(bien_mun[, 2])), 1] #
    set6A <- set6col[which(set6col$id %in% select), ] #
    
    B <- corroboracion(set6A, mpios1964, "MPIOS", "municipio") #
    bien_mun[bien_mun[, 1] %in% B[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] %in% B[[1]], 3] <- 1964  
    
    rm(B)
    
    ##CORROBORACION CON MUNICIPIOS DE 1973
    
    cat("  5. Evaluando concordancia de municipios (1973) (5 de 11)", "\n")
    
    select <- bien_mun[which(is.na(bien_mun[,2])), 1] #
    set6A <- set6col[which(set6col$id%in% select), ] #
    C <- corroboracion(set6A, mpios1973, "MPIOS", "municipio")#
    bien_mun[bien_mun[, 1] %in% C[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] %in% C[[1]], 3] <- 1973
    
    rm(C, set6col)
    
    ##CORROBORACION CON MUNICIPIOS DE 1985
    
    cat("  6. Evaluando concordancia de municipios (1985) (6 de 11)", "\n")
    
    select <- which(is.na(bien_mun[, 2]))#
    set6A <- IGeom[select,]#
    D <- corroboracion(set6A, mpios1985, "MPIOS", "municipio")#
    bien_mun[bien_mun[, 1] %in% D[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] %in% D[[1]], 3] <- 1985
    
    rm(D)
    
    ##CORROBORACION CON MUNICIPIOS DE 1993
    cat("  7. Evaluando concordancia de municipios (1993) (7 de 11)", "\n")
    
    select <- which(is.na(bien_mun[,2]))#
    set6A <- IGeom[select,]#
    E <- corroboracion(set6A, mpios1993, "MPIOS", "municipio")#
    bien_mun[bien_mun[, 1] %in% E[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] %in% E[[1]], 3] <- 1993
    
    rm(E)
    
    ##CORROBORACION CON MUNICIPIOS DE 2003
    cat("  8. Evaluando concordancia de municipios (2003) (8 de 11)", "\n")
    
    select <- which(is.na(bien_mun[,2])) #
    set6A <- IGeom[select,] #
    G <- corroboracion(set6A, mpios2003, "MPIOS", "municipio") # 76'
    bien_mun[bien_mun[, 1] %in% G[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] %in% G[[1]], 3] <- 2003
    
    rm(G,set6, set6A)
    
    set8 <- cbind(IGeom, "bien_muni" = bien_mun[,2], sugerencia_mun) #
    rm(IGeom,bien_mun,sugerencia_mun)
    
    # # RURAL/URBANO ----------------------------------------------------------  
    #### Revisar si los registros se encuentran en areas urbanas o rurales
    
    cat("  9. Evaluando registros para areas rurales y urbanas (9 de 11)", "\n")
    
    IGeor <- set8 #
    
    coord8 <- cbind(IGeor$longitud,IGeor$latitud1) #
    
    mr <- SpatialPoints(coord8) #
    rm(coord8, IGeor)
    
    en_casco <- overlay(mr, casco) #
    rural <- rep("NA", nrow(set8))  #
    rural[which(is.na(en_casco))]=1 #
    
    over_mar <- overlay(mr,mar) #
    en_mar <- which(!is.na(over_mar))
    en_col <- which(set8$pais == "CO" | gsub(" ", "", tolower(set8$pais)) == "colombia")
    set8$bienPais[en_mar[en_mar %in% en_col]] <- 1  
    
    set10 <- cbind(set8, rural) #
    rm(rural, set8, en_casco)
    # #### 8. DUPLICADOS GEOGRAFICOS  -----------------------------------------
    
    cat("  10. Evaluando duplicados (10 de 11)", "\n")
    
    Igeodup <- set10  #
    coordinates(Igeodup)=~longitud+latitud1 #
    
    celda <- over(Igeodup, id) #
    
    select <- cbind(set10$id, set10$nombre, celda) #
    names(select) <- c("id", "nombre", "celda") #
    duplicados <- duplicated(select[which(select$nombre != "NA_NA"), 2:3]) #
    unidos <- as.data.frame(cbind(select[which(select$nombre != "NA_NA"), c(1, 3)], duplicados)) #
    unidos$duplicados[which(unidos$duplicados == 0)] <- NA #
    names(unidos) <- c("id", "celda", "duplicados") #
    
    set12 <- merge(set10, unidos, by="id", all=T) #
    rm(duplicados, set10, unidos)
    rm(Igeodup, celda, select)
    
    
    # ### 9. EXTREMOS EN ALTURA -----------------------------------------------
    cat("11. Evaluando elevaciones de los registros (11 de 11)", "\n")
    
    IGeoALT2 <- set12 #
    coordALT2 <- cbind(IGeoALT2$longitud,IGeoALT2$latitud1) #
    coordinates(IGeoALT2)=~longitud+latitud1 #
    prealt <-over(IGeoALT2,ALT) #
    colnames(prealt) <- "alt" #
    set12$alt <- as.numeric(prealt[,1])  #
    rm(coordALT2)
    
    # inicia metodo para detectar outliers (modified z-score method) 
    listsp <- unique(set12$nombre) #
    rm(IGeoALT2)
    
    preset16<-NULL #
    time.alt <- Sys.time()
    
    altDATA <- cbind(set12[, c("id", "nombre", "alt")], "extremo" = NA) 
    count <- 0
    time.alt <- Sys.time()
    for (w in 1:length(listsp)) # 
    {
      cat(w,"de",length(listsp), round((w/length(listsp))*100,2) ,"%",listsp[w])
      pos <- which(altDATA$nombre==listsp[w] & !is.na(altDATA$alt))
      v <- altDATA[pos, c("id", "alt")]; cat (" -", nrow(v), "registros")#; dim(DAT)
      if (nrow(v)>0){
        s <- median(v$alt,na.rm=T)
        N <- length(v$alt)
        m <- abs(v$alt-s) #debug add 
        MAD <- median(m, na.rm=T) # mediana de todos los datos |xi - xm | calculados
        if (MAD > 0) { 
          pZ <- cbind(v$id, abs(0.6745*(v$alt-s)/MAD)) # formula para calculo de outliers segun  # el modified z-score method
          Z <- which(pZ[, 2]>3.5)
          if (length(Z)!=0){
            altDATA$extremo[pos[-Z]] <- 1
            count <- count + 1
            cat(" - Extremo", count)
          } # 1 no es extremo
        }
      }; cat("\n")
    }
    
    Sys.time() - time.alt
    preset16 <- cbind(set12,extremo = altDATA$extremo)
    
    set16 <- preset16[order(preset16$id), ]
  } else {
    set16 <- cbind(IGeom, "bien_muni" = bien_mun[, 2], sugerencia_mun)
    set16 <- preset16[order(preset16$id), ]
  }
  
  p16 <- dim(set16)[1]
  
  set16$sugerencia_pais <- capitalize(set16$sugerencia_pais)
  set16$sugerencia_depto <- capitalize(set16$sugerencia_depto)
  set16$sugerencia_mun <- capitalize(set16$sugerencia_mun)
  
  # ####10.  PREPARAR SETS DE DATOS PARA CORRECCION  ----------------------------
  cat("  12. Escribiendo datos", "\n")
  
  pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)), ] 
  quitar.columnas <- which(colnames(set16)%in%c("id", "source", "nombre", "pais", "departamento", "municipio", "latitud", "latitud1", "longitud", "fecha_inicial", "localidad", "alt", "nombre_acept", "Nombre"))      
  set16 <<- cbind(set2[order(set2$ID), ],set16[order(set16$id), -quitar.columnas])
  set16 <- cbind(set2[order(set2$ID), ],set16[order(set16$id), -quitar.columnas])
  save(set16, file = paste0(root, "/", outPath, "/Registros.RData"))


  (p1 <- nrow(set16[which(!is.na(set16$nombre)), ]))
  (p2 <- nrow(set16[which(set16$bienPais == 1), ]))
  (p3 <- nrow(set16[which(set16$bien_depto == 1), ])) 
  (p4 <- nrow(set16[which(set16$bien_muni == 1), ]))
  (p5 <- nrow(set16[which(set16$rural == 1), ]))
  (p6 <- nrow(set16[which(set16$extremo == 1), ]))
  (p7 <- nrow(pasa_todo))
    
  cat("\n \n \n  Fin del codigo \n \n \n \n")
  
  col <- set16[which(set16$enAOI == 1), c("species","speciesOriginal","especie_aceptada","bienPais","bien_depto","bien_muni","rural","extremo")]
  resumen.salida <- rbind(
    c(nrow(set16), nrow(col)),
    c(length(which(set16$bienPais == 1)), length(which(col$bienPais==1))),
    c(length(which(set16$bienPais == 1 & set16$bien_depto == 1)), length(which(col$bienPais == 1 & col$bien_depto == 1))),
    c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1)), length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1))),
    c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural==1)), length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1 & col$rural==1))),
    c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural==1 & is.na(set16$extremo))), length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1 & col$rural == 1 & is.na(col$extremo))))
  )
  colnames(resumen.salida) <- c("Area de estudio", "Colombia")
  rownames(resumen.salida) <- c("Total", "Pais", "Departamento", "Municipio", "Rural", "Altura")      
  write.csv(resumen.salida, paste0(root, "/", outPath, "/", "Resumen_salida_set16.csv"))
}

#### Ejecucion de la funcion

## Variables interactivas
refineType <- "species" # or 'region'
refreshDB <- TRUE # or FALSE
spRefine <- "Ara macao"
inShape <- NULL # or poligono de class() == "SpatialPolygonsDataFrame" y con gisFilter@proj4string@projargs == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
inFile <- NULL # or csv as "estructuraset16.csv"
routineType <- "Colombia" # or 'World'

verifGeo(root, routineType = "Colombia", inShape = NULL, set1Path = "set1/", set2Path = "set2/", set16Path = "set16/", mapsPath = "maps/")

