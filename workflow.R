root <- "C:/IAvH/DINAVIS_set16/scriptsUniandes/"
scriptsPath <- "scripts"

## Variables interactivas
refineType <- "species" # or 'region'
refreshDB <- TRUE # or FALSE
spRefine <- "Ara macao"
inShape <- NULL # or poligono de class() == "SpatialPolygonsDataFrame" y con gisFilter@proj4string@projargs == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
inFile <- NULL # or csv as "estructuraset16.csv"
routineType <- "Colombia" # or 'World'

source(paste0(root,"/", scriptsPath, "/dbDownload.R"))
updateLBAB(root, outPath = "set0/", refineType = "species", refreshDB = TRUE, spRefine = "Ara macao", inShape = NULL, inFile = NULL)

source(paste0(root,"/", scriptsPath, "/verifTax.R"))
verifTax(root, set0Path = "set0/", outPath = "set1/", .user = "root", .password = "root", .dbname = "col2012ac", .host = "localhost", registros  = NULL)

source(paste0(root,"/", scriptsPath, "/verifGeo.R"))
verifGeo(root, routineType = "Colombia", inShape = NULL, set1Path = "set1/", set2Path = "set2/", set16Path = "set16/", mapsPath = "maps/")

