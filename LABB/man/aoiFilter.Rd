\name{aoiFilter}
\alias{aoiFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aoiFilter(set2, root, routineType = "Colombia", inShape = NULL, outPath = "/set2/", taxPath = "maps/TAX.RData")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set2}{
%%     ~~Describe \code{set2} here~~
}
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{routineType}{
%%     ~~Describe \code{routineType} here~~
}
  \item{inShape}{
%%     ~~Describe \code{inShape} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
}
  \item{taxPath}{
%%     ~~Describe \code{taxPath} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (set2, root, routineType = "Colombia", inShape = NULL, 
    outPath = "/set2/", taxPath = "maps/TAX.RData") 
{
    gc()
    memory.limit(size = 1e+06)
    library(raster)
    library(sp)
    set2 <- registros
    rm(registros)
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
    if (class(inShape) == "SpatialPolygonsDataFrame") {
        library(maptools)
        library(rgdal)
        inShape$inAoi <- 1
        coords <- set2[, c("ID", "source", "species", "speciesOriginal", 
            "lat", "lon", "especie_aceptada")]
        coordinates(coords) = ~lon + lat
        coords@proj4string@projargs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        over.coords <- over(coords, inShape)
        set2 <- cbind(set2, enAOI = over.coords$inAoi)
        world <- readShapePoly("C:/IAvH/VERIFICACION PAISES VECINOS/Info geografica/PAISES_COMPLETO5.shp")
        nsame <- readShapePoly("C:/Users/GIC 76/Downloads/Mapas/gadm2.shp")
        tabla.over <- (table(set2$species, set2$enAOI))
        tabla.overO <- (table(set2$speciesOriginal, set2$enAOI))
        tabla.overV <- (table(set2$especie_aceptada, set2$enAOI))
        tabla.over <- cbind(tabla.over, rowSums(tabla.over), 
            rowSums(tabla.over) > 0)
        tabla.overO <- cbind(tabla.overO, rowSums(tabla.overO), 
            rowSums(tabla.overO) > 0)
        tabla.overV <- cbind(tabla.overV, rowSums(tabla.overV), 
            rowSums(tabla.overV) > 0)
        sp.col <- rownames(tabla.over)[unique(c(which(tabla.over[, 
            3] > 0)))]
        sp.colO <- rownames(tabla.overO)[unique(c(which(tabla.overO[, 
            3] > 0)))]
        sp.colV <- rownames(tabla.overV)[unique(c(which(tabla.overV[, 
            3] > 0), which(rownames(tabla.overV) == "NA NA")))]
        sp.pos <- which(set2$species \%in\% sp.col)
        sp.posO <- which(set2$speciesOriginal \%in\% sp.colO)
        sp.posV <- which(set2$especie_aceptada \%in\% sp.colV)
        indexspcol <- unique(c(sp.pos, sp.posO, sp.posV))
        if (routineType == "Colombia") {
            load(paste(root, "/", taxPath))
            sp.TAX1 <- which(set2$species \%in\% TAX$scientificName)
            sp.TAX2 <- which(set2$speciesOriginal \%in\% TAX$scientificName)
            sp.TAX3 <- which(set2$especie_aceptada \%in\% TAX$scientificName)
            sp.TAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3))
            indexspcol <- unique(c(indexspcol, sp.TAX))
        }
        set2 <- set2[indexspcol, ]
    }
    else if (is.null(inShape) & routineType == "Colombia") {
        load(paste0(root, "/", taxPath))
        sp.TAX1 <- which(set2$species \%in\% TAX$scientificName)
        sp.TAX2 <- which(set2$speciesOriginal \%in\% TAX$scientificName)
        sp.TAX3 <- which(set2$especie_aceptada \%in\% TAX$scientificName)
        sp.TAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3))
        indexspcol <- unique(c(sp.TAX))
        set2 <- set2[indexspcol, ]
    }
    set2$ID <- 1:nrow(set2)
    indDup <- paste(set2$collection, set2$catalogNumber, sep = "-")
    set2$DupIndex <- 0
    dup <- duplicated(indDup, fromLast = TRUE)
    dupNA <- which(indDup == "NA-NA" | indDup == "-")
    set2$DupIndex[dup] <- 1
    set2$DupIndex[dupNA] <- 0
    set2$taxDoubdt <- 0
    cf <- grep(" cf\\.", set2$speciesOriginal)
    cf2 <- grep(" cf ", set2$speciesOriginal)
    inter <- grep("\\?", set2$speciesOriginal)
    taxDoubdt <- unique(c(cf, cf2, inter))
    set2$taxDoubdt[taxDoubdt] <- 1
    save(set2, file = paste0(root, "/", outPath, "/set2.RData"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
