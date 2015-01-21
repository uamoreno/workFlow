\name{dbDownload}
\alias{dbDownload}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dbDownload(root, outPath = "set0/", refineType = "species", refreshDB = FALSE, spRefine = NULL, inShapePath = NULL, inFilePath = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
}
  \item{refineType}{
%%     ~~Describe \code{refineType} here~~
}
  \item{refreshDB}{
%%     ~~Describe \code{refreshDB} here~~
}
  \item{spRefine}{
%%     ~~Describe \code{spRefine} here~~
}
  \item{inShapePath}{
%%     ~~Describe \code{inShapePath} here~~
}
  \item{inFilePath}{
%%     ~~Describe \code{inFilePath} here~~
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
function (root, outPath = "set0/", refineType = "species", refreshDB = FALSE, 
    spRefine = NULL, inShapePath = NULL, inFilePath = NULL) 
{
    library(maptools)
    library(raster)
    library(rvertnet)
    if (refreshDB == TRUE) {
        if (refineType == "region") {
            zip <- list.files(path = inShapePath, pattern = ".zip")
            unzip(zipfile = paste0(inShapePath, "/", zip), exdir = inShapePath)
            shape.file <- gsub(".shp", "", list.files(path = InShapePath, 
                pattern = ".shp"))
            aoi <- readOGR(InShapePath, shape.file)
            registros <- updateGSDB(root, aoi)
            registros <- cbind(ID = c(1:nrow(registros)), registros)
            registros$species <- cleanSciNames(registros$species)
        }
        else if (refineType == "species") {
            gen <- strsplit(spRefine, " ")[[1]][1]
            spe <- strsplit(spRefine, " ")[[1]][2]
            baseGBIF <- "http://data.gbif.org/ws/rest/occurrence/"
            baseSiB <- "http://data.sibcolombia.net/ws/rest/occurrence/"
            (spec <- paste0(gen, "+", spe))
            urlGBIF <- paste(baseGBIF, "list?scientificname=", 
                spec, "&coordinateissues=true&coordinatestatus=true&mode=processed&format=darwin", 
                sep = "")
            urlSiB <- paste(baseSiB, "list?scientificname=", 
                spec, "&coordinateissues=true&coordinatestatus=true&mode=processed&format=darwin", 
                sep = "")
            webGBIF <- try(readLines(urlGBIF, warn = FALSE))
            webSiB <- try(readLines(urlSiB, warn = FALSE))
            dataGBIF <- try(gbifxmlToDataFrame2(webGBIF))
            dataSiB <- try(gbifxmlToDataFrame2(webSiB))
            dataGBIF <- cbind(source = "GBIF", dataGBIF)
            dataSiB <- cbind(source = "SiB", dataSiB)
            dataVer <- verRecords(spRefine)
            urlSpL <- paste0("http://splink2humboldt.services.cria.org.br/retrieve/userid/humboldt/genus/", 
                gen, "/species/", spe)
            webSpL <- try(readLines(urlSpL, warn = FALSE))
            dataSpL <- try(gbifxmlToDataFrame3(webSpL))
            registros <- rbind(dataGBIF, dataSiB, dataSpL)
            if (class(dataVer) != "logical") {
                registros <- rbind(registros, dataVer)
            }
            colnames(registros) <- c("source", "occurrenceID", 
                "speciesOriginal", "species", "continent", "country", 
                "adm1", "adm2", "locality", "lat", "lon", "coordUncertaintyM", 
                "maxElevationM", "minElevationM", "maxDepthM", 
                "minDepthM", "institution", "collection", "catalogNumber", 
                "basisOfRecord", "collector", "earliestDateCollected", 
                "latestDateCollected", "gbifNotes")
            registros$occurrenceID <- as.character(registros$occurrenceID)
            registros$species <- cleanSciNames(registros$speciesOriginal)
        }
    }
    else if (!is.null(inFilePath) & refreshDB != TRUE) {
        InFile <- read.csv(inFilePath)
        registros <- rbind(registros, InFile)
    }
    save(registros, file = paste0(root, "/", outPath, "/set0.RData"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
