\name{verifGeo}
\alias{verifGeo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
verifGeo(root, routineType = "Colombia", inShapePath = NULL, set1Path = "set1/", set2Path = "set2/", set16Path = "set16/", mapsPath = "maps/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{routineType}{
%%     ~~Describe \code{routineType} here~~
}
  \item{inShapePath}{
%%     ~~Describe \code{inShapePath} here~~
}
  \item{set1Path}{
%%     ~~Describe \code{set1Path} here~~
}
  \item{set2Path}{
%%     ~~Describe \code{set2Path} here~~
}
  \item{set16Path}{
%%     ~~Describe \code{set16Path} here~~
}
  \item{mapsPath}{
%%     ~~Describe \code{mapsPath} here~~
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
function (root, routineType = "Colombia", inShapePath = NULL, 
    set1Path = "set1/", set2Path = "set2/", set16Path = "set16/", 
    mapsPath = "maps/") 
{
    library(dismo)
    library(maptools)
    library(sp)
    library(maps)
    library(svDialogs)
    library(xlsx)
    library(xlsxjars)
    library(rJava)
    library(R.utils)
    if (!is.null(inShapePath)) {
        zipFile <- list.files(path = inShapePath, pattern = ".zip")
        unzip(zipfile = paste0(InShapePath, "/", zip), exdir = InShapePath)
        shpFile <- list.files(path = inShapePath, pattern = ".dbf")
        inShape <- readOGR(inShapePath, substr(shpFile, 0, nchar(shpFile) - 
            4))
    }
    else {
        inShape <- NULL
    }
    load(paste0(root, "/", set1Path, "/set1.RData"))
    aoiFilter(registros, root, routineType = "Colombia", inShape, 
        outPath = set2Path, taxPath = "maps/TAX.RData")
    load(paste0(root, "/", set2Path, "/set2.RData"))
    set3 <- set2[, c("ID", "source", "especie_aceptada", "country", 
        "adm1", "adm2", "lat", "lon", "earliestDateCollected", 
        "locality")]
    colnames(set3) <- c("id", "source", "nombre", "pais", "departamento", 
        "municipio", "latitud", "longitud", "fecha_inicial", 
        "localidad")
    RESULTADOS <- VERIFICACION_PAISES(set3, routineType = "Colombia", 
        mapsPath = "/maps", outPath = set16Path)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
