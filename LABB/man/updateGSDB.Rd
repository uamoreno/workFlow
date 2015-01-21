\name{updateGSDB}
\alias{updateGSDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateGSDB(root, aoi, resume = FALSE, resumeObj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{aoi}{
%%     ~~Describe \code{aoi} here~~
}
  \item{resume}{
%%     ~~Describe \code{resume} here~~
}
  \item{resumeObj}{
%%     ~~Describe \code{resumeObj} here~~
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
function (root, aoi, resume = FALSE, resumeObj) 
{
    library(raster)
    library(sp)
    library(XML)
    pathGBIF <- paste(root, "/gbif", sep = "")
    pathSIB <- paste(root, "/sib", sep = "")
    dir.create(pathGBIF, showWarnings = FALSE, recursive = T)
    dir.create(pathSIB, showWarnings = FALSE, recursive = T)
    a <- matrix(0:64799, nrow = 180, ncol = 360, byrow = TRUE)
    c <- a[nrow(a):1, ]
    gbifWorld <- raster(c, xmn = -180, xmx = 180, ymn = -90, 
        ymx = 90, crs = "+proj=longlat +datum=WGS84")
    rm(a, c)
    aoi_raster <- rasterize(aoi, gbifWorld, getCover = T, background = 0)
    plot(aoi_raster, xlim = c(-85, -50), ylim = c(-18, 15), legend = F)
    gb_cellids <- Which(aoi_raster > 0, cells = TRUE)
    xy_cellids <- xyFromCell(aoi_raster, gb_cellids)
    cellids <- extract(gbifWorld, xy_cellids)
    rm(aoi_raster, gb_cellids, xy_cellids)
    cellobjs <- paste("c", cellids, sep = "")
    start1 = 1
    start2 = 1
    step1 = TRUE
    step2 = TRUE
    if (resume) {
        step1 = resumeObj$step1
        step2 = resumeObj$step2
        if (step1) {
            start1 = resumeObj$errorIter
            start2 = 1
        }
        if (step2) {
            start2 = resumeObj$errorIter
        }
    }
    if (step1) {
        gbif <- dwnData(cellids, start = start1, path = pathGBIF, 
            base = "http://data.gbif.org/ws/rest/occurrence/")
        if (gbif$isError) {
            print("Stopping execution of updateGSDB. Use resume=TRUE to continue")
            return(c(gbif, step1 = TRUE, step2 = TRUE))
        }
    }
    if (step2) {
        sib <- dwnData(cellids, start = start2, path = pathSIB, 
            base = "http://data.sibcolombia.net/ws/rest/occurrence/")
        if (sib$isError) {
            print("Stopping execution of updateGSDB. Use resume=TRUE to continue")
            return(c(sib, step1 = FALSE, step2 = TRUE))
        }
    }
    z <- NULL
    for (i in 1:length(cellobjs)) {
        print(paste0("GBIF - ", cellobjs[i], ".txt - ", round(i/length(cellobjs) * 
            100, 1), "\%"))
        if (!file.exists(paste0(pathGBIF, "/", cellobjs[i], ".txt"))) {
            next
        }
        inFile <- tryCatch(read.table(paste0(pathGBIF, "/", cellobjs[i], 
            ".txt"), header = TRUE, as.is = TRUE), error = function(e) {
            error.read <- FALSE
        })
        if (class(inFile) != "logical") {
            z <- rbind(z, cbind(source = "GBIF", inFile))
        }
        if (class(inFile) == "logical") {
            cat("FALSO")
            table.error <- read.table(paste0(pathGBIF, "/", cellobjs[i], 
                ".txt"), header = TRUE, stringsAsFactors = FALSE, 
                fill = TRUE)
            dim(table.error)
            table.error2 <- cbind(source = "GBIF", table.error[, 
                1:24])
            z <- rbind(z, table.error2)
        }
        z <- rbind(z, cbind(source = "GBIF", inFile))
    }
    w <- NULL
    for (i in 1:length(cellobjs)) {
        print(paste0("SiB - ", cellobjs[i], ".txt - ", round(i/length(cellobjs) * 
            100, 1), "\%"))
        if (!file.exists(paste0(pathSIB, "/", cellobjs[i], ".txt"))) {
            next
        }
        inFile <- tryCatch(read.table(paste0(pathSIB, "/", cellobjs[i], 
            ".txt"), header = TRUE, as.is = TRUE), error = function(e) {
            error.read <- FALSE
        })
        if (class(inFile) != "logical") {
            w <- rbind(w, cbind(source = "SiB", inFile))
        }
        if (class(inFile) == "logical") {
            cat("FALSO")
            table.error <- read.table(paste0(pathSIB, "/", cellobjs[i], 
                ".txt"), header = TRUE, stringsAsFactors = FALSE, 
                fill = TRUE)
            dim(table.error)
            table.error2 <- cbind(source = "SiB", table.error[, 
                1:24])
            w <- rbind(w, table.error2)
        }
        w <- rbind(w, cbind(source = "SiB", inFile))
    }
    rm(i, inFile)
    head(z)
    filterAOI <- function(dt, aoi) {
        dt_aoi <- data.frame(lon = dt$lon, lat = dt$lat)
        coordinates(dt_aoi) = ~lon + lat
        projection(dt_aoi) <- projection(aoi)
        ind <- overlay(dt_aoi, aoi)
        return(dt[-is.na(ind), ])
    }
    zFilt <- filterAOI(z, aoi)
    wFilt <- filterAOI(w, aoi)
    db <- rbind(zFilt, wFilt)
    return(db)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
