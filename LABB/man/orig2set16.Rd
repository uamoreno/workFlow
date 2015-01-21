\name{orig2set16}
\alias{orig2set16}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orig2set16(archivo.i, format, fuente = NULL, occID = NULL, cleanscinames = TRUE, startID = 0, cleanCoords = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{archivo.i}{
%%     ~~Describe \code{archivo.i} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{fuente}{
%%     ~~Describe \code{fuente} here~~
}
  \item{occID}{
%%     ~~Describe \code{occID} here~~
}
  \item{cleanscinames}{
%%     ~~Describe \code{cleanscinames} here~~
}
  \item{startID}{
%%     ~~Describe \code{startID} here~~
}
  \item{cleanCoords}{
%%     ~~Describe \code{cleanCoords} here~~
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
function (archivo.i, format, fuente = NULL, occID = NULL, cleanscinames = TRUE, 
    startID = 0, cleanCoords = FALSE) 
{
    format.i <- as.data.frame(matrix("", nrow(archivo.i), ncol(format)))
    dim(format)
    colnames(format.i) <- colnames(format)
    j <- 1
    for (j in 1:ncol(format.i)) {
        (pos <- which(colnames(archivo.i) == colnames(format)[j]))
        if (length(pos) != 0) {
            format.i[, j] <- as.character(archivo.i[, pos])
        }
    }
    if (!is.null(fuente)) {
        format.i$source <- fuente
        if (is.null(occID)) {
            format.i$occurrenceID <- paste0(fuente, "-", startID:(startID + 
                nrow(format.i) - 1))
        }
    }
    if (!is.null(occID)) {
        format.i$occurrenceID <- occID
    }
    if (cleanCoords != TRUE) {
        if (length(which(colnames(format.i) == "lat")) > 0) {
            format.i$lat <- as.numeric(format.i$lat)
            format.i$lon <- as.numeric(format.i$lon)
            no_coords <- unique(append(which(is.na(format.i$lat)), 
                which(is.na(format.i$lon))))
            if (length(no_coords) > 0) {
                format.i <- format.i[-no_coords, ]
            }
        }
    }
    if (cleanscinames == TRUE) {
        format.i$species <- cleanSciNames(gsub("_", " ", format.i$speciesOriginal))
    }
    else {
        format.i$species <- format.i$speciesOriginal
    }
    return(format.i)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
