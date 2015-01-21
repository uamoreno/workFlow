\name{gbifxmlToDataFrame3}
\alias{gbifxmlToDataFrame3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gbifxmlToDataFrame3(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
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
function (s) 
{
    require(XML)
    doc <- xmlInternalTreeParse(s)
    nodes <- getNodeSet(doc, "//record")
    ids <- getNodeSet(doc, "//record")
    if (length(nodes) == 0) 
        return(data.frame())
    varNames <- c("source", "occurrenceID", "scientificname", 
        "scientificname", "continent", "country", "stateprovince", 
        "county", "locality", "latitude", "longitude", "coordinateUncertaintyInMeters", 
        "maximumElevationInMeters", "minimumElevationInMeters", 
        "maximumDepthInMeters", "minimumDepthInMeters", "institutionCode", 
        "collectioncode", "catalognumber", "basisOfRecordString", 
        "collector", "earliestDateCollected", "latestDateCollected", 
        "gbifNotes")
    dims <- c(length(nodes), length(varNames))
    ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), 
        dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
    names(ans) <- varNames
    for (i in seq(length = dims[1])) {
        ans[i, ] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
        ans[i, "occurrenceID"] <- xmlToList(nodes[[i]])$.attrs@.Data[1]
        ans[i, "source"] <- "spLink"
    }
    colnames(ans) <- colnames(dataGBIF)
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
