\name{gbifxmlToDataFrame2}
\alias{gbifxmlToDataFrame2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gbifxmlToDataFrame2(s)
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
    doc = xmlInternalTreeParse(s)
    nodes <- getNodeSet(doc, "//to:TaxonOccurrence")
    ids <- getNodeSet(doc, "to:TaxonOccurrence")
    if (length(nodes) == 0) 
        return(data.frame())
    varNames <- c("occurrenceID", "continent", "country", "stateProvince", 
        "county", "locality", "decimalLatitude", "decimalLongitude", 
        "coordinateUncertaintyInMeters", "maximumElevationInMeters", 
        "minimumElevationInMeters", "maximumDepthInMeters", "minimumDepthInMeters", 
        "institutionCode", "collectionCode", "catalogNumber", 
        "basisOfRecordString", "collector", "earliestDateCollected", 
        "latestDateCollected", "gbifNotes")
    dims <- c(length(nodes), length(varNames))
    ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), 
        dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
    names(ans) <- varNames
    for (i in seq(length = dims[1])) {
        ans[i, ] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
        ans[i, "occurrenceID"] <- xmlToList(nodes[[i]])$.attrs@.Data[1]
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
    cbind(occurrenceID = ans[, 1], speciesOriginal = tax, species = tax, 
        ans[, -1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
