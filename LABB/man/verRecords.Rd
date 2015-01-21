\name{verRecords}
\alias{verRecords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
verRecords(sp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
%%     ~~Describe \code{sp} here~~
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
function (sp) 
{
    lapp <- lapply(c("fish", "bird", "herp"), FUN = function(y) {
        vert <- tryCatch(vertoccurrence(t = sp, grp = y), error = function(e) {
            FALSE
        })
        if (class(vert) == "data.frame") {
            i <- sapply(vert, is.factor)
            vert[i] <- lapply(vert[i], as.character)
            mat <- cbind(source = "VertNet", occurrenceID = NA, 
                vert[, c("ScientificName", "ScientificName", 
                  "Country", "Country", "StateProvince", "County", 
                  "Locality", "Latitude", "Longitude", "CoordinateUncertaintyInMeters", 
                  "VerbatimElevation", "VerbatimElevation", "VerbatimDepth", 
                  "VerbatimDepth", "InstitutionCode", "CollectionCode", 
                  "CatalogNumber", "BasisOfRecord", "Collector")], 
                paste(vert$YearCollected, "-", vert$MonthCollected, 
                  "-", vert$DayCollected, sep = ""), paste(vert$YearCollected, 
                  "-", vert$MonthCollected, "-", vert$DayCollected, 
                  sep = ""), paste0(vert$Remarks, ";", vert$Tissues))
            colnames(mat) <- colnames(dataGBIF)
            return(mat)
        }
    })
    good <- which(!(sapply(lapp, is.null)))
    if (length(good) > 0) {
        return(lapp[[good]])
    }
    else {
        NA
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
