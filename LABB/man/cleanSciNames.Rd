\name{cleanSciNames}
\alias{cleanSciNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cleanSciNames(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (y) 
{
    require(R.utils)
    if (!require(R.utils)) {
        stop("You need to install the 'R.utils' package to use this function")
    }
    y <- sapply(y, FUN = function(x) {
        (x <- gsub("_", "", gsub(" _", "", gsub("  _", "", paste0(x, 
            "_")))))
        (x <- gsub("_", " ", gsub("/", " ", x)))
        (x <- gsub("\n", " n", x))
        if (gregexpr("\\).*", x)[[1]][1] != -1) {
            (x <- gsub("_", " ", x))
            ip <- gregexpr("\\(.*", x)[[1]][1]
            fp <- gregexpr("\\).*", x)[[1]][1]
            x1 <- substr(x, 0, ip - 2)
            x2 <- substr(x, fp + 1, nchar(x))
            x <- paste0(x1, x2)
        }
        (x <- gsub(" cf\\.", "", x))
        (x <- gsub(" cf ", " ", x))
        (x <- gsub(" var\\.", "", x))
        (x <- gsub(" var ", " ", x))
        (x <- gsub("sp\\.", "", x))
        (x <- gsub(" sp\\.", "", x))
        (x <- gsub(" sp ", "", x))
        (x <- gsub(" aff\\.", "", x))
        (x <- gsub(" affin\\.", "", x))
        (x <- gsub(" aff ", "", x))
        (x <- gsub(" affin ", "", x))
        (x <- gsub("  ", " ", x))
        if (!is.na(gregexpr(" ", x)[[1]][2])) {
            pos <- gregexpr(" ", x)[[1]][2]
            (x <- substr(x, 0, pos - 1))
        }
        return(capitalize(x))
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
