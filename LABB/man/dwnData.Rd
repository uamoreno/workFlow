\name{dwnData}
\alias{dwnData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dwnData(cellids, start = 1, path, base)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cellids}{
%%     ~~Describe \code{cellids} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
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
function (cellids, start = 1, path, base) 
{
    for (iter in start:length(cellids)) {
        print(paste("Working on cell", cellids[iter], "-", iter, 
            "of", length(cellids), "cells"))
        print(base)
        out <- tryCatch(gbif2(cellid = cellids[iter], base = base), 
            error = function(e) {
                errorIter = iter
                print(paste0("Code stopped at cell ", cellids[iter]))
                return(errorIter)
            })
        if (is.data.frame(out)) {
            write.table(out, paste(path, "/c", cellids[iter], 
                ".txt", sep = ""), row.names = FALSE)
            next
        }
        if (is.null(out)) {
            next
        }
        else {
            return(list(isError = TRUE, errorIter = out))
        }
    }
    return(list(isError = FALSE))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
