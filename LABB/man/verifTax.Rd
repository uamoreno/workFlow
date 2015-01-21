\name{verifTax}
\alias{verifTax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
verifTax(root, set0Path = "set0/", outPath = "set1/", .user = "root", .password = "root", .dbname = "col2012ac", .host = "localhost", registros = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{set0Path}{
%%     ~~Describe \code{set0Path} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
}
  \item{.user}{
%%     ~~Describe \code{.user} here~~
}
  \item{.password}{
%%     ~~Describe \code{.password} here~~
}
  \item{.dbname}{
%%     ~~Describe \code{.dbname} here~~
}
  \item{.host}{
%%     ~~Describe \code{.host} here~~
}
  \item{registros}{
%%     ~~Describe \code{registros} here~~
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
function (root, set0Path = "set0/", outPath = "set1/", .user = "root", 
    .password = "root", .dbname = "col2012ac", .host = "localhost", 
    registros = NULL) 
{
    load(paste0(root, "/", set0Path, "/set0.RData"))
    solo_nombres <- as.data.frame(registros$species)
    cofTable <- t(apply(solo_nombres, 1, FUN = function(y) {
        (pos <- regexpr(" ", y)[[1]][1])
        (genus <- substr(y, 0, pos - 1))
        (sp <- substr(y, pos + 1, nchar(y)))
        (file <- cbind(y, genus, sp))
        return(t(file))
    }))
    id <- 1:nrow(cofTable)
    cofTable <- as.data.frame(cbind(id, cofTable))
    nombres <- c("id", "nombre", "genero", "epiteto_especifico")
    colnames(cofTable) <- nombres
    rownames(cofTable) <- c(1:nrow(cofTable))
    library(RMySQL)
    con <- dbConnect(dbDriver("MySQL"), user = .user, password = .password, 
        dbname = .dbname, host = .host)
    cofTable2 <- nameValidation(con, cofTable)
    especie_aceptada <- paste0(cofTable2$genero_aceptado, " ", 
        cofTable2$epiteto_aceptado)
    cofTable2 <- cbind(cofTable2, especie_aceptada)
    datos <- registros
    datos$ID <- 1:nrow(datos)
    cofTable3 <- merge(datos, cofTable2, by.x = "ID", by.y = "id")
    registros <- cofTable3
    save(registros, file = paste0(root, "/", outPath, "/set1.RData"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
