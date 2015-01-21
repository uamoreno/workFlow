\name{corroboracion}
\alias{corroboracion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corroboracion(datos, mun, layer.field, data.field)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
%%     ~~Describe \code{datos} here~~
}
  \item{mun}{
%%     ~~Describe \code{mun} here~~
}
  \item{layer.field}{
%%     ~~Describe \code{layer.field} here~~
}
  \item{data.field}{
%%     ~~Describe \code{data.field} here~~
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
function (datos, mun, layer.field, data.field) 
{
    (a <- Sys.time())
    cat("    i. Coordenadas \n")
    coordinates(datos) = ~longitud + latitud1
    cat("   ii. Overlay \n")
    ovm <- overlay(datos, mun)
    assign("cntrm", eval(parse(text = paste0("as.character(mun@data$", 
        layer.field, "[ovm])"))))
    assign("l", eval(parse(text = paste0("length(which(!is.na(datos@data$", 
        data.field, ")))"))))
    if (l == 1) {
        assign("tmp", eval(parse(text = paste0("agrep(mun$", 
            layer.field, "[ovm], datos$", data.field, ", max = 1, value=F, ignore.case=T)"))))
        mmx <- c(1, 1, 1)[tmp]
        nmx <- NA
    }
    else if (l > 0) {
        assign("jmx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) == gsub(' ', '', tolower(datos@data$", 
            data.field, ")))"))))
        assign("imx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) != gsub(' ', '', tolower(datos@data$", 
            data.field, ")))"))))
        na.mx <- which(is.na(cntrm))
        id.exa <- datos@data$id[jmx]
        CompareMun <- cbind(imx, cntrm[imx], datos@data$municipio[imx], 
            datos@data$id[imx])
        assign("CompareMun", eval(parse(text = paste0("cbind(imx, cntrm[imx], datos@data$", 
            data.field, "[imx], datos@data$id[imx])"))))
        uniqueMun <- (sort(unique(CompareMun[, 3])))
        (uniqueMun <- uniqueMun[which(!is.na(uniqueMun) & uniqueMun != 
            "")])
        mmx <- c(0, 0)
        nmx <- c(0, 0)
        if (length(uniqueMun) > 0) {
            cat("  iii. Ciclo \n")
            ma1x <- NULL
            cat("  iii. Ciclo -", i, "de", length(uniqueMun), 
                "-", round(i/length(uniqueMun), 2) * 100, "\% \n")
            for (i in 1:length(uniqueMun)) {
                (uniqueMun[i])
                (pos.mun <- which(CompareMun[, 3] == uniqueMun[i]))
                (mun.i <- CompareMun[pos.mun, 2])
                (tmp <- agrep(gsub(" ", "", uniqueMun[i]), gsub(" ", 
                  "", mun.i), max = 2, value = F, ignore.case = T))
                (max <- cbind(as.integer(CompareMun[pos.mun, 
                  4]), 0))
                (max[tmp, 2] <- 1)
                (ma1x <- rbind(ma1x, max))
            }
            cat("  iii. Ciclo -", i, "de", length(uniqueMun), 
                "-", round(i/length(uniqueMun), 2) * 100, "\% \n")
            lmx <- ma1x[which(ma1x[, 2] == 1), 1]
            mmx <- sort(as.integer(c(id.exa, lmx)))
            kmx <- ma1x[which(ma1x[, 2] == 0), 1]
            nmx <- sort(c(imx, kmx, na.mx))
        }
        if (length(id.exa) > 0 | length(uniqueMun) <= 0) {
            mmx <- sort(as.integer(c(id.exa)))
            nmx <- sort(c(na.mx))
        }
    }
    else {
        mmx <- rep(0, nrow(datos))
        nmx <- rep(NA, nrow(datos))
    }
    Xx <- list()
    Xx[[1]] <- mmx
    Xx[[2]] <- nmx
    Xx[[3]] <- cntrm
    return(Xx)
    cat(print(a - Sys.time()))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
