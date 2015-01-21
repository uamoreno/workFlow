\name{VERIFICACION_PAISES}
\alias{VERIFICACION_PAISES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VERIFICACION_PAISES(set3, routineType = "Colombia", mapsPath = "/maps", outPath = "/set16")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set3}{
%%     ~~Describe \code{set3} here~~
}
  \item{routineType}{
%%     ~~Describe \code{routineType} here~~
}
  \item{mapsPath}{
%%     ~~Describe \code{mapsPath} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
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
function (set3, routineType = "Colombia", mapsPath = "/maps", 
    outPath = "/set16") 
{
    info_geografica(root, mapsPath = mapsPath, routineType = "Colombia")
    set3$latitud <- as.numeric(set3$latitud)
    set3$longitud <- as.numeric(set3$longitud)
    set3$municipio <- iconv(set3$municipio)
    set3$departamento <- iconv(set3$departamento)
    set3$localidad <- iconv(set3$localidad)
    cat("  1. Evaluando registros registros con coordenadas (1 de 11)", 
        "\n")
    set3$latitud1 <- set3$latitud
    lat <- set3$latitud1
    (p0 <- nrow(set3))
    row.without.lat <- which(!is.na(lat))
    conlat <- rep(NA, length(lat))
    conlat[row.without.lat] <- 1
    set3conlat <- cbind(set3, conlat)
    rm(lat, row.without.lat)
    row.without.lon <- which(!is.na(set3conlat$longitud))
    conlon <- rep(NA, nrow(set3conlat))
    conlon[row.without.lon] <- 1
    set5Coord <- cbind(set3conlat, conlon)
    set5 <- set5Coord[which(!is.na(set5Coord$conlon) | !is.na(set5Coord$conlat)), 
        ]
    rm(set3conlat, row.without.lon, conlat, conlon)
    cat("  2. Evaluando concordancia del pais (2 de 11)", "\n")
    ubicacion <- set5
    coordinates(ubicacion) = ~longitud + latitud
    ubicacion <<- ubicacion
    bien_pais <- overlay(ubicacion, paises)
    sugerencia_pais1 <- as.character(paises@data$NAME_0[bien_pais])
    sugerencia_pais2 <- as.character(paises@data$ISO2[bien_pais])
    sugerencia_pais3 <- as.character(paises@data$ISO3[bien_pais])
    sugerencia_paises <- data.frame(NOMB = unique(sugerencia_pais1), 
        ISO2 = unique(sugerencia_pais2), ISO3 = unique(sugerencia_pais2))
    sugerencia_paises <- sugerencia_paises <- na.omit(sugerencia_paises)
    set5$bienPais <- 0
    for (p in 1:nrow(sugerencia_paises)) {
        set5$bienPais[grep(tolower(sugerencia_paises$NOMB[p]), 
            tolower(set5$pais))] <- 1
        set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO2[p]))] <- 1
        set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO3[p]))] <- 1
    }
    set5$bienPais[which(gsub(" ", "", set5$pais) == gsub(" ", 
        "", sugerencia_pais1))] <- 1
    set5C <- cbind(set5, sugerencia_pais = sugerencia_pais1)
    ubicacion <<- set5C
    ubicacion <- set5C
    rm(bien_pais)
    coordinates(ubicacion) <- ~longitud + latitud1
    cat("  3. Evaluando concordancia de departamentos (3 de 11)", 
        "\n")
    IGeo <- set5C
    DEP <- corroboracion(IGeo, paises, "NAME_1", "departamento")
    bien_depto <- rep(NA, nrow(IGeo))
    bien_depto[IGeo[, 1] \%in\% DEP[[1]]] <- 1
    sugerencia_depto <- DEP[[3]]
    set6 <- cbind(IGeo, bien_depto, sugerencia_depto)
    cat("  4. Evaluando concordancia de municipios (3 de 11)", 
        "\n")
    IGeom <- set6
    A <- corroboracion(IGeom, paises, "NAME_2", "municipio")
    bien_mun <- cbind(set6$id, rep(NA, nrow(IGeo)), Mapa = rep(NA, 
        nrow(IGeo)))
    bien_mun[bien_mun[, 1] \%in\% A[[1]], 2] <- 1
    bien_mun[bien_mun[, 1] \%in\% A[[1]], 3] <- "Suramerica"
    sugerencia_mun <- A[[3]]
    rm(DEP, bien_depto, sugerencia_depto, set5C, set5)
    rm(A, IGeo)
    if (routineType == "Colombia") {
        cat("  4. Evaluando concordancia de municipios (1964) (4 de 11)", 
            "\n")
        set6col <- subset(set6, set6$sugerencia_pais == "Colombia")
        select <- bien_mun[which(is.na(bien_mun[, 2])), 1]
        set6A <- set6col[which(set6col$id \%in\% select), ]
        B <- corroboracion(set6A, mpios1964, "MPIOS", "municipio")
        bien_mun[bien_mun[, 1] \%in\% B[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] \%in\% B[[1]], 3] <- 1964
        rm(B)
        cat("  5. Evaluando concordancia de municipios (1973) (5 de 11)", 
            "\n")
        select <- bien_mun[which(is.na(bien_mun[, 2])), 1]
        set6A <- set6col[which(set6col$id \%in\% select), ]
        C <- corroboracion(set6A, mpios1973, "MPIOS", "municipio")
        bien_mun[bien_mun[, 1] \%in\% C[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] \%in\% C[[1]], 3] <- 1973
        rm(C, set6col)
        cat("  6. Evaluando concordancia de municipios (1985) (6 de 11)", 
            "\n")
        select <- which(is.na(bien_mun[, 2]))
        set6A <- IGeom[select, ]
        D <- corroboracion(set6A, mpios1985, "MPIOS", "municipio")
        bien_mun[bien_mun[, 1] \%in\% D[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] \%in\% D[[1]], 3] <- 1985
        rm(D)
        cat("  7. Evaluando concordancia de municipios (1993) (7 de 11)", 
            "\n")
        select <- which(is.na(bien_mun[, 2]))
        set6A <- IGeom[select, ]
        E <- corroboracion(set6A, mpios1993, "MPIOS", "municipio")
        bien_mun[bien_mun[, 1] \%in\% E[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] \%in\% E[[1]], 3] <- 1993
        rm(E)
        cat("  8. Evaluando concordancia de municipios (2003) (8 de 11)", 
            "\n")
        select <- which(is.na(bien_mun[, 2]))
        set6A <- IGeom[select, ]
        G <- corroboracion(set6A, mpios2003, "MPIOS", "municipio")
        bien_mun[bien_mun[, 1] \%in\% G[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] \%in\% G[[1]], 3] <- 2003
        rm(G, set6, set6A)
        set8 <- cbind(IGeom, bien_muni = bien_mun[, 2], sugerencia_mun)
        rm(IGeom, bien_mun, sugerencia_mun)
        cat("  9. Evaluando registros para areas rurales y urbanas (9 de 11)", 
            "\n")
        IGeor <- set8
        coord8 <- cbind(IGeor$longitud, IGeor$latitud1)
        mr <- SpatialPoints(coord8)
        rm(coord8, IGeor)
        en_casco <- overlay(mr, casco)
        rural <- rep("NA", nrow(set8))
        rural[which(is.na(en_casco))] = 1
        over_mar <- overlay(mr, mar)
        en_mar <- which(!is.na(over_mar))
        en_col <- which(set8$pais == "CO" | gsub(" ", "", tolower(set8$pais)) == 
            "colombia")
        set8$bienPais[en_mar[en_mar \%in\% en_col]] <- 1
        set10 <- cbind(set8, rural)
        rm(rural, set8, en_casco)
        cat("  10. Evaluando duplicados (10 de 11)", "\n")
        Igeodup <- set10
        coordinates(Igeodup) = ~longitud + latitud1
        celda <- over(Igeodup, id)
        select <- cbind(set10$id, set10$nombre, celda)
        names(select) <- c("id", "nombre", "celda")
        duplicados <- duplicated(select[which(select$nombre != 
            "NA_NA"), 2:3])
        unidos <- as.data.frame(cbind(select[which(select$nombre != 
            "NA_NA"), c(1, 3)], duplicados))
        unidos$duplicados[which(unidos$duplicados == 0)] <- NA
        names(unidos) <- c("id", "celda", "duplicados")
        set12 <- merge(set10, unidos, by = "id", all = T)
        rm(duplicados, set10, unidos)
        rm(Igeodup, celda, select)
        cat("11. Evaluando elevaciones de los registros (11 de 11)", 
            "\n")
        IGeoALT2 <- set12
        coordALT2 <- cbind(IGeoALT2$longitud, IGeoALT2$latitud1)
        coordinates(IGeoALT2) = ~longitud + latitud1
        prealt <- over(IGeoALT2, ALT)
        colnames(prealt) <- "alt"
        set12$alt <- as.numeric(prealt[, 1])
        rm(coordALT2)
        listsp <- unique(set12$nombre)
        rm(IGeoALT2)
        preset16 <- NULL
        time.alt <- Sys.time()
        altDATA <- cbind(set12[, c("id", "nombre", "alt")], extremo = NA)
        count <- 0
        time.alt <- Sys.time()
        for (w in 1:length(listsp)) {
            cat(w, "de", length(listsp), round((w/length(listsp)) * 
                100, 2), "\%", listsp[w])
            pos <- which(altDATA$nombre == listsp[w] & !is.na(altDATA$alt))
            v <- altDATA[pos, c("id", "alt")]
            cat(" -", nrow(v), "registros")
            if (nrow(v) > 0) {
                s <- median(v$alt, na.rm = T)
                N <- length(v$alt)
                m <- abs(v$alt - s)
                MAD <- median(m, na.rm = T)
                if (MAD > 0) {
                  pZ <- cbind(v$id, abs(0.6745 * (v$alt - s)/MAD))
                  Z <- which(pZ[, 2] > 3.5)
                  if (length(Z) != 0) {
                    altDATA$extremo[pos[-Z]] <- 1
                    count <- count + 1
                    cat(" - Extremo", count)
                  }
                }
            }
            cat("\n")
        }
        Sys.time() - time.alt
        preset16 <- cbind(set12, extremo = altDATA$extremo)
        set16 <- preset16[order(preset16$id), ]
    }
    else {
        set16 <- cbind(IGeom, bien_muni = bien_mun[, 2], sugerencia_mun)
        set16 <- preset16[order(preset16$id), ]
    }
    p16 <- dim(set16)[1]
    set16$sugerencia_pais <- capitalize(set16$sugerencia_pais)
    set16$sugerencia_depto <- capitalize(set16$sugerencia_depto)
    set16$sugerencia_mun <- capitalize(set16$sugerencia_mun)
    cat("  12. Escribiendo datos", "\n")
    pasa_todo <<- set16[which(set16$bienPais == 1 & set16$bien_depto == 
        1 & set16$bien_muni == 1 & set16$rural == 1 & is.na(set16$extremo)), 
        ]
    quitar.columnas <- which(colnames(set16) \%in\% c("id", "source", 
        "nombre", "pais", "departamento", "municipio", "latitud", 
        "latitud1", "longitud", "fecha_inicial", "localidad", 
        "alt", "nombre_acept", "Nombre"))
    set16 <<- cbind(set2[order(set2$ID), ], set16[order(set16$id), 
        -quitar.columnas])
    set16 <- cbind(set2[order(set2$ID), ], set16[order(set16$id), 
        -quitar.columnas])
    save(set16, file = paste0(root, "/", outPath, "/Registros.RData"))
    (p1 <- nrow(set16[which(!is.na(set16$nombre)), ]))
    (p2 <- nrow(set16[which(set16$bienPais == 1), ]))
    (p3 <- nrow(set16[which(set16$bien_depto == 1), ]))
    (p4 <- nrow(set16[which(set16$bien_muni == 1), ]))
    (p5 <- nrow(set16[which(set16$rural == 1), ]))
    (p6 <- nrow(set16[which(set16$extremo == 1), ]))
    (p7 <- nrow(pasa_todo))
    cat("\n \n \n  Fin del codigo \n \n \n \n")
    col <- set16[which(set16$enAOI == 1), c("species", "speciesOriginal", 
        "especie_aceptada", "bienPais", "bien_depto", "bien_muni", 
        "rural", "extremo")]
    resumen.salida <- rbind(c(nrow(set16), nrow(col)), c(length(which(set16$bienPais == 
        1)), length(which(col$bienPais == 1))), c(length(which(set16$bienPais == 
        1 & set16$bien_depto == 1)), length(which(col$bienPais == 
        1 & col$bien_depto == 1))), c(length(which(set16$bienPais == 
        1 & set16$bien_depto == 1 & set16$bien_muni == 1)), length(which(col$bienPais == 
        1 & col$bien_depto == 1 & col$bien_muni == 1))), c(length(which(set16$bienPais == 
        1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural == 
        1)), length(which(col$bienPais == 1 & col$bien_depto == 
        1 & col$bien_muni == 1 & col$rural == 1))), c(length(which(set16$bienPais == 
        1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural == 
        1 & is.na(set16$extremo))), length(which(col$bienPais == 
        1 & col$bien_depto == 1 & col$bien_muni == 1 & col$rural == 
        1 & is.na(col$extremo)))))
    colnames(resumen.salida) <- c("Area de estudio", "Colombia")
    rownames(resumen.salida) <- c("Total", "Pais", "Departamento", 
        "Municipio", "Rural", "Altura")
    write.csv(resumen.salida, paste0(root, "/", outPath, "/", 
        "Resumen_salida_set16.csv"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
