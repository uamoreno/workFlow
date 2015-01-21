\name{nameValidation}
\alias{nameValidation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nameValidation(con, inTable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
%%     ~~Describe \code{con} here~~
}
  \item{inTable}{
%%     ~~Describe \code{inTable} here~~
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
function (con, inTable) 
{
    dbSendQuery(con, "truncate tabla_trabajo")
    dbWriteTable(con, "gbif_sib", inTable, overwrite = TRUE)
    dbSendQuery(con, "update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"NA\"")
    dbSendQuery(con, "update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"\"")
    dbSendQuery(con, "update gbif_sib set genero = null where genero =\"\"")
    dbSendQuery(con, "delete from gbif_sib where id = \"\"")
    dbSendQuery(con, "INSERT INTO tabla_trabajo (id,nombre,genero,epiteto_especifico) select id,nombre,genero,epiteto_especifico from gbif_sib")
    dbSendQuery(con, "UPDATE Tabla_trabajo INNER JOIN CoL_sn_aceptados ON (Tabla_trabajo.genero = CoL_sn_aceptados.genus) AND (Tabla_trabajo.epiteto_especifico = CoL_sn_aceptados.species) SET Tabla_trabajo.autor_nombre_aceptado= col_sn_aceptados.author, Tabla_trabajo.es_aceptadoCoL = is_accepted_name, Tabla_trabajo.id_registro_CoL = col_sn_aceptados.id, Tabla_trabajo.id_familia_CoL = family_id, Tabla_trabajo.id_nombre_aceptado = accepted_species_id, Tabla_trabajo.genero_aceptado = genus, Tabla_trabajo.epiteto_aceptado = species WHERE (((CoL_sn_aceptados.infraspecies)=''))\n")
    dbSendQuery(con, "UPDATE Tabla_trabajo INNER JOIN CoL_sn ON (Tabla_trabajo.genero = CoL_sn.genus) AND (Tabla_trabajo.epiteto_especifico = CoL_sn.species) SET Tabla_trabajo.es_aceptadoCoL = is_accepted_name, Tabla_trabajo.id_registro_CoL = col_sn.id, Tabla_trabajo.id_familia_CoL = family_id, Tabla_trabajo.id_nombre_aceptado = accepted_species_id WHERE Tabla_trabajo.es_aceptadoCoL is null AND CoL_sn.infraspecies=''\n")
    dbSendQuery(con, "UPDATE Tabla_trabajo INNER JOIN CoL_sn_aceptados ON Tabla_trabajo.id_nombre_aceptado = CoL_sn_aceptados.id SET tabla_trabajo.id_familia_col= col_sn_aceptados.family_id, Tabla_trabajo.Autor_nombre_aceptado = CoL_sn_aceptados.author, Tabla_trabajo.genero_aceptado = CoL_sn_aceptados.genus, Tabla_trabajo.epiteto_aceptado = CoL_sn_aceptados.species where tabla_trabajo.es_aceptadocol = 2")
    dbSendQuery(con, "UPDATE Tabla_trabajo INNER JOIN CoL_familiesT ON Tabla_trabajo.id_familia_CoL = CoL_familiesT.id SET Tabla_trabajo.nombre_aceptado = concat_ws ('',genero_aceptado, epiteto_aceptado) , Tabla_trabajo.familia_CoL = col_familiest.family, Tabla_trabajo.orden_CoL = col_familiest.`order`, Tabla_trabajo.clase_CoL = col_familiest.class, Tabla_trabajo.phylum_CoL = col_familiest.phylum, Tabla_trabajo.reino_CoL = col_familiest.kingdom where tabla_trabajo.es_aceptadocol= 2 or tabla_trabajo.es_aceptadocol =1")
    outTable = dbGetQuery(con, "select * from tabla_trabajo")
    return(outTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
