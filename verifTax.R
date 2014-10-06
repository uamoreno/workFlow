nameValidation<-function(con,inTable){
  dbSendQuery(con,"truncate tabla_trabajo")
  dbWriteTable(con,"gbif_sib", inTable,overwrite=TRUE)
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"NA\"")
  dbSendQuery(con,"update gbif_sib set epiteto_especifico = null where epiteto_especifico =\"\"")
  dbSendQuery(con,"update gbif_sib set genero = null where genero =\"\"")
  dbSendQuery(con,"delete from gbif_sib where id = \"\"")
  dbSendQuery(con,"INSERT INTO tabla_trabajo (id,nombre,genero,epiteto_especifico) select id,nombre,genero,epiteto_especifico from gbif_sib")
  dbSendQuery(con,"UPDATE Tabla_trabajo INNER JOIN CoL_sn_aceptados ON (Tabla_trabajo.genero = CoL_sn_aceptados.genus) AND (Tabla_trabajo.epiteto_especifico = CoL_sn_aceptados.species) SET Tabla_trabajo.autor_nombre_aceptado= col_sn_aceptados.author, Tabla_trabajo.es_aceptadoCoL = is_accepted_name, Tabla_trabajo.id_registro_CoL = col_sn_aceptados.id, Tabla_trabajo.id_familia_CoL = family_id, Tabla_trabajo.id_nombre_aceptado = accepted_species_id, Tabla_trabajo.genero_aceptado = genus, Tabla_trabajo.epiteto_aceptado = species WHERE (((CoL_sn_aceptados.infraspecies)=''))
")
  dbSendQuery(con,"UPDATE Tabla_trabajo INNER JOIN CoL_sn ON (Tabla_trabajo.genero = CoL_sn.genus) AND (Tabla_trabajo.epiteto_especifico = CoL_sn.species) SET Tabla_trabajo.es_aceptadoCoL = is_accepted_name, Tabla_trabajo.id_registro_CoL = col_sn.id, Tabla_trabajo.id_familia_CoL = family_id, Tabla_trabajo.id_nombre_aceptado = accepted_species_id WHERE Tabla_trabajo.es_aceptadoCoL is null AND CoL_sn.infraspecies=''
")
  dbSendQuery(con,"UPDATE Tabla_trabajo INNER JOIN CoL_sn_aceptados ON Tabla_trabajo.id_nombre_aceptado = CoL_sn_aceptados.id SET tabla_trabajo.id_familia_col= col_sn_aceptados.family_id, Tabla_trabajo.Autor_nombre_aceptado = CoL_sn_aceptados.author, Tabla_trabajo.genero_aceptado = CoL_sn_aceptados.genus, Tabla_trabajo.epiteto_aceptado = CoL_sn_aceptados.species where tabla_trabajo.es_aceptadocol = 2")
  dbSendQuery(con,"UPDATE Tabla_trabajo INNER JOIN CoL_familiesT ON Tabla_trabajo.id_familia_CoL = CoL_familiesT.id SET Tabla_trabajo.nombre_aceptado = concat_ws ('',genero_aceptado, epiteto_aceptado) , Tabla_trabajo.familia_CoL = col_familiest.family, Tabla_trabajo.orden_CoL = col_familiest.`order`, Tabla_trabajo.clase_CoL = col_familiest.class, Tabla_trabajo.phylum_CoL = col_familiest.phylum, Tabla_trabajo.reino_CoL = col_familiest.kingdom where tabla_trabajo.es_aceptadocol= 2 or tabla_trabajo.es_aceptadocol =1")
  outTable=dbGetQuery(con, "select * from tabla_trabajo")
  return(outTable)
}

verifTax <- function(root, set0Path = "set0/", outPath = "set1/", .user = "root", 
                     .password = "root", .dbname = "col2012ac", .host = "localhost",
                     registros  = NULL){
    
  ##  Variables interactivas:
  # registros <- "tabla"
  load(paste0(root, "/", set0Path, "/set0.RData"))
  
  solo_nombres <- as.data.frame(registros$species)
  
  cofTable <- t(apply(solo_nombres, 1, FUN = function(y){
    (pos <- regexpr(" ", y)[[1]][1])
    (genus <- substr(y, 0, pos-1))
    (sp <- substr(y, pos+1, nchar(y)))
    (file <-cbind(y, genus, sp))
    return(t(file))
  }))
  
  id <- 1:nrow(cofTable)
  cofTable <- as.data.frame(cbind(id, cofTable))
  
  nombres <- c("id","nombre", "genero","epiteto_especifico")
  colnames(cofTable) <- nombres
  rownames(cofTable) <- c(1:nrow(cofTable))
  

  library(RMySQL)
  con <- dbConnect(dbDriver("MySQL"), user = .user, password = .password, dbname = .dbname, host = .host) #Cambie este comando ingresando el nombre de usuario y password asociado con su instalaci?n de MySQL
  cofTable2 <- nameValidation(con,cofTable)
  
  especie_aceptada <- paste0(cofTable2$genero_aceptado, " ", cofTable2$epiteto_aceptado)
  cofTable2 <-cbind(cofTable2,especie_aceptada) 
  
  datos <- registros
  datos$ID <- 1:nrow(datos)
  cofTable3 <- merge(datos, cofTable2, by.x = "ID", by.y = "id")
  registros <- cofTable3
   
  ## Guardar archivo en la nube y hacer descargable
  save(registros, file = paste0(root, "/", outPath, "/set1.RData"))
}