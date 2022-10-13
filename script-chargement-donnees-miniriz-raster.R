
#### Script de mise en forme des donnees de Miniriz a l'etape de profils XY

# L'objectif est d'importer les donnees .csv et d'obtenir des Rasters : 
#   - de la RLD en fonction de la profondeur
#   - des notes en foncion de la profondeur

path <- list.files(path = "bdd-essais-miniriz-data/vlb-2022/")
# Localisation des .csv

for (i in 1:length(path)) {

  assign(x = path[i], value = read.csv2(paste0("bdd-essais-miniriz-data/vlb-2022/", path[i])))
  #Je charge mes .csv
    
}

test <- mget(setdiff(ls(), c("i", "path")))
# J'en fait un unique objet
rm(list = setdiff(ls(), c("i", "path", "test")))
# Je me separe de ce que je n'utilise pas

for (i in 1:length(test)) {

  temp <- test[[i]][,3]
  
  temp <- gsub(x = temp, pattern = "\\[|\\]", replacement = "")
  # Je retire les [] dans les chaines de caracteres 
  test[[i]][,3] <- temp
  
  temp <- data.frame(do.call("rbind", strsplit(as.character(test[[i]][,3]), ",", fixed = TRUE)))
  # Je separe les diferentes mesures avec une colonne chacune
  test[[i]] <- cbind(test[[i]][,1:2], temp)
      
}

rm(list = c("i", "temp"))
# Je mets en forme les donnees issues de .csv pour la rasterization

raster.stack <- terra::rast()

for (i in 1:length(test)) {

  temp <- test[[i]]
  temp$profondeur_.mm. <- NULL
  temp$X <- NULL
  # Je retire les cases inutiles presentes dans le template de base servant de legendes et je ne garde que les donnees pures
  
  for (j in 1:length(temp)) {
    
    temp[,j] <- as.numeric(temp[,j])
    
  }
  # Je convertis le tout en classe numeric
  
  raster.temp <- rast(x = as.matrix(temp))
  # Je cree le Raster
  
  names(raster.temp) <- path[i]
  # Je renome la couche du raster
  
  raster.stack <- c(raster.stack, raster.temp)
  # Je combine les raster pour creer un stack
  
}

rm(list = setdiff(ls(),"raster.stack"))

#raster.stack
plot(raster.stack)

terra::writeRaster(filename = "JDD-raster-donnees-miniriz.tif", x = raster.stack, overwrite = T)


