
#### Script de mise en forme des données de Miniriz à l'étape de profils XY

# L'objectif est d'importer les données .csv et d'obtenir des Rasters : 
#   - de la RLD en fonction de la profondeur
#   - des notes en foncion de la profondeur


path <- list.files(path = "bdd-essais-miniriz-data/vlb-2022/")

for (i in 1:length(path)) {

  assign(x = path[i], value = read.csv2(paste0("bdd-essais-miniriz-data/vlb-2022/", path[i])))
    
}

test <- mget(setdiff(ls(), c("i", "path")))
rm(list = setdiff(ls(), c("i", "path", "test")))

for (i in 1:length(test)) {

  temp <- test[[i]][,3]
  
  temp <- gsub(x = temp, pattern = "\\[|\\]", replacement = "")
  
  test[[i]][,3] <- temp
  
  temp <- data.frame(do.call("rbind", strsplit(as.character(test[[i]][,3]), ",", fixed = TRUE)))

  test[[i]] <- cbind(test[[i]][,1:2], temp)
      
}

rm(list = c("i", "temp"))





raster.stack <- terra::rast()

for (i in 1:length(test)) {
#for (i in 1:2) {

  temp <- test[[i]]
  temp$profondeur_.mm. <- NULL
  temp$X <- NULL
  
  for (j in 1:length(temp)) {
    
    temp[,j] <- as.numeric(temp[,j])
    
  }
  
  raster.temp <- rast(x = as.matrix(temp))
  
  names(raster.temp) <- path[i]
 
#  print(ext(raster.temp))
   
  raster.stack <- c(raster.stack, raster.temp)

#  plot(temp)

}

raster.stack

plot(raster.stack)

















































